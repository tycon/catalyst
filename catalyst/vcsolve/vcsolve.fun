functor VCSolve (S : VC_SOLVE_STRUCTS) : VC_SOLVE =
struct
  open S
  open VC
  structure TyD = TypeDesc
  structure TyDB = TyDBinds
  structure RI = RelLang.RelId
  structure RelTy = RelLang.RelType
  structure P = Predicate
  structure BP = Predicate.BasePredicate
  structure RP = Predicate.RelPredicate
  structure L = Layout
  structure C = Control

  datatype result = Success | Undef | Failure
  structure Z3_Encode = Z3_Encode (structure Z3_FFI = Z3_FFI
                                   val z3_log = z3_log)

  exception TyDNotFound
  exception ConstNotFound
  exception RelNotFound
  exception CantSolve

  fun $ (f,arg) = f arg
  infixr 5 $
  val assert = Control.assert
  val empty_set = RelLang.emptyexpr ()
 
  (*
  fun oneOf l = 
    let
      fun getNextGoodX [] k = raise CantSolve
        | getNextGoodX (xs as x::xs') k = MLton.Cont.throw (k,xs)
            handle CantSolve => (print " ** oneOf **\n"; 
              getNextGoodX xs' k)
    in
      MLton.Cont.callcc (getNextGoodX l)
    end
  *)

  (*
   * Caution: f should be side-effect free.
   *)
  fun applyWithoneOf ([],f) = raise CantSolve
    | applyWithoneOf (xs as x::xs', f) = f xs 
        handle CantSolve => applyWithoneOf (xs',f)

  fun varStrEq (v1,v2) = (Var.toString v1 = Var.toString v2)
  fun relStrEq (rid1,rid2) = RI.toString rid1 = RI.toString rid2
  fun rappEq (RelLang.R (rid1,vid1), RelLang.R (rid2,vid2)) =
    RI.toString rid1 = RI.toString rid2 andalso
    Var.toString vid1 = Var.toString vid2
  fun sanitizeRE re = 
    let
      open RelLang
      val len = Vector.length
    in
      case re of 
        U (T el, re') => if len el = 0 then sanitizeRE re' 
            else U (T el, sanitizeRE re')
      | U (re', T el) => sanitizeRE $ U (T el, re')
      | U (re',re'') => U (sanitizeRE re', sanitizeRE re'')
      | ratom => ratom
    end

  structure  HoleMap: APPLICATIVE_MAP where
    type Key.t = string and type Value.t = RP.t vector =
  struct
    structure Key = 
    struct
      type t = string
      val equal = op=
      val layout = L.str 
    end
    structure Value =
    struct
      type t = RP.t vector
      val layout = Vector.layout (fn rp => L.str $ RP.toString rp) 
    end
    structure Map = ApplicativeMap (structure Key = Key
                                   structure Value = Value)
    open Map
  end

  structure  AlphaVCMap: APPLICATIVE_MAP where
    type Key.t = int and type Value.t = VC.t list =
  struct
    structure Key = 
    struct
      type t = int
      val equal = op=
      val layout = L.str o Int.toString
    end
    structure Value =
    struct
      type t = VC.t list
      val layout = VC.layout o Vector.fromList
    end
    structure Map = ApplicativeMap (structure Key = Key
                                   structure Value = Value)
    open Map
  end

  structure AVCM = AlphaVCMap

  fun splitAndGroupVCs vcs = 
    let
      val vcs' = Vector.concatV $ Vector.map (vcs,
        fn (VC.T (tydbinds,anteP,VC.Conj conseqPs)) => 
          Vector.map (conseqPs, fn conseqP => 
              VC.T (tydbinds, anteP, conseqP))
          | vc => Vector.new1 vc)
      val avcMap = Vector.foldr (vcs', AVCM.empty, 
        fn (vc as VC.T (tydbinds, anteP, Simple 
              (Rel (RP.Eq (_, RelLang.Alpha {id, ...})))), avcm) => 
                let
                  val avcs = AVCM.find avcm id 
                    handle AVCM.KeyNotFound _ => []
                  val avcs' = vc::avcs
                in
                  AVCM.add (AVCM.remove avcm id) id avcs'
                end
          | (VC.T (_,_,conseqP),_) => Error.bug $ 
              "Unexpected structure of conseqP:\n" ^
              (L.toString $ VC.layout $ Vector.new1 $ 
                    VC.T (Vector.new0 (), Simple True, conseqP)))
    in
      avcMap
    end

  datatype constraint = C of {tydbinds: (Var.t * TyD.t) vector, 
                              anteEqs: RP.t vector,
                              conseqEq: RP.t}

  fun layoutC (C {anteEqs, conseqEq, ...}) = 
    let
      val inpEqsLyt = L.align $ Vector.toListMap(anteEqs, 
        L.str o RP.toString) 
      val opEqLyt = L.str $ RP.toString conseqEq
      val lyt = L.align[inpEqsLyt, 
                        L.str "---------------------------",
                        opEqLyt]
    in
      lyt
    end

  exception Return
  fun eliminateAlphaFromAnte (cs as C {tydbinds=tyDB, anteEqs, conseqEq}) =
    let
      val (anteEqOps, substOp) = Vector.mapAndFold(anteEqs, NONE,
        fn (anteEq as RP.Eq (rapp,e), NONE) => if RelLang.exprHasAlpha e 
            then (NONE, SOME (e,rapp)) 
            else (SOME anteEq,NONE)
          | (anteEq,someSubst) => (SOME anteEq, someSubst))
      val subst as (e,rapp) = case substOp of SOME s => s
        | NONE => raise Return
      val anteEqs' = Vector.keepAllSome anteEqOps
      fun doSubstInEq (RP.Eq (e1,e2)) = RP.Eq (e1,
        RelLang.mapRApp e2 (fn rapp' => if rappEq (rapp,rapp') 
          then e else rapp'))
      val newAnteEqs = Vector.map (anteEqs', doSubstInEq)
      val newConseqEq = doSubstInEq conseqEq
    in
      eliminateAlphaFromAnte $ C {tydbinds=tyDB, 
                                  anteEqs=newAnteEqs, 
                                  conseqEq=newConseqEq}
    end handle Return => cs

  fun applyHypothesis hyp (re : RelLang.expr) : RelLang.expr =
    let
      open RelLang
      (*val _ = print $ "applyHypothesis: hyp "^(exprToString hyp)
        ^" in "^(exprToString re)^"\n"*)
      fun doSubstsInHyp substs = applySubsts 
          (Vector.fromList substs) hyp
      fun doItRatom ratom = case ratom of
          Alpha {substs, ...} => U (doSubstsInHyp substs,
            ratom)
        | X (s,ratom') => raise (Fail "applyHyp: CrossPrd Unimpl.")
        | U _ => Error.bug "applyHypothesis: Expected RAtom. \
                            \Got RExpr."
        | D _ => Error.bug "applyHypothesis: Expected RAtom. \
                            \Got RExpr."
        | _ => ratom
    in
      case re of
        U (ratom, re') => 
            U (applyHypothesis hyp ratom, applyHypothesis hyp re')
      | ratom => doItRatom ratom
    end  


  fun solveCS cs (dom : (RelLang.expr*RelTy.t) list) 
        : RelLang.expr = 
    let
      val _ = Z3_Encode.logComment " --- New Constraint ---"
      val ctx = Z3_Encode.mkDefaultContext ()
      (*
       * APIs for the current context.
       *)
      val api = Z3_Encode.generateAPI ctx
      val bool_sort = #bool_sort api
      val int_sort = #int_sort api
      val const_true = #const_true api
      val const_false = #const_false api
      val truee = #truee api
      val falsee = #falsee api
      val mkUninterpretedSort = #mkUninterpretedSort api
      val mkConst = #mkConst api
      val mkInt = #mkInt api
      val mkStrucRel = #mkStrucRel api
      val mkStrucRelApp = #mkStrucRelApp api
      val mkNullSet = #mkNullSet api
      val mkSingletonSet = #mkSingletonSet api
      val mkUnion = #mkUnion api
      val mkIntersection = #mkIntersection api
      val mkCrossPrd = #mkCrossPrd api
      val mkDiff = #mkDiff api
      val mkSetEqAssertion = #mkSetEqAssertion api
      val mkSubSetAssertion = #mkSubSetAssertion api
      val mkConstEqAssertion = #mkConstEqAssertion api
      val mkNot = #mkNot api
      val mkIf = #mkIf api
      val mkIff = #mkIff api
      val mkAnd = #mkAnd api
      val mkOr = #mkOr api
      val doPush = #doPush api
      val doPop = #doPop api
      val dischargeAssertion = #dischargeAssertion api
      fun strEq (str1,str2) = (str1 = str2)
      val tyMap = HashTable.mkTable (MLton.hash, TyD.sameType) 
        (117, TyDNotFound)
      val intTyD = TyD.makeTconstr (Tycon.intInf,[])
      val boolTyD = TyD.makeTconstr (Tycon.bool,[])
      val _ = HashTable.insert tyMap (intTyD,int_sort)
      val _ = HashTable.insert tyMap (boolTyD,bool_sort)
      fun addTyD tyd = (fn sort => 
          (HashTable.insert tyMap (tyd,sort); sort))
        (mkUninterpretedSort ())
      val constMap = HashTable.mkTable (MLton.hash, strEq) 
        (117, ConstNotFound)
      val relMap = HashTable.mkTable (MLton.hash, strEq)
        (117, RelNotFound)
      fun getConstForVar v = (fn vstr => HashTable.lookup constMap vstr
        handle ConstNotFound => Error.bug ("Variable "^vstr^" undec\
          \lared despite processing tydbinds")) (Var.toString v)
      fun getStrucRelForRelId rid = (fn rstr => HashTable.lookup relMap
        rstr handle RelNotFound => Error.bug ("Rel "^rstr^" undec\
          \lared despite processing tydbinds")) (RI.toString rid)
      (*
       * Encoding functions
       * encodeConst and encodeStrucRel rely on uniqueness of 
       * bindings in tydbinds. In case of duplicate bindings,
       * duplication declarations show up in Z3 VC, but most
       * recent binding is used.
       *)
      fun encodeTyD tyD = case HashTable.find tyMap tyD of
          SOME sort => sort
        | NONE => (case tyD of TyD.Tvar _ =>  addTyD tyD
            | TyD.Tconstr _ => addTyD tyD
            | _ => Error.bug "Unexpected type")

      fun encodeConst (v:Var.t,tyd:TyD.t) = 
        let
          val vstr = Var.toString v
          val sort = encodeTyD tyd 
          val const = mkConst (vstr,sort)
          val _ = HashTable.insert constMap (vstr,const)
        in
          const
        end

      fun encodeStrucRel (rid : RI.t, TyD.Tarrow (t1,_)) =
        let
          val rstr = RI.toString rid
          val sorts = case t1 of 
              TyD.Trecord tydr => Vector.map (#2 $ Vector.unzip $ 
                Record.toVector tydr, encodeTyD)
            | _ => Vector.new1 $ encodeTyD t1
          val sr = mkStrucRel (rstr,sorts)
          val _ = HashTable.insert relMap (rstr,sr)
        in
          sr
        end

      fun processTyDBind (v:Var.t,tyd:TyD.t) = case tyd of 
        (*
         * Currently, the only values with function types
         * are structural relations encoded as functions from
         * a val or tuple of vals to bool.
         *)
          TyD.Tarrow (t1,TyD.Tconstr (tycon,_)) => 
            if Tycon.isBool tycon then ignore $ encodeStrucRel 
              (RI.fromString $ Var.toString v, tyd)
            else ignore $ encodeConst (v,tyd)
        | _ => ignore $ encodeConst (v,tyd)

      fun encodeBasePred (bp:BP.t) : Z3_Encode.assertion = 
        let
          open BP
          val encodeBaseExpr = fn (Int i) => mkInt i
            | Bool true => const_true | Bool false => const_false
            | Var v => getConstForVar v
        in
          case bp of Eq (e1,e2) => mkConstEqAssertion 
              (encodeBaseExpr e1, encodeBaseExpr e2)
            | Iff (bp1,bp2) => mkIff (encodeBasePred bp1,
                encodeBasePred bp2)
        end

      fun encodeRelPred (rp:RP.t) : Z3_Encode.assertion =
        let
          open RelLang
          val encodeRelElem = fn (Int i) => mkInt i 
            | Bool true => const_true | Bool false => const_false
            | Var v => getConstForVar v
          fun encodeRelExpr (e:expr) : Z3_Encode.set =
            case e of T els => (case Vector.length els of
                0 => mkNullSet ()
              | _ => mkSingletonSet $ Vector.map (els,
                  encodeRelElem))
            | X (e1,e2) => mkCrossPrd (encodeRelExpr e1, 
                encodeRelExpr e2)
            | Xn (e1,e2) => mkIntersection (encodeRelExpr e1, 
                encodeRelExpr e2)
            | U (e1,e2) => mkUnion (encodeRelExpr e1, 
                encodeRelExpr e2)
            | D (e1,e2) => mkDiff (encodeRelExpr e1, 
                encodeRelExpr e2)
            | R (rid,v) => mkStrucRelApp (getStrucRelForRelId rid,
                getConstForVar v)
            | Alpha _ => Error.bug "Antecedent is not alpha-free!\n"
          val f = encodeRelExpr
          open RP
        in
          case rp of Eq (e1,e2) => mkSetEqAssertion (f e1, f e2)
          | Sub (e1,e2) => mkSubSetAssertion (f e1, f e2)
          | SubEq (e1,e2) => (fn s => mkOr $ Vector.new2 (mkSetEqAssertion s,
              mkSubSetAssertion s)) (f e1, f e2)
        end

      fun encodePred (p:P.t) : Z3_Encode.assertion = 
        let
          open P
        in
          case p of
            Base bp => encodeBasePred bp
          | Rel rp => encodeRelPred rp
          | Not p' => mkNot $ encodePred p'
          | _ => raise (Fail "Unimpl.")
        end

      val assertRelPred  = dischargeAssertion o encodeRelPred

      fun isValid ctx (* assumps *) p = 
        let
          val comment = "Checking if ("^(L.toString $ P.layout p)
              ^") is valid... "
          val _ = print comment
          val _ = doPush ()
          val _ = Z3_Encode.logComment comment
          (* val _ = List.foreach (assumps, dischargeAssertion o
              encodePred) *)
          val _ = dischargeAssertion $ mkNot $ encodePred p
          val res = Z3_Encode.checkContext ctx
          val _ = doPop ()
        in
          case res of Z3_Encode.UNSAT => (print "Yes\n"; true) 
            | Z3_Encode.SAT => (print "No\n"; false)
            | _ => raise (Fail "Solver timed out!")
        end

      fun isInvalid ctx p = not $ isValid ctx p

      fun semanticSolve ctx hypAcc 
                        (conseqEq as 
                          RP.Eq (alpha as RelLang.Alpha {substs, ...},
                                 opEqRHS))
                        dom =
        let
          val _ = print "Domain is:\n"
          val _ = List.foreach (dom, fn rapp => 
            print $ RelLang.exprToString rapp)
          val _ = print "\n"
          val substs = Vector.fromList substs
          (*
           * If hypAcc is a valid solution, then conseqEq is
           * satisfied. Hence, the following is valid:
           *        ctx ⊢ substs(hypAcc) = opEqRHS. 
           *)
          val hypAcc' = RelLang.applySubsts substs hypAcc
          val r = sanitizeRE $ RelLang.mapAlpha opEqRHS
            (fn (RelLang.Alpha _) => RelLang.emptyexpr ())
          val p = P.Rel $ RP.Eq (hypAcc',r)
          val _ = if isValid ctx p then raise Return else ()
          val _ = if List.isEmpty dom then raise CantSolve else ()
          exception Return of bool
          val {no=notHyps, yes=hyps} = List.partition (dom, 
            fn d => 
              let
                (* As with hypAcc, replace freevars *)
                val d' = RelLang.applySubsts substs d
                val hyp_eq_empty = P.Rel $ RP.Eq (d',empty_set)
                val _ = if isValid ctx hyp_eq_empty 
                  then raise (Return false) else ()
                val r_hypAcc = RelLang.D (r,hypAcc')
                val r_hypAccUhyp = RelLang.D (r, RelLang.U (hypAcc',d'))
                val subAssn1 = P.Rel $ RP.SubEq (r_hypAccUhyp, r_hypAcc)
                val _ = if isValid ctx subAssn1 
                    then raise (Return true)
                    else raise (Return false)
                (*
                val preCond = (P.Not o P.Rel) $ RP.Eq
                  (r_hypAcc,empty_set)
                *)
                (*
                 * Note: r is not ∅, for if it is ∅, then we
                 * wouldn't have gotten this far; semanticSolve
                 * would've terminated immediately with {()} as answer.
                 * hypAcc could be ∅ if:
                 * 1. This is the 1st iteration, and hypAcc is {()}, or
                 * 2. hypAcc is not {()}, but it is semantically
                 * equal to a ∅.
                 * Case 2 is impossible, because it violates the
                 * invariant that (hypAcc ∩ r ≠ ∅) except when
                 * hypAcc={()}. So, we only have to check for case 1.
                 *)
                val r_eq_hyp = RP.Eq (r,d')
                val r_eq_hyp_assn = P.Rel r_eq_hyp
                val r_xn_hyp = RelLang.Xn (r,d')
                val r_xn_hyp_assn = (P.Not o P.Rel) $ RP.Eq 
                            (r_xn_hyp,empty_set)
                val r_eqorxn_hyp_assn = P.Disj (r_eq_hyp_assn,
                                                r_xn_hyp_assn)
                val _ = if isValid ctx r_xn_hyp_assn then () 
                          else raise (Return false)
                val _ = if RelLang.isEmptyExpr hypAcc' then 
                          raise (Return true) else ()
                val r_xn_hypAcc = RelLang.Xn (r,hypAcc')
                val r_xn_hypAcc_assn = P.Rel $ RP.Eq 
                            (r_xn_hyp,r_xn_hypAcc)
              in
                (*isValid ctx [preCond]  assn*)
                isInvalid ctx r_xn_hypAcc_assn
              end handle Return tf => tf)
          val sol = applyWithoneOf (hyps, 
            fn (hyp::restHyps) => 
              let
                val _ = print $ "\t Current hypothesis: "
                    ^(RelLang.exprToString hyp)^"\n"
                val newDom = List.concat [restHyps,notHyps]
                val newOpEqRHS = applyHypothesis hyp opEqRHS
                val newConseqEq = RP.Eq (alpha,newOpEqRHS)
                val newHypAcc = RelLang.U (hypAcc,hyp)
              in
                semanticSolve ctx newHypAcc newConseqEq newDom
              end)
        in
          sol
        end handle Return => hypAcc 
      
      (* -- Constraint encoding -- *)
      val C {tydbinds, anteEqs, conseqEq} = cs
      val RP.Eq (RelLang.Alpha {sort=cspSort,...},_) = conseqEq
      val _ = Vector.foreach (tydbinds, processTyDBind)
      val _ = Vector.foreach (anteEqs, assertRelPred)
      fun domOfSort srt = List.keepAllMap (dom, 
        fn (rapp,srt') => if RelTy.equal (srt,srt') 
                            then SOME rapp else NONE)
      val initHyp = RelLang.emptyexpr ()
      (*
       * Hack: we are only looking to infer union invaraints.
       * We restrict our domain to ratoms of cspSort.
       *)
      val sol = semanticSolve ctx initHyp conseqEq (domOfSort cspSort)
      val _ = print "semanticSolve done. Solution returned is:\n"
      val _ = print $ RelLang.exprToString sol
      val _ = print "\n"
    in
      sol
    end

  (*
   * Solves a given VC for the alpha contained in its conseqP.
   *)
  fun solveThisForAlpha (VC.T (tydbinds, anteP, conseqP)) 
                        (alphaId:int) =
    let
      (*
       * Some pre-processing.
       * 1. We need a way to get relations by domain type.
       * 2. We need TyDB.
       *)
      exception NoRelsOnTyD
      val (relTab: (TyD.t, (RI.t*RelTy.t) list) HashTable.hash_table) = 
          HashTable.mkTable (MLton.hash o TyD.toString, TyD.sameType) 
            (67, NoRelsOnTyD)
      val tyDB = Vector.fold (tydbinds, TyDB.empty,
        fn ((relvar,TyD.Tarrow (TyD.Trecord trec,_)), tyDB) => 
          let
            val relId = RI.fromString $ Var.toString relvar
            val snd = fn (x,y) => y
            val domTyd::rngTyds = Vector.toListMap 
                (Record.toVector trec, snd)
            val rngSort = RelTy.Tuple $ Vector.fromList rngTyds
            val others = HashTable.lookup relTab domTyd 
              handle NoRelsOnTyD => []
            val _ = HashTable.insert relTab (domTyd, 
                    (relId,rngSort)::others)
          in
            tyDB
          end
          | ((v,tyd),tyDB) => TyDB.add tyDB v tyd)
      fun relsWithDomTyd tyd = HashTable.lookup relTab tyd 
        handle NoRelsOnTyD => []
      (*
       * First step is to extract RPEqs from anteP
       *)
      datatype eq1 = VarEq of Var.t * Var.t 
                   | RelEq of RelLang.expr * RelLang.expr
      val eqs = case anteP of 
        VC.Conj vcps => Vector.toListMap (vcps, 
          fn (Simple (Base (BP.Eq (BP.Var lv, BP.Var rv)))) =>
                VarEq (lv,rv)
            | (Simple (Rel (RP.Eq (e1,e2)))) => RelEq (e1,e2)
            | _ => Error.bug "Unexpected conjunct in anteP")
      | _ => raise (Fail "solveThisForAlpha: Unimpl")
      (*
       * conseqP must be an equation of form:
       *   cspRID(cspBV) = cspAlpha {...}
       *)
      val cspEqn = case conseqP of Simple (Rel eq) => eq
        | _ => raise (Fail "Impossible case")
      val (cspRApp, 
           cspBV, 
           cspAlpha,
           holeBV,
           holeEnv) 
        = case cspEqn of 
              RP.Eq (rapp as RelLang.R (_,v),
                     alpha as RelLang.Alpha {substs,sort,env,bv,...}) 
                => 
                  (rapp, v, alpha, bv,env)
      (*
       * Domain RApps of the solution is the set of all possible relational
       * abstractions of input vars.
       *)
      val domRApps = List.concat $ List.map (TyDB.toList holeEnv, 
        fn (v,tyd) => if varStrEq (v,holeBV) then [] 
            else List.map (relsWithDomTyd tyd, 
                  fn (relId, relTy) => (RelLang.R (relId,v), relTy)))
      (*
       * Next, eliminate variable equalities.
       * The process is straightforward, except when we encounter a
       * varEq (v1,v2) where v1 = cspBV. For such eqn, we replace it
       * with R(v1) = R(v2) for every suitable R. This is for
       * technical reason.
       *)
      val opEqnsGend = ref false
      fun elimVarEqs eqs1 [] = eqs1
        | elimVarEqs eqs1 ((VarEq (v1,v2))::eqs2) = 
            if varStrEq (v1,cspBV) andalso
               not (!opEqnsGend) then
              let
                val rels = #1 $ List.unzip $ relsWithDomTyd 
                  (TyDB.find tyDB v1)
                val newRPs = List.map (rels, fn rel => 
                  RP.Eq (RelLang.app (rel,v1),
                         RelLang.app (rel,v2)))
                val _ = opEqnsGend := true
              in
               elimVarEqs (List.concat [newRPs, eqs1]) eqs2
              end
            else if not $ varStrEq (v1,cspBV) then
              let
                val substs = Vector.new1 (v2,v1)
                val doSubst = RelLang.applySubsts substs
                val doSubstInEq1  = RP.applySubsts substs
                fun doSubstInEq2 (VarEq (v3,v4)) = 
                       VarEq (v3, if varStrEq (v4,v1) then v2 else v4)
                  | doSubstInEq2 (RelEq (e1,e2)) =
                       RelEq (doSubst e1, doSubst e2)
                fun doSubstInEqs1 eqs1 = List.map (eqs1, doSubstInEq1)
                fun doSubstInEqs2 eqs2 = List.map (eqs2, doSubstInEq2)
              in
                elimVarEqs (doSubstInEqs1 eqs1) (doSubstInEqs2 eqs2)
              end
            else 
              elimVarEqs eqs1 eqs2
        | elimVarEqs eqs1 ((RelEq x)::eqs2) = elimVarEqs 
              ((RP.Eq x)::eqs1) eqs2
      val (allEqs : RP.t list) = elimVarEqs [] eqs
      (*
       * Output eqn is the equation whose LHS Rapp is same as that of
       * cspEqn'. Lets extract the rapp from cspEqn'.
       *)
      val {no=anteEqs, yes=opEqs} = List.partition (allEqs,
        fn (RP.Eq (rapp',_)) => rappEq (cspRApp,rapp')
         | _ => false)
      val opEq = case opEqs of [opEq] => opEq
        | _ => raise (Fail "anteP is expected to contain exactly one\
          \ output equation.\n")
      (*
       * conseqEq is an eqn with cspRHS (an alpha) as LHS and opEqRHS 
       * as RHS.
       *)
      val conseqEq = case opEq of RP.Eq (_,opEqRHS) => 
            RP.Eq (cspAlpha,opEqRHS)
      (*
       * This is the constraint system to solve
       *)
      val cs = eliminateAlphaFromAnte $ 
                    C {tydbinds=tydbinds, 
                       anteEqs= Vector.fromList anteEqs,
                       conseqEq=conseqEq }
      val _ = print "\n\n"
      val _ = L.print (layoutC cs, print)
      val _ = print "\n"
      val sol = solveCS cs domRApps
    in
      ()
    end

  (*
   * Pre-condition: vcs have same single alpha in their conseqP.
   * Solves VCs and generates solution for that alpha.
   *)
  fun solveAllForAlpha (vcs : VC.t list) (alphaId:int) = 
    let
      val sols = List.map (vcs, fn vc => solveThisForAlpha vc alphaId)
    in
      sols
    end

  structure HM = HoleMap 
  fun solve vcs = 
    let
      (*
       * We get elaborated VCs, whose consequent is a conjunction of
       * equations, each with an alpha. We want to solve one alpha at 
       * a time. So, we split VCs (i.e., A => B /\ C becomes (A => B)
       * /\ (A => C)), and group them according to alphas.
       *)
      val avcMap = splitAndGroupVCs vcs
      val _ = C.saveToFile ({suffix = "avcm"}, C.No, avcMap,
                                 C.Layout AVCM.layout)
      (*
       * For each alpha, solve VCs to generate a solution. If an alpha
       * does not have a solution, we simply discard it. Subsequently,
       * it gets dropped from the conjuncts of the hole solution.
       *)
      val (avcs : (int * VC.t list) vector) = AVCM.toVector avcMap
      val asols = Vector.map (avcs,
        fn (alphaId,vcs) => solveAllForAlpha vcs alphaId)
    in
      raise (Fail "solve is Unimpl.")
    end
end

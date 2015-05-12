functor VCSolve (S : VC_SOLVE_STRUCTS) : VC_SOLVE =
struct
  open S
  open VC
  structure TyD = TypeDesc
  structure TyDB = TyDBinds
  structure RI = RelLang.RelId
  structure RelTy = RelLang.RelType
  structure R = RelLang
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
  val len = Vector.length
  val snd = fn (_,y) => y
  val fst = fn (x,_) => x
  fun info str = C.messageStr (C.Detail,str)
 
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
    | rappEq _ = Error.bug "rappEq: one of the arguments is\
      \ not an rapp\n"
  fun sanitizeRE re = 
    let
      open RelLang
    in
      case re of 
        U (T el, re') => if len el = 0 then sanitizeRE re' 
            else U (T el, sanitizeRE re')
      | U (re', T el) => sanitizeRE $ U (T el, re')
      | U (re',re'') => U (sanitizeRE re', sanitizeRE re'')
      | ratom => ratom
    end

  fun allInpCPsOfSort dom (RelTy.Tuple tyds) = 
    let
      open RelLang
      exception NotPrefix
      fun suffix (vec1, vec2, isEq) = 
        let
          val len2 = len vec2
          val _ = if len2 > len vec1 then raise NotPrefix
                    else ()
          val prefix1 = Vector.prefix (vec1, len2)
          val _ = if Vector.forall2 (prefix1,vec2, isEq) then ()
                    else raise NotPrefix
        in
          Vector.dropPrefix (vec1,len2)
        end
    in
      if len tyds = 0 then [] 
      else List.concat $ List.keepAllMap (dom,
          fn (d,RelTy.Tuple tyds') => 
            let
              val restTyDs = suffix (tyds, tyds', TyD.sameType)
              val atoms'= allInpCPsOfSort dom $
                                  RelTy.Tuple restTyDs
              val atoms = case atoms' of [] => [d]
                | _ => List.map (atoms', fn atom' => X (d,atom'))
            in
              SOME $ atoms
            end handle NotPrefix => NONE)
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

  structure  AlphaMap: APPLICATIVE_MAP where
    type Key.t = int and type Value.t = RelLang.expr =
  struct
    structure Key = 
    struct
      type t = int
      val equal = op=
      val layout = L.str o Int.toString
    end
    structure Value =
    struct
      type t = RelLang.expr
      val layout = L.str o RelLang.exprToString
    end
    structure Map = ApplicativeMap (structure Key = Key
                                   structure Value = Value)
    open Map
  end

  structure AM = AlphaMap

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
      (*val _ = info $ "applyHypothesis: hyp "^(exprToString hyp)
        ^" in "^(exprToString re)^"\n"*)
      fun doSubstsInHyp substs = applySubsts 
          (Vector.fromList substs) hyp
      fun doItRatom ratom = case ratom of
          Alpha {substs, ...} => U (doSubstsInHyp substs,
            ratom)
        | X (a1,a2) => X (doItRatom a1, doItRatom a2)
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

  datatype vc_ctx = VCCtx of {z3API: Z3_Encode.api,
                              encodePred: P.t -> Z3_Encode.assertion,
                              encodeRelExpr : R.expr -> Z3_Encode.set}
  datatype alpha_puzzle  = AP of {vcCtx : vc_ctx,
                                  conseqEq : RP.t}

  
  fun isValid (VCCtx {z3API, encodePred, ...}) assumps p = 
    let
      val Z3_Encode.API {doPush, doPop, checkSAT,
          mkNot, dischargeAssertion, ...} = z3API
      val comment = "Checking if ("^(L.toString $ P.layout p)
          ^") is valid... "
      val _ = info comment
      val _ = doPush ()
      val _ = Z3_Encode.logComment comment
      val _ = List.foreach (assumps, dischargeAssertion o
          encodePred)
      val _ = dischargeAssertion $ mkNot $ encodePred p
      val res = checkSAT ()
      val _ = doPop ()
    in
      case res of Z3_Encode.UNSAT => (info "Yes\n"; true) 
        | Z3_Encode.SAT => (info "No\n"; false)
        | _ => raise (Fail "Solver timed out!")
    end

  fun isConsistent (VCCtx {z3API, encodePred, ...}) assns  = 
    let
      val Z3_Encode.API {doPush, doPop, checkSAT,
          mkNot, dischargeAssertion, ...} = z3API
      val comment = "Checking if the context is consistent ..."
      val _ = info comment
      val _ = doPush ()
      val _ = Z3_Encode.logComment comment
      val _ = List.foreach (assns, dischargeAssertion o
          encodePred)
      val res = checkSAT ()
      val _ = doPop ()
    in
      case res of Z3_Encode.UNSAT => (info "No\n"; false) 
        | Z3_Encode.SAT => (info "Yes\n"; true)
        | _ => raise (Fail "Solver timed out!")
    end

  (*
   * Given a vc_ctx and a set (preds) of predicates, return a
   * predicate which is mutually inconsistent with a (possibly empty)
   * subset of the given set (preds).
   * vc_ctx should remain observably invariant.
   *)

  fun semanticSolve n hypAcc (puzzles : alpha_puzzle vector) dom =
    let
      val _ = info "Domain is: "
      val _ = List.foreach (dom, fn rapp => 
        info $ (RelLang.exprToString rapp)^" ")
      val _ = info "\n"
      val x = Vector.peekMapi (puzzles,
        fn (AP {vcCtx,
                conseqEq = RP.Eq (RelLang.Alpha {substs, id, ...},
                                  opEqRHS)}) =>
          let
            (*
             * If hypAcc is a valid solution, then conseqEq is
             * satisfied. Hence, the following is valid:
             *        ctx ⊢ substs(hypAcc) = opEqRHS. 
             *)
            val substs = Vector.fromList substs
            val hypAcc' = RelLang.applySubsts substs hypAcc
            val r = sanitizeRE $ RelLang.mapAlpha opEqRHS
              (fn (RelLang.Alpha _) => RelLang.emptyexpr ())
            val p = P.Rel $ RP.Eq (hypAcc',r)
          in
            if not (isValid vcCtx [] p) 
              then SOME (vcCtx, substs, id, hypAcc', r, p)
              else NONE
          end)
      val (i,(vcCtx, substs, alphaId, hypAcc', r, hypAccIsSol)) = 
          case x of NONE => raise Return | SOME x => x 
      val _ = if List.isEmpty dom then raise CantSolve else ()
      val _ = print $ "Finding hypotheses for alphaId: "
        ^(Int.toString alphaId)^"\n"
      (*
       * Finding minimal unsat cores among propositions of form 
       *    {(r - hypAcc) ∩ d = ∅ | d ∈ dom}
       *)
      val VCCtx (vcAPI as {z3API = Z3_Encode.API api, ...}) = vcCtx
      val encodePred = #encodePred vcAPI
      val encodeRelExpr = #encodeRelExpr vcAPI
      val mkNullSet = #mkNullSet api
      val mkSetEqAssertion = #mkSetEqAssertion api
      val mkNot = #mkNot api
      val doPush = #doPush api
      val doPop = #doPop api
      val dischargeAssertion = #dischargeAssertion api
      val doSubsts = R.applySubsts substs
      val r_subeq_hypAcc = P.Rel $ RP.SubEq (r,hypAcc')
      val _ = if isValid vcCtx [] r_subeq_hypAcc
        (* r C= hypAcc Unimpl. *)
        then raise CantSolve else ()
      (*
       * Find non-empty domain
       *)
      val nonEmptyDom = List.keepAll (dom, 
        fn d =>
          let
            (* We don't consider trivial hypotheses *)
            val d' = doSubsts d
            val d_eq_empty = P.Rel $ RP.Eq (d',empty_set)
          in
            not $ isValid vcCtx [] d_eq_empty
          end)

      val _ = doPush ()
      val nullSet = mkNullSet ()
      val r_hypAcc = encodeRelExpr $ RelLang.D (r,hypAcc')
      val r_hypAcc_non_empty = mkNot $ mkSetEqAssertion 
                                        (r_hypAcc, nullSet)
      val _ = dischargeAssertion r_hypAcc_non_empty
      (* ToDo: val props = {(r - hypAcc) ∩ d = ∅ | d ∈ dom}*)
      val minUnsatCores = findMinUnsatCores props
      val _ = doPop ()
      val idom = List.mapi (dom, fn x => x)
      exception Return of bool
      val {no=notHypsi, yes=hypsi} = List.partition (idom, 
        fn (i,d) => 
          let
            val d' = doSubsts d
            val hyp_eq_empty = P.Rel $ RP.Eq (d',empty_set)
            val r_hypAcc = RelLang.D (r,hypAcc')
            val r_hypAccUhyp = RelLang.D (r, RelLang.U (hypAcc',d'))
            val subAssn = P.Rel $ RP.Sub (r_hypAccUhyp, r_hypAcc)
            (*
             * Since we know hyp is not empty and hypAcc is not a
             * solution, hyp is relevant iff it leads to progress under
             * these conditions.
             * ToDo: it is not enough to assert hyp ≠ ∅. We need to
             * assert that every known subset of hyp is ≠ ∅.
             *)
            val cond = P.Conj (P.Not hyp_eq_empty, P.Not hypAccIsSol)
            val condSubAssn = P.If (cond, subAssn)
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
          in
            isValid vcCtx [] condSubAssn
          end handle Return tf => tf)
      val (hyps, notHyps) = (List.map (hypsi,snd), 
                             List.map (notHypsi,snd))
      val sol = applyWithoneOf (hyps, 
        fn (hyp::restHyps) => 
          let
            val (nstr,istr) =  (Int.toString n,Int.toString i)
            val _ = print $ "\t Current hypothesis (Level "
                ^nstr^", VC#"^istr^"): " 
                ^(RelLang.exprToString hyp)^"\n"
            val newDom = List.concat [restHyps,notHyps]
            (*
             * hyp is a hypothesis for the solution to all puzzles.
             * Hence, it needs to be applied to all puzzles.
             *)
            val newPuzzles = Vector.map (puzzles, 
              fn (AP {vcCtx,
                      conseqEq = RP.Eq (alpha as RelLang.Alpha {substs, ...},
                                   opEqRHS)}) =>
                let
                  val newOpEqRHS = applyHypothesis hyp opEqRHS
                  val newConseqEq = RP.Eq (alpha,newOpEqRHS)
                in
                  AP {vcCtx=vcCtx,
                      conseqEq=newConseqEq}
                end)
            val newHypAcc = RelLang.U (hypAcc,hyp)
          in
            semanticSolve (n+1) newHypAcc newPuzzles newDom
          end)
    in
      sol
    end handle Return => hypAcc 
  
  fun cstrToAlphaPuzzle (cstr:constraint) : alpha_puzzle = 
    let
      val _ = Z3_Encode.logComment " --- New Constraint ---"
      val ctx = Z3_Encode.mkDefaultContext ()
      (*
       * APIs for the current context.
       *)
      val Z3_Encode.API api = Z3_Encode.generateAPI ctx
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

      val encodeRelElem = 
        let
          open RelLang
        in
          fn (Int i) => mkInt i 
            | Bool true => const_true | Bool false => const_false
            | Var v => getConstForVar v
        end 

      fun encodeRelExpr (e:R.expr) : Z3_Encode.set =
        let
          open RelLang
        in
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
        end 
          
      fun encodeRelPred (rp:RP.t) : Z3_Encode.assertion =
        let
          
          val f = encodeRelExpr
          open RP
        in
          case rp of Eq (e1,e2) => mkSetEqAssertion (f e1, f e2)
          | Sub (e1,e2) => (fn s => mkAnd $ Vector.new2 (mkSubSetAssertion s,
              mkNot $ mkSetEqAssertion s)) (f e1, f e2)
          | SubEq (e1,e2) => mkSubSetAssertion (f e1, f e2)
        end

      fun encodePred (p:P.t) : Z3_Encode.assertion = 
        let
          val z3_true = truee
          open P
        in
          case p of
            Base bp => encodeBasePred bp
          | Rel rp => encodeRelPred rp
          | Not p' => mkNot $ encodePred p'
          | If (p1,p2) => mkIf (encodePred p1, encodePred p2)
          | Conj (p1,p2) => mkAnd $ Vector.new2 (encodePred p1, encodePred p2)
          | True => z3_true
          | _ => raise (Fail "Unimpl.")
        end

      val assertRelPred  = dischargeAssertion o encodeRelPred

      (* cstr --> ctx *)
      val C {tydbinds,anteEqs,conseqEq, ...} = cstr
      val _ = Vector.foreach (tydbinds, processTyDBind)
      val _ = Vector.foreach (anteEqs, assertRelPred)
      val vcCtx = VCCtx {z3API=Z3_Encode.API api, 
                         encodePred=encodePred,
                         encodeRelExpr=encodeRelExpr}
    in
      AP {vcCtx=vcCtx, conseqEq=conseqEq} 
    end

  (*
   * Returns constraints imposed by this VC.
   *)
  fun constraintsOfVC (VC.T (tydbinds, anteP, conseqP)) 
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
    in
      (cs,domRApps)
    end

  (*
   * Pre-condition: vcs have same single alpha in their conseqP.
   * Solves VCs and generates solution for that alpha.
   *)
  fun solveAllForAlpha (vcs : VC.t list) (alphaId:int) = 
    let
      val astr = Int.toString alphaId
      val _ = print $ "\n ********* Solving new alpha ("
            ^astr^")******* \n"
      val csAndDoms = List.map (vcs, fn vc => constraintsOfVC vc alphaId)
      val cstrs = List.map (csAndDoms, fn (x,_) => x)
      val puzzles = Vector.fromList $ List.map (cstrs, cstrToAlphaPuzzle)
      (*
       * assert : all conseqEqs have same sort, and same domain.
       *)
      val (C {conseqEq, ...},dom)::_ = csAndDoms
      val RP.Eq (RelLang.Alpha {sort=cspSort,...},_) = conseqEq
      val initHyp = RelLang.emptyexpr ()
      (*
       * Hack: we are only looking to infer union invaraints.
       * We restrict our domain to ratoms of cspSort.
       *)
      (*val domOfCSPSort = List.keepAllMap (dom, 
        fn (rapp,srt) => if RelTy.equal (srt,cspSort) 
                            then SOME rapp else NONE)*)
      val dom' = List.keepAll (dom, 
        fn (RelLang.R (rid,_),_) => 
          let
            val rstr = RI.toString rid
            fun rstrHasPrefix s = String.hasPrefix (rstr,{prefix=s})
          in
            not (rstrHasPrefix "Roa") andalso 
            not (rstrHasPrefix "Rob") (*andalso 
            (case cspSort of RelTy.Tuple tys => 
                if len tys > 1 
                    then not (rstrHasPrefix "Rhd")
                    else true)*)
          end)
      val atoms = allInpCPsOfSort dom' cspSort
      val sol = semanticSolve 0 initHyp puzzles atoms
        handle CantSolve => 
          (print $ "No solution for alpha ("^astr^")\n";
          raise CantSolve)
      val _ = print $ "Solution for alpha ("^astr^") is: "
        ^(RelLang.exprToString sol)^"\n"
    in
      sol
    end

  fun instAlphaInvs am (VC.T (tydbinds,anteP,conseqP)) =
    let
      open VC
      fun doItTup f (x1,x2) = (f x1, f x2)
      fun doItSP sp = case sp of
          Rel (RP.Eq (lhs,RelLang.Alpha {id,substs,...})) => 
            (let
              val solRE = AM.find am id
              val thisRE = RelLang.applySubsts (Vector.fromList substs)
                              solRE
            in
              Rel $ RP.Eq (lhs,thisRE)
            end handle AM.KeyNotFound _ => sp)
        | _ => sp
      fun doItVCP vcp = case vcp of
          Simple sp =>  Simple $ doItSP sp
        | Conj vcps => Conj $ Vector.map (vcps,doItVCP)
        | Disj vcps => Disj $ Vector.map (vcps,doItVCP)
        | Not vcp' => Not $ doItVCP vcp'
        | _ => raise (Fail "If and Iff Unimpl.")
    in
      VC.T (tydbinds, doItVCP anteP, conseqP)
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
      val am = Vector.fold (avcs,AM.empty,
        fn ((alphaId,vcs),am) => 
          let
            (*
             * Make use of invariants discovered so far.
             *)
            val vcs' = List.map (vcs, instAlphaInvs am)
            val sol = solveAllForAlpha vcs' alphaId
          in
            AM.add am alphaId sol
          end handle CantSolve => am) 
    in
      raise (Fail "solve is Unimpl.")
    end
end

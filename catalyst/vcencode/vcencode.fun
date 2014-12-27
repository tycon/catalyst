functor VCEncode (S : VC_ENCODE_STRUCTS) : VC_ENCODE =
struct
  open S
  open VC
  structure HM = HoleMap
  structure TyD = TypeDesc
  structure RI = RelLang.RelId
  structure P = Predicate
  structure Hole = P.Hole
  structure BP = Predicate.BasePredicate
  structure RP = Predicate.RelPredicate
  structure L = Layout
  datatype result = Success | Undef | Failure
  structure Z3_Encode = Z3_Encode (structure Z3_FFI = Z3_FFI
                                   val z3_log = z3_log)
  exception TyDNotFound
  exception ConstNotFound
  exception RelNotFound
      
 (* structure TyMap : APPLICATIVE_MAP where
    type Key.t = TypeDesc.t and type Value.t = Z3_Encode.sort =
  struct
    structure Key = 
    struct
      type t = TyD.t
      val layout = TyD.layout
      val equal = TyD.sameType
    end
    structure Value =
    struct
      type t = Z3_Encode.sort
      val layout = fn _ => L.empty
    end
    structure Map = ApplicativeMap (structure Key = Key
                                   structure Value = RelLang.RelId)
    open Map
  end*)

  fun $ (f,arg) = f arg
  infixr 5 $
  val assert = Control.assert
  val ignore = fn _ => ()
  val log = z3_log

  val symbase = "!a"

  val count = ref 0
 
  val cegisBound = ref 10

  fun setCegisBound i = cegisBound := i

  val genSelector = fn _ => 
    let val id = symbase ^ (Int.toString (!count))
        val _ = count := !count + 1
    in
      Var.fromString id 
    end
  val varStrEq = fn (v1,v2) => Var.toString v1 = 
    Var.toString v2
  val intTyD = TyD.makeTconstr (Tycon.intInf,[])
  val boolTyD = TyD.makeTconstr (Tycon.bool,[])

  datatype hole_rp = RPEq of {lhs: RelLang.expr,
                                rhs: (Var.t * RelLang.expr) vector}

  (*
   * Takes e = (e1 U (e2 U e3)) U e4 and returns its holeRP version:
   * e = SEL(a1,e1) U SEL(a2,e2) U SEL(a3,e3) U SEL(a4,e4), along with
   * the vector of selectors : [a1,a2,a3,a4]
   *)
  fun toHoleRP (RP.Eq (lhsExpr, rhsExpr)) =
    let
      fun foldUnion (RelLang.U (e1,e2)) = Vector.concat 
        [foldUnion e1, foldUnion e2]
        | foldUnion e = Vector.new1 e
      val rhsExprs = foldUnion rhsExpr
      val (selectors,rhs) = Vector.unzip $ Vector.map (rhsExprs,
        fn e => (fn v => (v, (v,e))) (genSelector()))
    in
      (RPEq {lhs = lhsExpr, rhs=rhs}, selectors)
    end

  fun fromHoleRP (RPEq {lhs, rhs}) =
    let
      val rhsExpr = Vector.fold (Vector.map (rhs,#2),
        RelLang.emptyexpr (), RelLang.union)
    in
      RP.Eq (lhs,rhsExpr)
    end
  
  (*
   * Given a VC with one hole, a weaker solution to the hole, and a
   * state space of candidate solutions, selects a candidate solution
   * that can be used to strengthen the weaker solution.
   *)
  fun discharge (VC.T (tydbinds,anteP,conseqP)) knownRPs (holeRP, sels) =
    let
      val _ = print $ "Current Candidate RP:\n"
      val _ = print $ RP.toString $ fromHoleRP $ holeRP
      val _ = print $ "\n"
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
      val mkSelectableSet = #mkSelectableSet api
      val mkUnion = #mkUnion api
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
      val modelToString = #modelToString api
      val dischargeAssertion = #dischargeAssertion api
      val getValueOf = #getValueOf api
      val astToAssertion = #astToAssertion api
      (*
       * Maps to keep track of encoded values
       *)
      fun strEq (str1,str2) = (str1 = str2)
      val tyMap = HashTable.mkTable (MLton.hash, TyD.sameType) 
        (117, TyDNotFound)
      val _ = HashTable.insert tyMap (intTyD,int_sort)
      val _ = HashTable.insert tyMap (boolTyD,bool_sort)
      fun addTyD tyd = (fn sort => 
          (HashTable.insert tyMap (tyd,sort); sort))
        (mkUninterpretedSort ())
      val constMap = HashTable.mkTable (MLton.hash, strEq) 
        (117, ConstNotFound)
      val relMap = HashTable.mkTable (MLton.hash, strEq)
        (117, RelNotFound)
      fun getConstForVar v = 
        let
          val vstr = Var.toString v
          val c = HashTable.lookup constMap vstr handle ConstNotFound =>
            Error.bug ("Variable "^vstr^" undeclared despite \
            \processing tydbinds")
        in
          c
        end
      fun getStrucRelForRelId rid = 
        let
          val rstr = RI.toString rid
          val sr = HashTable.lookup relMap rstr handle RelNotFound =>
            Error.bug ("Rel "^rstr^" undeclared despite \
            \processing tydbinds")
        in
          sr
        end
      (*
       * Add selectors to tydbinds
       *)
      val selBinds = Vector.map (sels, fn sel => (sel,boolTyD))
      val tydbinds = Vector.concat [tydbinds, selBinds]
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

      local
        open RelLang
      in
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
          | U (e1,e2) => mkUnion (encodeRelExpr e1, 
              encodeRelExpr e2)
          | D (e1,e2) => mkDiff (encodeRelExpr e1, 
              encodeRelExpr e2)
          | R (rid,v) => mkStrucRelApp (getStrucRelForRelId rid,
              getConstForVar v)
      end

      fun encodeRelPred (rp:RP.t) : Z3_Encode.assertion =
        let
          val f = encodeRelExpr
          open RP
        in
          case rp of Eq (e1,e2) => mkSetEqAssertion (f e1, f e2)
          | Sub (e1,e2) => mkSubSetAssertion (f e1, f e2)
          | SubEq (e1,e2) => (fn s => mkOr $ Vector.new2 (mkSetEqAssertion s,
              mkSubSetAssertion s)) (f e1, f e2)
        end

      fun encodeHoleRP (RPEq {lhs,rhs}) = 
        let
          val lhsSet = encodeRelExpr lhs
          val rhsAsts = Vector.map (rhs, fn (sel,re) =>
            let
              val selAst = getConstForVar sel
              val s = encodeRelExpr re
            in
              mkSelectableSet (selAst,s)
            end)
          val rhsSet = Vector.fold (rhsAsts, mkNullSet (), 
            fn (rhsAst, rhs) => mkUnion (rhsAst,rhs))
        in
          mkSetEqAssertion (lhsSet,rhsSet)
        end

      fun encodeSimplePred (sp : VC.simple_pred) : Z3_Encode.assertion =
        case sp of 
          (Base bp) => encodeBasePred bp
        | (Rel rp) => encodeRelPred rp
        | (Hole h) => 
          let
            val Hole.T {substs, ...} = h
            val RPEq {lhs,rhs} = holeRP
            val substs = Vector.fromList substs
            val doSubstInRP = fn rp => RP.applySubsts substs rp
            val knownAssns = List.map (knownRPs, 
              encodeRelPred o doSubstInRP)
            val doSubstInRE = fn re => RelLang.applySubsts 
              substs re
            val thisHoleRP =  RPEq {lhs = doSubstInRE lhs, 
                rhs = Vector.map (rhs, 
                    fn (sel,re) => (sel, doSubstInRE re))}
            val holeRPAssn = encodeHoleRP thisHoleRP
            val holeAssn = mkAnd $ Vector.fromList $
              holeRPAssn::knownAssns
          in
            holeAssn
          end

      val assertSimplePred  = dischargeAssertion o encodeSimplePred

      fun encodeVCPred vcp = case vcp of 
          VC.Simple sp => encodeSimplePred sp
        | VC.Conj vcps => mkAnd $ Vector.map (vcps, encodeVCPred)
        | VC.Disj vcps => mkOr $ Vector.map (vcps, encodeVCPred)
        | VC.Not vcp => mkNot $ encodeVCPred vcp
        | VC.If (vcp1,vcp2) => mkIf (encodeVCPred vcp1, 
              encodeVCPred vcp2)
        | VC.Iff (vcp1,vcp2) => mkIff (encodeVCPred vcp1, 
              encodeVCPred vcp2)

      fun assertVCPred vcp = case vcp of 
          VC.Simple sp => assertSimplePred sp

        | VC.Conj spv => Vector.foreach (spv,assertVCPred)
        | _ => dischargeAssertion $ encodeVCPred vcp
    
      (*
       * Processing TyDBinds
       *)
      val _ = Vector.foreach (tydbinds, processTyDBind)

      val _ = assertVCPred anteP
      exception CegisFailure
      exception CegisSuccess of (Var.t * bool) vector
      fun cegisLoop (iter,invalidSels : Z3_Encode.assertion list) =
        let
          val _ = if (iter < !cegisBound) then () else (print $ 
            "CEGIS did not converge in " ^(Int.toString $ !cegisBound)
            ^" iterations. Terminating.\n";
            raise CegisFailure)
          val _ = print $ "------ CEGIS Iteration " 
            ^(Int.toString iter)^" ------\n"
          (*
           * Generate a candidate solution
           *)
          val _ = doPush ()
          (*
           * First rule out those solutions that are known to be
           * incorrect
           *)
          val _ = List.foreach (invalidSels, fn invalidSel =>
            dischargeAssertion $ mkNot invalidSel)
          (*
           * Check SAT of conseqP
           *)
          val _ = dischargeAssertion $ encodeVCPred conseqP
          val (res,model) = Z3_Encode.checkContextGetModel ctx
          val _ = doPop ()
          val _ = case res of Z3_Encode.SAT => ()
            | _ => (print "Oracle gaveup\n"; raise CegisFailure)
          (*val modelStr = modelToString model
          val _ = log modelStr
          val _ = log "\n\n" *)
          val (coreModel, selAsts) = Vector.unzip $ Vector.map (sels, 
            fn sv => 
              let
                val ast = getConstForVar sv
                val assn = astToAssertion ast
                val boolval = getValueOf model ast
                val _ = print $ (Var.toString sv)^" : "^(Bool.toString
                  boolval) ^"\n"
                val assnAst = if boolval then  assn else mkNot assn
              in
                ((sv,boolval), assnAst)
              end)
          val selection = mkAnd selAsts
          val _ = doPush ()
          (*
           * To check the validity of candidate solution, we check 
           * SAT of ¬conseqP
           *)
          val _ = dischargeAssertion selection
          val _ = dischargeAssertion $ mkNot $ encodeVCPred conseqP
          val res = Z3_Encode.checkContext ctx
          val _ = doPop ()
          val _ = case res of Z3_Encode.SAT => ()
            | Z3_Encode.UNSAT => raise (CegisSuccess coreModel)
            | Z3_Encode.UNKNOWN => Error.bug "Z3 timeout while deciding validity"
          val modelStr = modelToString model
          (*
          val _ = log "CounterExample:\n"
          val _ = log modelStr
          val _ = log "\n\n"
          *)
        in
          cegisLoop (iter+1, selection :: invalidSels)
        end handle CegisSuccess soln => SOME soln 
                 | CegisFailure => NONE
      val cegisRes = cegisLoop (0,[])
      val _ = Z3_Encode.delContext ctx
      fun solToHoleRP soln = case holeRP of RPEq {lhs,rhs} => 
        let
          val newSels= Vector.keepAllMap (soln, 
            fn (sv,true) => SOME sv | _ => NONE)
          val rhs' = Vector.keepAll (rhs, 
            fn (sv,_) => Vector.exists (newSels,
              fn (sel) => varStrEq (sel,sv)))
        in
          RPEq {lhs=lhs, rhs=rhs'}
        end
    in
      case cegisRes of NONE => NONE 
      | SOME soln => SOME $ solToHoleRP soln
    end 

    fun solve (vcs,hm) = 
      let
        (* only one hole for now *)
        val [(holeId,rps)] = Vector.toList $ HM.toVector hm
        val _ = Vector.foreach (vcs, fn (VC.T (_,anteP,conseqP)) =>
          case conseqP of Simple (Hole h) => 
            assert (P.Hole.idEq (P.Hole.idOf h, holeId), 
              "More than 1 hole Unimpl.")
          | _ => ())
        (* 
         * First, get selectors for the hole
         *)
        val holeRPSels = Vector.map (rps, toHoleRP)
        (* Only two VCs for now *)
        val [VC.T (tydbinds1,anteP1,conseqP1),
             VC.T (tydbinds2,anteP2,conseqP2)] = Vector.toList vcs
        val len = Vector.length
        val isVC0 = Var.fromString "vc!0"
        val anteP =  Conj $ Vector.fromList $ 
          [
            If (Simple $ Base $ BP.Eq (BP.Var isVC0, BP.Bool true),
                anteP1),
            If (Simple $ Base $ BP.Eq (BP.Var isVC0, BP.Bool false),
                anteP2)
          ]
        val (tydbinds,conseqP) = case (len tydbinds1 < len tydbinds2) of
          true => (tydbinds2,conseqP2) | false => (tydbinds1,conseqP1)
        val theVC = VC.T (Vector.concat [tydbinds, 
          Vector.new1 (isVC0,boolTyD)], anteP, conseqP)
        val solRPs = Vector.fromList $ Vector.fold (holeRPSels, [], 
          fn ((holeRPWithSels),solsAcc) =>
            case discharge theVC solsAcc holeRPWithSels of
              NONE => solsAcc | SOME sol => (fromHoleRP sol)::solsAcc)
      in
        HM.add (HM.empty) holeId solRPs
      end
end

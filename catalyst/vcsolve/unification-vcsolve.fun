functor VCSolve (S : VC_SOLVE_STRUCTS) : VC_SOLVE =
struct
  open S
  open VC
  structure TyD = TypeDesc
  structure TyDB = TyDBinds
  structure RI = RelLang.RelId
  structure RelTy = RelLang.RelType
  structure BP = Predicate.BasePredicate
  structure RP = Predicate.RelPredicate
  structure L = Layout
  structure C = Control

  fun $ (f,arg) = f arg
  infixr 5 $
  val assert = Control.assert
  val empty_set = Vector.new0 ()
  fun varStrEq (v1,v2) = (Var.toString v1 = Var.toString v2)
  fun rappEq (RelLang.R (rid1,vid1), RelLang.R (rid2,vid2)) =
    RI.toString rid1 = RI.toString rid2 andalso
    Var.toString vid1 = Var.toString vid2
  fun sanitizeRE re = 
    let
      open RelLang
      val len = Vector.length
    in
      case re of 
        U (T el, re') => (assert (len el = 0, "Constant sets \
          \Unimpl."); sanitizeRE re')
      | U (re', T el) => (assert (len el = 0, "Constant sets \
          \Unimpl."); sanitizeRE re')
      | U (re',re'') => U (sanitizeRE re', sanitizeRE re'')
      | ratom => ratom
    end

  fun toDNF re = raise (Fail "Unimpl.")

  fun allInpCombosOfSort dom sort = raise (Fail "Unimpl.")

  fun sameRE (re1,re2) = 
    let
      val len = Vector.length
      open RelLang
    in
      case (re1,re2) of
        (U (re11,re12), U (re21,re22)) => sameRE (re11,re21) 
              andalso sameRE (re12,re22)
      | (X (re11,re12), X (re21,re22)) => sameRE (re11,re21) 
              andalso sameRE (re12,re22)
      | (D (re11,re12), D (re21,re22)) => sameRE (re11,re21) 
              andalso sameRE (re12,re22)
      | (rapp1 as R _, rapp2 as R _) => rappEq (rapp1,rapp2)
      | (T els1, T els2) => len els1 = 0 andalso len els1 = len els2 
      | _ => false

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
                  AVCM.add avcm id avcs'
                end
          | (VC.T (_,_,conseqP),_) => Error.bug $ 
              "Unexpected structure of conseqP:\n" ^
              (L.toString $ VC.layout $ Vector.new1 $ 
                    VC.T (Vector.new0 (), Simple True, conseqP)))
    in
      avcMap
    end

  datatype constraint = C of {tydbinds: TyDB.t, 
                              dom: (RelLang.expr*RelTy.t) list,
                              inpEqs: RP.t list, 
                              intEqs: RP.t list, 
                              opEq: RP.t }
  fun layoutC (C {inpEqs, intEqs, opEq, ...}) = 
    let
      val inpEqsLyt = L.align $ List.map(inpEqs, L.str o RP.toString) 
      val intEqsLyt = L.align $ List.map(intEqs, L.str o RP.toString) 
      val opEqLyt = L.str $ RP.toString opEq 
      val lyt = L.align[L.str "Input Equations:",
                        inpEqsLyt, 
                        L.str "Intermediary Equations:",
                        intEqsLyt,
                        L.str "---------------------------",
                        opEqLyt]
    in
      lyt
    end

  datatype s_constraint = SC of {tydbinds: TyDB.t, 
                                 dom:(RelLang.expr*RelTy.t) list,
                                 inpEqs: RP.t list, 
                                 opEq: RP.t }
  fun layoutSC (SC {inpEqs, opEq, ...}) = 
    let
      val inpEqsLyt = L.align $ List.map(inpEqs, L.str o RP.toString) 
      val opEqLyt = L.str $ RP.toString opEq 
      val lyt = L.align[L.str "Input Equations:",
                        inpEqsLyt, 
                        L.str "---------------------------",
                        opEqLyt]
    in
      lyt
    end

  fun simplifyConstraint (C {tydbinds=tyDB, dom, inpEqs,
                             intEqs, opEq}) =
    let
      val substs = List.map (intEqs, 
        fn (RP.Eq (rapp,e)) => (e,rapp))
      fun doSubstInEq (e,rapp) (RP.Eq (e1,e2)) = RP.Eq (e1,
        RelLang.mapRApp e2 (fn rapp' => if rappEq (rapp,rapp') 
          then e else rapp'))
      fun doSubstsInEq substs eq = List.foldr (substs,eq,
        fn (subst,eq) => doSubstInEq subst eq)
      fun simplifyIntEqs old [] = old
        | simplifyIntEqs old ((eq as RP.Eq (rapp,e))::new) = simplifyIntEqs
            (eq::(List.map (old, doSubstInEq (e,rapp))))
            (List.map (new, doSubstInEq (e,rapp)))
      val inpEqs' = List.map (inpEqs, doSubstsInEq substs)
      val opEq' = doSubstsInEq substs opEq
    in
      SC {tydbinds=tyDB, dom=dom, inpEqs=inpEqs', opEq=opEq'}
    end

  exception CantUnify

  fun tryUnify (ob,re) = raise (Fail "Unimpl.")

  fun hypothesizeCombos dom inpEqs ratom = 
    let
      open RelLang
    in
      case ratom of
        Alpha {sort,substs, ...} => 
          let
            val applySubstsIn = applySubsts (Vector.fromList substs)
            (*
             * Generate all input combinations of the sort that 
             * α is supposed to be.
             *)
            val inpCombos = allInpCombosOfSort dom sort
            (*
             * Retain only ones that are idempotent under
             * substitution. Only they meet the well-formedness
             * condition.
             *)
            val (validCombos : expr list) = List.keepAll (inpCombos, 
              fn comboRE => sameRE (applySubstsIn comboRE, comboRE))
            val sol_obs = List.map (validCombos, fn validCombo => 
              (validCombo, T empty_set))
          in
            sol_obs
          end
      | _ => raise (Fail "Unimpl.")
    end

  fun applyHypothesis hyp re =
    let
      open RelLang
      fun doSubstsInHyp substs = applySubsts 
          (Vector.fromList substs) hyp
      fun doItRatom ratom = case ratom of
          Alpha {substs, ...} => U (doSubstsInHyp substs,
            ratom)
        | X (s,ratom) => toDNF $ X (doItRatom s, doItRatom ratom)
        | U _ => Error.bug "applyHypothesis: not in DNF"
        | D _ => raise (Fail "Unimpl.")
        | _ => ratom
    in
      case re of
        U (ratom, re') => (* α is always in the last disjunct *)
            U (ratom, applyHypothesis hyp re')
      | ratom => doItRatom ratom
    end 

  fun findAllInputCombos (SC {tydbinds=tyDB, dom, inpEqs, opEq}) =
    let
      val RP.Eq (lhsRE,re) = opEq
      fun scWithOpEqRhsAs newRE = SC {tydbinds=tyDB, dom=dom,
                                      inpEqs=inpEqs,
                                      opEq = RP.Eq (lhsRE,newRE)}
      val hypo = hypothesizeCombos dom inpEqs 
      val len = Vector.length
      open RelLang
    in
      (*
       * re should actually be in DNF. We need a separate datatype for
       * relational expressions in DNF.
       *)
      case re of 
        T elems => if len elems = 0 then
            let
              val sol_obs = hypo (T empty_set)
              val sols = List.keepAllMap (sol_obs,
                fn (sol, T el) => if len el = 0 then SOME sol 
                                  else NONE
                  | _ => NONE)
            in
              (T empty_set)::sols
            end
          else
            raise (Fail "findAllInputCombos: Const sets Unimpl.")
      | U (ratom, re') => 
          let
            val hyp_obs = hypo ratom
            val hyp_resREs = List.keepAllMap (hyp_obs,
              fn (hyp,ob) => 
                let
                  val re'' = applyHypothesis hyp re'
                  val residueRE = tryUnify (ob,re'')
                in
                  SOME (hyp,residueRE)
                end handle CantUnify => NONE)
            val combos = List.concat $ List.map (hyp_resREs,
              fn (hyp,resRE) => 
                let
                  val resREsols = findAllInputCombos $ 
                      scWithOpEqRhsAs resRE
                  (*
                   * Observe that if resREsols is an empty list, we
                   * return an empty list.
                   *)
                  val sols = List.map (resREsols, 
                    fn resREsol => U (hyp,resREsol))
                in
                  sols
                end)
          in
            combos
          end
        | ratom => findAllInputCombos $ scWithOpEqRhsAs 
                                        (U (ratom, T empty_set))
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
       * Find conseqP eqn, its RApp, RelId, and bound var.
       *)
      val cspEqn = case conseqP of Simple (Rel eq) => eq
        | _ => raise (Fail "Impossible case")
      val (cspRApp, cspRID, cspBV) = case cspEqn of 
        RP.Eq (rapp as RelLang.R (rid,v),_) => (rapp, rid, v)
      (*
       * RHS of cspEqn is an alpha with substs. Co-domain of substs is
       * cspBV U inputVars. We extract inputVars using this.
       *)
      val inputVars = case cspEqn of 
          RP.Eq (_,RelLang.Alpha {substs, ...}) => #1 $ List.unzip $ 
            List.keepAll (substs,fn (v,_) => not (varStrEq (v,cspBV)))
        | _ => raise (Fail "Impossible Case")
      (*
       * Domain RApps of the solution is the set of all possible relational
       * abstractions of input vars.
       *)
      val domRApps = List.concat $ List.map (inputVars, 
        fn v => List.map (relsWithDomTyd $ TyDB.find tyDB v, 
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
       * Eqs in allEqs whose RHS is an alpha different from the
       * given alpha are useless. Discard them.
       *)
       val allEqs' = List.keepAll (allEqs, 
        fn (RP.Eq (e1,e2 as RelLang.Alpha {id, ...})) => id=alphaId
         | _ => true)
      (*
       * Output eqn is the equation whose LHS Rapp is same as that of
       * cspEqn'. Lets extract the rapp from cspEqn'.
       *)
      val {no=restEqs, yes=opEqs} = List.partition (allEqs',
        fn (RP.Eq (rapp',_)) => rappEq (cspRApp,rapp')
         | _ => false)
      val opEq = case opEqs of [RP.Eq (lhs,rhs)] => 
                        RP.Eq (lhs, sanitizeRE rhs) 
        | _ => raise (Fail "anteP is expected to contain exactly one\
          \ output equation.\n")
      (*
       * Among the restEqs, input equations are those whose LHS is an
       * RApp \in domRApps. Remaining are intermediary equations.
       *)
      val {yes=inpEqs, no=intEqs } = List.partition (restEqs, 
        fn (RP.Eq (rapp,_)) => List.exists (domRApps,
          (fn (domRApp,_) => rappEq (domRApp,rapp))))
      val cs = C {tydbinds = tyDB, 
                  dom=domRApps, 
                  inpEqs=inpEqs, 
                  intEqs=intEqs,
                  opEq=opEq}
      (*
       * -----------------------------------------------
       *   CONSTRAINT SIMPLIFICATION 
       * -----------------------------------------------
       *)
      val scs = simplifyConstraint cs
      val _ = L.print (layoutSC scs, print)
      val _ = print "\n\n"
      val _ = findAllInputCombos scs
    in
      NONE
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
      raise (Fail "Unimpl.")
    end 
end

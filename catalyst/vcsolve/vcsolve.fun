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
  val len = Vector.length
  val fromJust = fn (SOME x) => x 
      | NONE => raise (Fail "fromJust")
  fun vecEq (vec1,vec2,f) = len vec1 = len vec2 andalso
    Vector.forall2 (vec1,vec2,f)
  fun varStrEq (v1,v2) = (Var.toString v1 = Var.toString v2)
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
  (*
   * Converts list of sets to set of lists
   * flatten [[a,b],[c],[d]] = [[a,c,d],[b,c,d]]
   *)
  fun flatten (l : 'a list list) : 'a list list = case l of
      [] => []
    | [zs] => List.map (zs, fn z => [z])
    | xs::yszs => List.concat $ List.map (xs, fn x => 
        List.map (flatten yszs, fn yz => x::yz))
    
  fun listCrossPrd (l1,l2,f) = case l2 of
      [] => []
    | x2::xs2 => List.concat 
        [List.foldr (l1,x2, 
            fn (x1,acc) => (f (x1,x2))::acc), 
         listCrossPrd (l1,xs2,f)]
  fun rappEq (RelLang.R (rid1,vid1), RelLang.R (rid2,vid2)) =
    RI.toString rid1 = RI.toString rid2 andalso
    Var.toString vid1 = Var.toString vid2
  local
    open RelLang
  in
    fun isEmptySet (T els) = (len els = 0)
      | isEmptySet _ = false

    fun allDisjsInRExpr (re : RelLang.expr) : RelLang.expr list =
      case re of 
        U (re1,re2) => List.concat [allDisjsInRExpr re1,
                                    allDisjsInRExpr re2]
      | D _ => raise (Fail "Unimpl.")
      | _ => [re]

    fun allRappsInRatom (ratom : RelLang.expr) : RelLang.expr list = 
      case ratom of
          R _ => [ratom]
        | X (e1,e2) => List.concat [allRappsInRatom e1, allRappsInRatom e2]
        | T els => []
        | _ => Error.bug "allRappsInRatom: DNF assumption violated"

    fun crossPrdOf (expr::exprs) = List.fold (exprs, expr, 
          fn (expr,acc) => crossprd (acc,expr))
      | crossPrdOf [expr] = expr
      | crossPrdOf [] = RelLang.T empty_set

    fun unionOf [] = RelLang.T empty_set
      | unionOf [re] = re
      | unionOf (re::rest) = U (re,unionOf rest)

    (*
     * Expects re in DNF. Removes unions and cross-products with
     * empty-set.
     *)
    fun sanitizeRE re = 
      let
        val msg = "Constant sets Unimpl."
        fun simplifyCPs re = case re of
            X (T el,_) => (assert (len el = 0, msg); T empty_set)
          | X (_,T el) => (assert (len el = 0, msg); T empty_set)
          | X (re1,re2) => (case (simplifyCPs re1,simplifyCPs re2) of
                (re1' as T _, re2') => simplifyCPs $ X (re1',re2')
              | (re1', re2' as T _) => simplifyCPs $ X (re1',re2')
              | (re1',re2') => X (re1',re2'))
          | U (re1,re2) => U (simplifyCPs re1, simplifyCPs re2)
        fun simplifyUnions re = case re of 
            U (T el, re') => (assert (len el = 0, msg); sanitizeRE re')
          | U (re', T el) => (assert (len el = 0, msg); sanitizeRE re')
          | U (re',re'') => U (sanitizeRE re', sanitizeRE re'')
          | ratom => ratom
      in
        simplifyUnions $ simplifyCPs re
      end

    fun toDNF (U (re1,re2)) = U (toDNF re1, toDNF re2)
      | toDNF (X (U (re11,re12),re2)) = U (toDNF (X (re11,re2)),
                                           toDNF (X (re12,re2)))
      | toDNF (X (re1,U (re21,re22))) = U (toDNF (X (re1,re21)),
                                           toDNF (X (re1,re22)))
      | toDNF (D _) = raise (Fail "Unimpl.")
      | toDNF re = re

    fun allInpCPsOfSort dom (RelTy.Tuple tyds) = 
      if len tyds = 0 then [] 
      else List.concat $ List.keepAllMap (dom,
          fn (bigS,RelTy.Tuple tyds') => 
            let
              val restTyDs = suffix (tyds, tyds', TyD.sameType)
              val atoms'= allInpCPsOfSort dom $
                                  RelTy.Tuple restTyDs
              val atoms = case atoms' of [] => [bigS]
                | _ => List.map (atoms', fn atom' => X (bigS,atom'))
            in
              SOME $ atoms
            end handle NotPrefix => NONE)

    (*
     * map (set of all subsets of atoms, U)
     *)
    fun allUnionCombos [] = []
      | allUnionCombos (a::atoms') = 
          let
            val combos' = allUnionCombos atoms'
            val combos = List.concat [a::combos',
              List.map (combos', fn combo' => U (a,combo'))]
          in
            combos
          end

    fun allInpCombosOfSort dom sort = 
      let
        val combos =  allUnionCombos $ 
          allInpCPsOfSort dom sort
        (*
        val _ = print $ "\tHere are allInpCPsOfSort "
          ^(RelTy.toString sort)^": \n"
        val _ = L.print (L.align $ List.map (combos,
          L.str o exprToString),print)
        *)
      in
        combos
      end

    fun allCPSplits (rapps : RelLang.expr(*RApp*) list) 
        : RelLang.expr(*RAtom*) list list  = 
      let
        val splits = case rapps of 
            [] => []
          | [s] => [[s]]
          | [s1,s2] => [[X (s1,s2)], [s1,s2]]
          | [s1,s2,s3] => [[X ((X (s1,s2)),s3)], [X (s1,(X (s2,s3)))],
                           [X (s1,s2), s3], [s1, X (s2,s3)], 
                           [s1,s2,s3]]
          | _ => raise (Fail "Unimpl.")
      in
        splits
      end

    fun sameRE (re1,re2) = case (re1,re2) of
        (U (re11,re12), U (re21,re22)) => sameRE (re11,re21) 
              andalso sameRE (re12,re22)
      | (X (re11,re12), X (re21,re22)) => sameRE (re11,re21) 
              andalso sameRE (re12,re22)
      | (D (re11,re12), D (re21,re22)) => sameRE (re11,re21) 
              andalso sameRE (re12,re22)
      | (rapp1 as R _, rapp2 as R _) => rappEq (rapp1,rapp2)
      | (T els1, T els2) => len els1 = 0 andalso len els1 = len els2 
      | _ => false

    (*
     * Size of an RE in DNF is the number of disjuncts in RE.
     *)
    fun sizeRE (U (e1,e2)) = 1 + (sizeRE e1) + (sizeRE e2)
      | sizeRE (D (e1,e2)) = 1 + (sizeRE e1) + (sizeRE e2)
      | sizeRE _ = 1

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
    type Key.t = int and type Value.t = RP.t =
  struct
    structure Key = 
    struct
      type t = int
      val equal = op=
      val layout = L.str o Int.toString
    end
    structure Value =
    struct
      type t = RP.t 
      val layout = L.str o RP.toString
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

  datatype constraint = C of {tydbinds: TyDB.t, 
                              dom: (RelLang.expr*RelTy.t) list,
                              inpEqs: RP.t list, 
                              intEqs: RP.t list, 
                              opEq: RP.t,
                              sort : RelTy.t}
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
                                 opEq: RP.t,
                                 sort : RelTy.t}
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

  (*
   * -----------------------------------------------
   *   CONSTRAINT SIMPLIFICATION 
   * -----------------------------------------------
   *)
  fun simplifyConstraint (C {tydbinds=tyDB, dom, inpEqs,
                             intEqs, opEq, sort}) =
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
      SC {tydbinds=tyDB, dom=dom, inpEqs=inpEqs', 
            opEq=opEq', sort=sort}
    end

  (*
   * -------------------------------------
   *          UNIFICATION
   * -------------------------------------
   *)
  exception CantUnify

  local
    open RelLang
  in
    fun unifiable (ratom1,ratom2) = 
      let
        fun doIt (ratom1,ratom2) = case (ratom1,ratom2) of 
             (X (rapp1,ratom1'), X (rapp2,ratom2')) => 
                (rappEq (rapp1,rapp2) andalso 
                  doIt (ratom1',ratom2'))
           | (rapp1 as R _,rapp2 as R _) => rappEq (rapp1,rapp2)
           | (T els1, T els2) => len els1 = 0 andalso len els2 = 0
           | _ => false
         val isUnif = doIt (ratom1,ratom2)
         val ratomStr1 = exprToString ratom1
         val ratomStr2 = exprToString ratom2
         (*
         val _ = print $ ratomStr1 ^" and "^ ratomStr2 ^" are"
          ^(if isUnif then "" else " NOT")^" unifiable\n"
          *)
      in
        isUnif
      end 

    fun tryUnify (re1 (*ob*),re2)  = 
      let
        (*val _ = print $ "tryUnify: "^(exprToString re1)
          ^" with "^(exprToString re2)^"\n"*)
      in
        case re1 of 
          U (ratom, re1') => 
            let
              val re2' = tryUnify (ratom,re2)
            in
              tryUnify (re1',re2')
            end
        | D _ => raise (Fail "Unimpl.")
        | T els => if len els = 0 then re2
                    else raise (Fail "Unimpl.")
        | ratom => 
            let
              val isUnifiable = ref false
              val disjs = allDisjsInRExpr re2
              val leftOvers = List.keepAll (disjs,
                fn (disj) => if unifiable (ratom,disj) 
                  then (isUnifiable := true; false)
                  else true) 
              val _ = if (not (!isUnifiable)) 
                then raise CantUnify else ()
              val res = unionOf leftOvers
              (*
              val _ = print $ "Unified. Left-over is: "
                    ^(exprToString res)^"\n"
              *)
            in
              res
            end
      end 

    (*
     * Finds an eqns of form [S = sre] in inpEqs such that disjuncts
     * of re are subset of disjuncts of sre. Returns (S,sre'), where
     * sre' is the union of remaining disjuncts.
     *)
    fun tryUnifySome (re, inpEqs) 
      : (RelLang.expr * RelLang.expr) list= 
        case List.keepAllMap  
            (inpEqs, fn (RP.Eq (bigS,sre)) => 
              let
                val sreLeft = tryUnify (re,sre)
              in
                SOME (bigS,sreLeft)
              end handle CantUnify => NONE) of
          [] => raise CantUnify
        | l => l

    (*
     * unifySome is like tryUnifySome, but returns S only if re and
     * sre contain same disjuncts (i.e., they are equivalent). 
     *)
    fun unifySome (re, inpEqs) = 
      List.keepAllMap (tryUnifySome (re,inpEqs), 
        fn (sol,ob) => if isEmptySet ob then SOME sol else NONE)
  end
  (*
   * -------------------------------------
   *        HYPOTHESIS GENERATION
   * -------------------------------------
   *)
  fun hypothesizeCombos dom inpEqs sort (ratom : RelLang.expr) = 
    let
      open RelLang
      fun isInputRAbs rapp = List.exists (dom, 
        fn (rapp',relTy) => rappEq (rapp,rapp'))
      fun sortOfInputRAbs rapp = fromJust $ 
        List.peekMap (dom, fn (rapp',relTy) => 
          if rappEq (rapp',rapp) then SOME relTy else NONE)
      fun solObToStr (s,ob) = "("^(exprToString s)^","
                                 ^(exprToString ob)^")"
      fun doIt () = 
        let
          (*
           * We need to consider all possible crossprd splits of ratom
           * for unification.
           *)
          val (splits: expr(*ratom*) list list) = 
                    allCPSplits $ allRappsInRatom ratom
          val ratomStr = exprToString ratom
          (*
          val _ = print $ "hypothesizeCombos: splits for "
            ^ratomStr^":\n"
          val _ = L.print (L.align $ List.map (splits, 
              fn ratoms => List.layout (L.str o exprToString) ratoms),
            print)
          val _ = print "\n"
          *)
          (*
           * Each split is a list of RAtoms, and each RAtom may be
           * unified with multiple S's. Therefore, each split can have
           * multiple sols.
           *)
          val sol_obs = List.concat $ List.keepAllMap (splits, 
            fn ratoms => 
              let
                (*
                 * If an RAtom in a split can't be unified with any
                 * inpEqn, then this split is useless. tryUnifySome
                 * raises CantUnify, which leads to this split being
                 * discarded.
                 *)
                val xsys : (expr*expr) list list = List.map (ratoms,
                  fn ratom => tryUnifySome (ratom,inpEqs))
                (*
                val _ = print $ "hypothesizeCombos: tryUnifySome \
                  \returned:\n"
                val _ = L.print (L.align $ List.map (xsys, 
                    fn sol_obs => List.layout (L.str o solObToStr)
                        sol_obs),
                  print)
                val _ = print "\n"
                *)
                (*
                 * Each list in xys is a list of (RAtomSol,RAtomOb)
                 * pairs, such that each pair is a sol-ob for a unique
                 * RAtom. Their crossprd gives us a (sol,ob) pair.
                 *)
                val xys = flatten xsys
                val _ = List.foreach (xys, fn xy => assert 
                  (List.length xy = List.length ratoms, "Number of \
                    \(RAtomSol,RAtomOb) pairs in a solution did not \
                    \match the number of actual RAtoms."))
                val sol_obs = List.map (xys, 
                  fn (ra_sol_obs) =>
                    let
                      val (ra_sols,ra_obs) = List.unzip ra_sol_obs
                      val sol = crossPrdOf ra_sols
                      val ob = toDNF $ crossPrdOf ra_obs
                    in
                      (sol,ob)
                    end)
              in
                SOME sol_obs
              end handle CantUnify => NONE)
            (*
            val _ = print $ "hypothesizeCombos: returning \
              \following sol_ob pairs:\n"
            val _ = L.print (L.align $ List.map 
                (sol_obs,L.str o solObToStr), print)
            val _ = print "\n"
            *)
        in
          sol_obs
        end
    in
      case ratom of
        Alpha {sort,substs, ...} => 
          let
            (*val _ = print $ "\tHypothesizing combos for alpha:"
              ^(exprToString ratom)^"\n"*)
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
      | rapp as R _ => if isInputRAbs rapp 
            then [(ratom, RelLang.T empty_set)] else doIt()
      | T els => 
          let
            val _ = if len els = 0 then () else raise CantUnify
            val sol_obs = tryUnifySome (T empty_set,inpEqs)
            (* Remove hyps that don't pass typecheck *)
            val (sols,obs) = List.unzip $ List.keepAll (sol_obs,
              fn (sol,ob) => RelTy.equal (sortOfInputRAbs sol,sort))
            (*
             * If S1 and S2 can be unified with empty-set producing
             * obligations ob1 and ob2, then S1 U S2 can also be
             * unified with empty-set with obligation as ob1 U ob2.
             *)
            (*
            val _ = print $ "\t hypo: Sols are:\n"
            val _ = L.print (List.layout (L.str o exprToString) sols, 
              print)
            val _ = print "\n"
            *)
            val uSols = allUnionCombos sols
            (*
            val _ = print $ "\t hypo: uSols are:\n"
            val _ = L.print (List.layout (L.str o exprToString) uSols, 
              print)
            val _ = print "\n"
            *)
            val uObs = List.map (allUnionCombos obs, sanitizeRE)
          in
            List.zip (uSols,uObs)
          end
      | _ => doIt ()
    end

  fun applyHypothesis hyp (re : RelLang.expr) : RelLang.expr =
    let
      open RelLang
      val _ = print $ "applyHypothesis: hyp "^(exprToString hyp)
        ^" in "^(exprToString re)^"\n"
      fun doSubstsInHyp substs = applySubsts 
          (Vector.fromList substs) hyp
      fun doItRatom ratom = case ratom of
          Alpha {substs, ...} => U (doSubstsInHyp substs,
            ratom)
        | X (s,ratom') => toDNF $ X (doItRatom s, doItRatom ratom')
        | U _ => Error.bug "applyHypothesis: Expected RAtom. \
                            \Got RExpr."
        | D _ => Error.bug "applyHypothesis: Expected RAtom. \
                            \Got RExpr."
        | _ => ratom
      fun return re = (print $ "applyHypothesis: returning "^
        (exprToString re)^"\n"; re)
    in
      case re of
        U (ratom, re') => (* α is always in the last disjunct *)
            U (ratom, applyHypothesis hyp re')
      | ratom => return $ doItRatom ratom
    end 

  fun findAllInputCombos (SC {tydbinds=tyDB, dom, inpEqs, 
                              opEq, sort}) =
    let
      val RP.Eq (lhsRE,re) = opEq
      fun scWithOpEqRhsAs newRE = SC {tydbinds=tyDB, dom=dom,
                                      inpEqs=inpEqs,
                                      opEq = RP.Eq (lhsRE,newRE),
                                      sort=sort}
      val hypo = hypothesizeCombos dom inpEqs sort
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
              (*
               * Retain only those hypotheses which do not entail new
               * obligations.
               *)
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
            val ratomStr = exprToString ratom
            (*
            val _ = print $ "findAllInputCombos: HypOb pairs for "
              ^ratomStr^":\n"
            val _ = L.print (L.align $ List.map (hyp_obs, 
              fn (hyp,ob) => L.str $ "("^(exprToString hyp)^","
                  ^(exprToString ob)^")"),print)
            val _ = print "\n"
            *)
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
        (*
         * We have the invariant that opEqRhs does not contain a union
         * with empty-set; it is sanitized. 
         * However, we would like to find solution disjuncts for
         * empty-set also .
         *)
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
      fun sortOfRApp (RelLang.R (relId,v)) =
        let
          val rels = HashTable.lookup relTab (TyDB.find tyDB v)
          fun relStrEq (rid1,rid2) = 
            RI.toString rid1 = RI.toString rid2
        in
          fromJust $ List.peekMap (rels, fn (relId',sort) => 
            if relStrEq (relId',relId) then SOME sort else NONE)
        end
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
      | _ => raise (Fail "solveThisForAlpha: anteP not conjunction")
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
      val saneAllEqs = List.map (allEqs', fn (RP.Eq (e1,e2)) =>
          RP.Eq (e1,sanitizeRE e2))
      (*
       * Output eqn is the equation whose LHS Rapp is same as that of
       * cspEqn'. Lets extract the rapp from cspEqn'.
       *)
      val {no=restEqs, yes=opEqs} = List.partition (saneAllEqs,
        fn (RP.Eq (rapp',_)) => rappEq (cspRApp,rapp')
         | _ => false)
      val (opEq,opEqSort) = case opEqs of [RP.Eq (lhs,rhs)] => 
                        (RP.Eq (lhs, sanitizeRE rhs), sortOfRApp lhs)
        | [] => raise (Fail "anteP is expected to contain exactly one\
          \ output equation. None found.\n")
        | _ => raise (Fail "anteP is expected to contain exactly one\
          \ output equation. Many found \n")
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
                  opEq=opEq,
                  sort=opEqSort}
      val scs = simplifyConstraint cs
      (*
       * ToDo: intEqs should also be maintained in scs. Why?
       * Whereas RApps of intermediate vars have been eliminated
       * during constraint simplification, the vars themselves are
       * still lurking in pending-substs of alphas. When
       * pending-substs are applied to a hypothesis (which is an input
       * RAbs), the resultant RAtom could be an RApp of intermediate
       * var! This RApp can only be eliminated if we apply intEqs.
       *)
      val _ = print " -------------------------------- \n"
      val _ = L.print (layoutSC scs, print)
      val _ = print "\n"
      val solREs = findAllInputCombos scs
      val (sols : RP.t list) = List.map (solREs, 
          fn solRE => RP.Eq (cspRApp,solRE))
      val _ = print "Here are solutions:\n"
      val _ = L.print (L.align $ List.map (sols, 
          fn solRP => L.str $ RP.toString solRP), 
        print)
      val _ = print "\n"
      val _ = print " -------------------------------- \n"
    in
      sols
    end

  (*
   * Pre-condition: vcs have same single alpha in their conseqP.
   * Solves VCs and generates solution for that alpha.
   *)
  exception AlphaUnsolvable
  fun solveAllForAlpha (vcs : VC.t list) (alphaId:int) : RP.t = 
    let
      val solss = List.map (vcs, fn vc => solveThisForAlpha vc alphaId)
      (*
       * Return if alpha is obviously unsolvable (if one of its VCs
       * has no sols).
       *)
      val _ = List.foreach (solss, fn sols => 
        if List.length sols = 0 then raise AlphaUnsolvable else ())
      (*
       * For a sol to be a sol of alpha, it has to be a sol of all VCs
       * of that alpha.
       *)
      val vc1sols::vcxsolss = solss
      val alphaSols = List.keepAll (vc1sols, 
        fn (vc1sol as RP.Eq (_,vc1solRE)) =>
          let
            open RelLang
            val _ = List.foreach (vcxsolss,
              fn vcxsols => 
                let
                  val sols = unifySome (vc1solRE, vcxsols)
                in
                  if List.length sols = 0 then raise CantUnify
                    else ()
                end)
          in
            (*
             * We come here iff vc1solRE can be unconditionally
             * unified with some vcxsolRE, forall x=2...n.
             * Otherwise, CantUnify is raised.
             *)
            true
          end handle CantUnify => false)
      val _ = if List.length alphaSols = 0 then raise AlphaUnsolvable
                else ()
      val _ = print "\n ********************************* \n"
      val _ = print $ "Solutions of alpha ("^(Int.toString alphaId)
        ^") are:\n"
      val _ = L.print (L.align $ List.map (alphaSols, 
          L.str o RP.toString), print)
      val _ = print "\n ********************************* \n"
      (*
       * Every eq in alphaSols is a solution. All sols must be
       * equivalent because there are no pre-conditions. Pick a sol
       * with minimum size.
       *)
      val solx::solxs = alphaSols
      fun sizeRPEq (RP.Eq (_,re)) = sizeRE re
        | sizeRPEq _ = Error.bug "sizeRPEq: Undefined"
      val (minSol, _) = List.fold (alphaSols, (solx,sizeRPEq solx),
        fn (sol,(solAcc, min)) => if sizeRPEq sol < min 
                                  then (sol,sizeRPEq sol)
                                  else (solAcc,min))
    in
      minSol
    end

  fun instAlphaInvs am (VC.T (tydbinds,anteP,conseqP)) =
    let
      open VC
      fun doItTup f (x1,x2) = (f x1, f x2)
      fun doItSP sp = case sp of
          Rel (RP.Eq (_,RelLang.Alpha {id,substs,...})) => 
            (let
              val solRP = AM.find am id
              val thisRP = RP.applySubsts (Vector.fromList substs)
                              solRP
            in
              Rel thisRP
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
      val _  = print $ "Solving "^(Int.toString $ Vector.length vcs)
        ^" VCs ...\n"
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
       * For each alpha, solve VCs to get solutions. If an alpha
       * does not have a solution, we simply discard it. Subsequently,
       * it gets dropped from the conjuncts of the hole solution.
       *)
      val (avcs : (int * VC.t list) vector) = AVCM.toVector avcMap
      val (alphaId,rmemVCs) = Vector.sub (avcs,1)
      val rmemSols = solveAllForAlpha rmemVCs alphaId
      (*
      val am = Vector.fold (avcs,AM.empty,
        fn ((alphaId,vcs),am) => 
          let
            (*
             * Make use of invariants discovered so far.
             *)
            val _ = print $ "Solving VCs for alphaId="
              ^(Int.toString alphaId)
            val vcs' = List.map (vcs, instAlphaInvs am)
            val sol = solveAllForAlpha vcs' alphaId
          in
            AM.add am alphaId sol
          end handle AlphaUnsolvable => am) 
      *)
    in
      raise (Fail "Unimpl.")
    end 
end

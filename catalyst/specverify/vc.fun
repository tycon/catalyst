functor VerificationCondition (S : VERIFICATION_CONDITION_STRUCTS)
  : VERIFICATION_CONDITION =
struct
  open S
  structure P = Predicate
  structure BP = Predicate.BasePredicate
  structure RP = Predicate.RelPredicate
  structure Hole = Predicate.Hole
  structure RelId = RelLang.RelId
  structure RefTy = RefinementType
  structure RefTyS = RefinementTypeScheme
  structure RelTy = RelLang.RelType
  structure RelTyS = RelLang.RelTypeScheme
  structure RI = RelLang.RelId
  structure TyD = TypeDesc
  structure Env = TyDBinds
  structure L = Layout

  exception TrivialVC (* raised when antecedent has false *)
  exception HoleNotFound (* raised by hash from holeId to sol *)

  type tydbind = Var.t * TyD.t
  type tydbinds = tydbind vector

  datatype simple_pred = True
                       | False
                       | Hole of Hole.t
                       | Base of BP.t 
                       | Rel of RP.t

  datatype vc_pred =  Simple of simple_pred
                   |  If of vc_pred * vc_pred
                   |  Iff of vc_pred * vc_pred
                   |  Conj of vc_pred vector
                   |  Disj of vc_pred vector
                   |  Not of vc_pred

  datatype t = T of tydbinds * vc_pred * vc_pred
  
  val assert = Control.assert
  fun $ (f,arg) = f arg
  infixr 5 $
  fun vectorAppend (vec,e) = Vector.concat [vec,Vector.new1 e]
  fun vectorPrepend (e,vec) = Vector.concat [Vector.new1 e,vec]
  fun vectorFoldrFoldr (vec1,vec2,acc,f) = Vector.foldr (vec1,acc,
    fn (el1,acc) => Vector.foldr (vec2,acc,fn (el2,acc) => f (el1,el2,acc)))

  fun conj (p1 : vc_pred,p2 : vc_pred) : vc_pred = case (p1,p2) of 
      (Simple True,_) => p2
    | (_, Simple True) => p1
    | (Simple False,_) => Simple False
    | (_, Simple False) => Simple False
    | (Simple sp1,Conj spv) => Conj $ vectorPrepend (p1,spv)
    | (Conj spv,Simple sp2) => Conj $ vectorAppend (spv,p2)
    | (Conj spv1,Conj spv2) => Conj $ Vector.concat [spv1,spv2]
    | _ => Conj $ Vector.new2 (p1,p2)

  fun disj(p1 : vc_pred,p2 : vc_pred) : vc_pred = case (p1,p2) of 
      (Simple True,_) => Simple True
    | (_, Simple True) => Simple True
    | (Simple False,_) => p2
    | (_, Simple False) => p1
    | (Simple sp1,Disj spv) => Disj $ vectorPrepend (p1,spv)
    | (Disj spv,Simple sp2) => Disj $ vectorAppend (spv,p2)
    | (Disj spv1,Disj spv2) => Disj $ Vector.concat [spv1,spv2]
    | _ => Disj $ Vector.new2 (p1,p2)

  fun negate (p : vc_pred) : vc_pred = case p of
      Simple True => Simple False
    | Simple False => Simple True
    | _ => Not p
  

  fun coercePTtoT (pt:P.t) : vc_pred = case pt of
      P.True => Simple True
    | P.False => Simple False
    | P.Hole h => Simple $ Hole h
    | P.Base p => Simple $ Base p
    | P.Rel p => Simple $ Rel p
    | P.Not p => negate $ coercePTtoT p
    | P.If (p1,p2) => If (coercePTtoT p1, coercePTtoT p2)
    | P.Iff (p1,p2) => Iff (coercePTtoT p1, coercePTtoT p2)
    | P.Conj (p1,p2) => 
        let
          val t1 = coercePTtoT p1
          val t2 = coercePTtoT p2
        in
          conj (t1,t2)
        end
    | P.Disj (p1,p2) => disj (coercePTtoT p1, coercePTtoT p2)
    | _ => Error.bug "Cannot coerce PT to T"

  fun truee () : vc_pred = Simple True
  fun falsee () : vc_pred = Simple False

  (*
   * join-order(vc,vc1,vc2) : binds = binds1@binds2
   *                          envP = envP1 /\ envP2
   *)
  fun joinVCs ((binds1,envP1),(binds2,envP2)) : (tydbinds * vc_pred) =
    (Vector.concat [binds1,binds2],conj (envP1,envP2))

  (*
   * forall vc1 in vcs1 and vc2 in vcs2, vc is in vcs s.t
   * join-order (vc,vc1,vc2)
   *)
  fun join (vcs1,vcs2) = 
    case (Vector.length vcs1, Vector.length vcs2) of 
      (0,_) => vcs2
    | (_,0) => vcs1
    | _ => 
      let
        val vcs = (Vector.fromList o vectorFoldrFoldr) (vcs1,vcs2,[], 
          fn (vc1,vc2,acc) => (joinVCs (vc1,vc2))::acc)
      in
        vcs
      end

  fun havocPred (pred : P.t) : (tydbinds*vc_pred) vector =
    let
      fun trivialAns () = Vector.new1 (Vector.new0(),coercePTtoT pred)
    in
      case pred of
        P.Exists (tyDB,p) => 
          let
            val mybinds = TyDBinds.toVector tyDB
            val vcs = havocPred p
          in
            Vector.map (vcs, fn (binds,envP) =>
              (Vector.concat [mybinds,binds],envP))
          end
      | P.Conj (p1,p2) =>
          let
            val vcs1 = havocPred p1
            val vcs2 = havocPred p2
          in
            (* conj is a join point *)
            join (vcs1,vcs2)
          end
      | P.Dot (p1,p2) => Vector.concat [havocPred p1,
          havocPred p2]
      | _ => trivialAns () (* May need havoc here.*)
    end
        
  fun havocTyBind (v : Var.t,refTy : RefTy.t) : (tydbinds*vc_pred) vector =
    let
      open RefTy
      (* -- These functions duplicated from SpecVerify -- *)
      val newLongVar = fn (var,fld) => Var.fromString $
        (Var.toString var)^"."^(Var.toString fld)
      (*
       * Decomposes single tuple bind of form v ↦ {x0:T0,x1:T1} to
       * multiple binds : [v.x0 ↦ T0, v.x1 ↦ T1]
       *)
      fun decomposeTupleBind (tvar : Var.t, tty as RefTy.Tuple 
        refTyBinds) : (Var.t*RefTy.t) vector =
        let
          val bindss = Vector.map (refTyBinds, 
            fn (refTyBind as (_,refTy)) => 
              case refTy of 
                RefTy.Tuple _ => decomposeTupleBind refTyBind
              | _ => Vector.new1 refTyBind)
          val binds = Vector.map (Vector.concatV bindss, 
            fn (v,ty) => (newLongVar (tvar,v), ty))
        in
          binds
        end
    in
      case refTy of
        (* removing any _mark_ *)
        Base (_,TyD.Tunknown,_) => Vector.new0 ()
      | Base (bv,td,pred) => 
          let
            val pred' = P.applySubst (v,bv) pred
            val vcs = havocPred pred'
            val mybind = (v,td)
          in
            Vector.map (vcs,fn (binds,envP) => 
              (vectorAppend (binds,mybind),envP))
          end
      | Tuple _ => havocTyBindSeq $ decomposeTupleBind (v,refTy)
        (* Bindings for functions not needed *)
      | _ => Vector.new0 ()
    end

  and havocTyBindSeq (tyBinds : (Var.t * RefTy.t) vector)
    : (tydbinds * vc_pred) vector =
    Vector.fold (tyBinds,
      Vector.new1 (Vector.new0 (),truee()),
      fn (tyBind,vcs1) => join (vcs1,havocTyBind tyBind))

  fun havocVE (ve : VE.t) : (tydbinds*vc_pred) vector =
    let
      (*
       * Remove polymorphic functions and constructors
       *)
      val vevec = Vector.keepAllMap (VE.toVector ve,
        fn (v,RefTyS.T{tyvars,refty,...}) => case Vector.length tyvars of
            0 =>  SOME (v,refty)
          | _ => NONE)
      (*
       * Remove true and false constructors
       *)
      val vevec = Vector.keepAllMap (vevec, 
        fn (v,refty) => case refty of
          RefTy.Base (_,TyD.Tconstr (tycon,_),_) => 
            if Tycon.isBool tycon andalso (Var.toString v = "true" 
              orelse Var.toString v = "false") then NONE 
              else SOME (v,refty)
        | _ => SOME (v,refty))
    in
      havocTyBindSeq vevec
    end

  fun fromTypeCheck (ve, subTy, supTy) : t vector = 
    let
      open RefTy
    in
      case (subTy,supTy) of
        (Base (_,TyD.Tunknown,p),_) => if P.isFalse p 
          then raise TrivialVC
          else raise (Fail "ML type of subtype is unknown")
      | (Base (v1,t1,p1), Base (v2,t2,p2)) => 
          let
            (*
             * First, make sure that base types are same.
             *)
            val _ = assert (TyD.sameType (t1,t2), (TyD.toString t1)
              ^" is not sub-type of "^(TyD.toString t2))
            (*
             * Second, substitute actuals for formals in p2
             *)
            val p2 = P.applySubst (v1,v2) p2
            (*
             *val _ = print "AnteP: "
             *val _ = L.print (P.layout p1,print)
             *val _ = print "\n"
             *)
            (*
             * Third, add base type of actuals to env
             *)
            val ve = VE.add ve (v1,RefTyS.generalize (Vector.new0 (),
              RefTy.fromTyD t1))
            val envVCs = fn _ => havocVE ve
            val anteVCs = fn _ => havocPred p1
            val vcs = fn _ => join (envVCs (),anteVCs ())
            val conseqPs = fn _ => case coercePTtoT p2 of
                Conj vcps => vcps | vcp => Vector.new1 (vcp)
          in
            case p2 of P.True => Vector.new0()
              | _ => Vector.fromList $ vectorFoldrFoldr 
                (vcs(), conseqPs(), [],
                  fn ((tybinds,anteP),conseqP,vcacc) =>
                    case anteP of Simple False => vcacc
                    | _ => (T (tybinds,anteP,conseqP))::vcacc)
          end
      | (Tuple t1v,Tuple t2v) => 
          (*
           * Unimpl: records
           *)
          (Vector.concatV o Vector.map2) (t1v,t2v, 
            fn ((v1,t1),(v2,t2)) => fromTypeCheck (ve,t1,t2))
      | (Arrow ((arg1,t11),t12),Arrow ((arg2,t21),t22)) => 
          let
            val vcs1 = fromTypeCheck (ve,t21,t11)
            (*
             * Typecheck results modulo argvar
             *)
            val t12' = RefTy.applySubsts (Vector.new1 (arg2,arg1)) t12
            (*
             * Extend the environment with type for arg2
             *)
            val ve'  = VE.add ve (arg2, RefTyS.generalize 
              (Vector.new0 (), t21))
            val vcs2 = fromTypeCheck (ve',t12',t22)
          in
            Vector.concat [vcs1, vcs2]
          end
    end handle TrivialVC => Vector.new0 ()

  datatype rinst = RInst of RelLang.RelId.t * TypeDesc.t vector

  structure RelInstTable : APPLICATIVE_MAP where
    type Key.t = rinst and type Value.t = RelLang.RelId.t =
  struct
    structure Key = 
    struct
      type t = rinst
      val layout = fn _ => L.empty
      val idStrEq = fn (id1,id2) => (RI.toString id1 = RI.toString id2)
      fun equal (RInst (id1,tyds1), RInst (id2,tyds2)) =
        let
          val eq = (idStrEq (id1,id2)) andalso
              (Vector.length tyds1 = Vector.length tyds2) andalso
              (Vector.forall2 (tyds1,tyds2, TyD.sameType))
        in
          eq
        end
        
    end
    structure Map = ApplicativeMap (structure Key = Key
                                   structure Value = RelLang.RelId)
    open Map
  end

  fun elaborate re (vc,rinstTab) =
    let
      val T (tydbinds,anteP,conseqP) = vc

      val tyDB = Vector.fold (tydbinds,TyDBinds.empty, 
        fn ((v,tyd),tyDB) => TyDBinds.add tyDB v tyd)

      val count = ref 0
      val genSym = fn idbase => 
        let
          val symbase = (*"_"^*)(RI.toString idbase)
          val id = symbase ^ (Int.toString (!count))
          val _ = count := !count + 1
        in
          RI.fromString id 
      end
      val inv = fn (x,y) => (y,x)
      fun mapFoldTuple b f (x,y) =
        ((fn (b',x') => 
            ((fn (b'',y') => (b'',(x',y'))) o (f b')) y) 
          o (f b)) x 
      fun mapSnd f (x,y) = (x,f y)

      fun elabRExpr (tab:RelInstTable.t) rexpr =  
        let
          fun getSymForRInst rinst = 
            (SOME $ RelInstTable.find tab (RInst rinst)) 
              handle RelInstTable.KeyNotFound _ => NONE
          fun addSymForRInst rinst rid =
            RelInstTable.add tab (RInst rinst) rid
          val mapper = mapFoldTuple tab elabRExpr
          fun tyArgsinTypeOf (v:Var.t) =
            (case TyDBinds.find tyDB v of
              TyD.Tconstr (tycon,targs) => Vector.fromList targs
              (* Hack : special case to deal with 'R *)
            | t => Vector.new1 t)
            (*| _ => Error.bug ("Relation instantiated over variable\
              \ of non-algebraic datatype")) *)
            handle TyDBinds.KeyNotFound _ => Error.bug ("Type of\
              \ variable "^(Var.toString v)^" not found in TyDBinds")
        in
          case rexpr of
            RelLang.T _ => (tab,rexpr)
          | RelLang.X t => mapSnd RelLang.X (mapper t)
          | RelLang.U t => mapSnd RelLang.U (mapper t)
          | RelLang.D t => mapSnd RelLang.D (mapper t)
          | RelLang.R (relId,v) => 
            let
              val relIdStr = RelId.toString relId
              val dTyInsts = tyArgsinTypeOf v
              (* Hack : to deal with 'R *)
              val rTyInsts = fn _ => tyArgsinTypeOf $ 
                Var.fromString "l"
              val rinst = case relIdStr = "qRm" orelse 
                relIdStr = "qRo" of
                  false => (relId,dTyInsts)
                | true => (relId, Vector.concat [dTyInsts, 
                    rTyInsts ()])
            in
              case getSymForRInst rinst of 
                SOME relId' => (tab,RelLang.R (relId',v))
              | NONE => (fn r' => (addSymForRInst rinst r', 
                  RelLang.R (r',v))) (genSym relId)
            end
        end

      fun elabRPred (tab : RelInstTable.t) rpred = case rpred of
          RP.Eq t => mapSnd RP.Eq (mapFoldTuple tab elabRExpr t)
        | RP.Sub t => mapSnd RP.Sub (mapFoldTuple tab elabRExpr t)
        | RP.SubEq t => mapSnd RP.SubEq (mapFoldTuple tab elabRExpr t)

      fun elabSimplePred (rinstTab : RelInstTable.t) sp = 
        case sp of
          Rel rpred => mapSnd Rel (elabRPred rinstTab rpred)
        | _ => (rinstTab,sp)

      fun elabVCPred (rinstTab : RelInstTable.t) (vcpred:vc_pred) :
        (RelInstTable.t*vc_pred) = 
        case vcpred of
          Simple sp  => mapSnd Simple (elabSimplePred rinstTab sp)
        | Conj vcps => mapSnd Conj ((inv o Vector.mapAndFold) 
           (vcps, rinstTab, fn (vcp,rt) => inv $ elabVCPred rt vcp))
        | Disj vcps => mapSnd Disj ((inv o Vector.mapAndFold) 
           (vcps, rinstTab, fn (vcp,rt) => inv $ elabVCPred rt vcp))
        | Not vcp => mapSnd Not (elabVCPred rinstTab vcp)
        | If vcps => mapSnd If $ mapFoldTuple rinstTab elabVCPred vcps
        | Iff vcps => mapSnd Iff $ mapFoldTuple rinstTab elabVCPred vcps

      val (rinstTab,anteP') = elabVCPred rinstTab anteP
      val (rinstTab,conseqP') = elabVCPred rinstTab conseqP

      val newtydbinds = Vector.map (RelInstTable.toVector rinstTab,
        fn (RInst (relId,tydvec),relId') =>
          let
            val {ty,map} = RE.find re relId handle RE.RelNotFound _ =>
              Error.bug ("Unknown Relation: "^(RelLang.RelId.toString relId))
            val RelTy.Tuple tydvec = RelTyS.instantiate (ty,tydvec)
            val boolTyD = TyD.makeTconstr (Tycon.bool,[])
            val relArgTyd = TyD.Trecord $ Record.tuple tydvec
            val relTyD = TyD.makeTarrow (relArgTyd,boolTyD)
            val relvid = Var.fromString $ RI.toString relId'
          in
            (relvid,relTyD)
          end)

      val tydbinds' = Vector.concat [tydbinds,newtydbinds]
    in
      (T (tydbinds',anteP',conseqP'), rinstTab)
    end

    fun elaborateAll (re,vcs) = #1 $ Vector.mapAndFold (vcs,
      RelInstTable.empty, (elaborate re))

    fun laytSimplePred sp = case sp of 
        True => L.str "true"
      | False => L.str "false"
      | Hole h => L.str $ Hole.toString h
      | Base bp => L.str $ BP.toString bp
      | Rel rp => L.str $ RP.toString rp

    fun laytVCPred vcpred = case vcpred of 
      Simple p => laytSimplePred p
    | Conj vcv => L.align $ Vector.toListMap (vcv,
        laytVCPred)
    | Disj vcv => L.align $ Vector.toListMap (vcv,
        fn vc => L.seq [L.str "OR [",laytVCPred vc, L.str "]"])
    | Not vc => L.seq [L.str "NOT [", laytVCPred vc, L.str "]"]
    | If (vc1,vc2) => L.seq [laytVCPred vc1, L.str " => ", 
          laytVCPred vc2]
    | Iff (vc1,vc2) => L.seq [laytVCPred vc1, L.str " <=> ", 
          laytVCPred vc2]

  fun layout (vcs : t vector) =
    let
      fun laytTyDBinds tybinds = L.vector (Vector.map (tybinds,
        fn (v,tyd) => L.str ((Var.toString v) ^ " : " ^ 
          (TyD.toString tyd))))

      fun layoutVC (T (tybinds,vcp1,vcp2)) = 
        Pretty.nest ("bindings",laytTyDBinds tybinds,
          L.align [
            L.indent(laytVCPred vcp1,3),
            L.str "=>",
            L.indent (laytVCPred vcp2,3)])
    in
      L.align $ Vector.toListMap (vcs, layoutVC)
    end

  fun layouts (vcs,output) =
    (output $ L.str "Verification Conditions:\n" ; output $ layout vcs)
    
  (* ---- Hole filling ---- *)
  structure  HoleMap: APPLICATIVE_MAP where
    type Key.t = P.Hole.id and type Value.t = RP.t vector =
  struct
    structure Key = 
    struct
      type t = P.Hole.id
      val equal = P.Hole.idEq
      val layout = fn x => L.str $ P.Hole.idToString x
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

  structure HM = HoleMap

  type typed_rel = RI.t * TyD.t 
  type candidate_rels = {unions : typed_rel vector,
                         crossprds : (typed_rel * typed_rel) vector}

  fun synthRPreds rels (hole as P.Hole.T {substs,bv,id,env}) =
    let
      val rels = Vector.map (rels, 
        fn (r,TyD.Tarrow (TyD.Trecord tupRec,_)) =>
          let
            val domTyD::sort= Vector.toList $ Record.range tupRec
          in
            (r,(domTyD,sort))
          end)
      val bvTyD = TyDBinds.find env bv
      fun sortEq (t1,t2) = RelTy.equal (RelTy.Tuple $
        Vector.fromList t1, RelTy.Tuple $ Vector.fromList t2)
      fun notRhd rel = not (String.hasPrefix(RI.toString rel,
        {prefix="Rhd"}))
      val lhsRels = Vector.keepAll (rels, fn (rel,(domTyD,_)) =>
        TyD.sameType (domTyD,bvTyD) andalso (notRhd rel))
      exception CRRet of RI.t * candidate_rels
      val crMap = Vector.map (lhsRels, fn (lhsRel, (_,lhsSort)) =>
        let
          val unions = Vector.keepAllMap (rels, 
            fn (r, (tyd,rhsSort)) => if sortEq (lhsSort,rhsSort) 
              then SOME (r,tyd) else NONE)
          val _ = if not (List.length lhsSort = 2) 
            then raise CRRet (lhsRel, {unions=unions, 
                                       crossprds= Vector.new0 ()})
            else ()
          val [t1,t2] = lhsSort
          val cp1 = Vector.keepAllMap (rels, fn (r,(tyd,rhsSort)) =>
            if sortEq ([t1],rhsSort) then SOME (r,tyd) else NONE)
          val cp2 = Vector.keepAllMap (rels, fn (r,(tyd,rhsSort)) =>
            if sortEq ([t2],rhsSort) then SOME (r,tyd) else NONE)
          val crossprds = vectorFoldrFoldr (cp1,cp2,[],
            fn (el1,el2,acc) => (el1,el2)::acc)
        in
          (lhsRel, {unions = unions, 
                    crossprds = Vector.fromList crossprds})
        end handle CRRet x => x)
        (*
         * The straightforard grammar is:
         *  v ::= x \in dom(env) and x \neq bv
         *  R1 \in unions
         *  R2 \in crossprds
         *  expr ::= R1(v) | cp | expr U expr
         *  cp ::= R2(v) X R2(v)
         * We make it left-associative so that we only have one
         * recursive call:
         *  expr ::= R1(v) U expr | cp U expr | {}
         *  cp ::= R2(v) X R2(v)
         *)
      val varbinds = TyDBinds.toVector $ TyDBinds.remove env bv
      fun getVarsOfType tyd = Vector.keepAllMap (varbinds, 
        fn (v,tyd') => if TyD.sameType (tyd,tyd') then SOME v else NONE) 
      fun getValidAppExprs (r,tyd) = Vector.map (getVarsOfType tyd, 
        fn v => RelLang.app (r,v))
      fun getValidCPExprs ((r1,t1),(r2,t2)) =
        let
          val r1apps = getValidAppExprs (r1,t1)
          val r2apps = getValidAppExprs (r2,t2)
        in
          Vector.fromList $ vectorFoldrFoldr (r1apps,r2apps,[], 
            fn (r1app,r2app,acc) =>
              (RelLang.crossprd (r1app,r2app)) :: acc)
        end
      fun synthRExpr {unions,crossprds} =
        let
          val appDisjuncts = Vector.concatV $ 
            Vector.map (unions, getValidAppExprs)
          val cpDisjuncts = Vector.concatV $
            Vector.map (crossprds, getValidCPExprs)
          val disjuncts = Vector.concat [appDisjuncts, cpDisjuncts]
          val unionExpr = Vector.foldr (disjuncts, 
            RelLang.emptyexpr (), fn (atom,expr) =>
              RelLang.union (atom,expr))
        in
          unionExpr
        end
      fun synthRPred (lhsRel,cands) = RP.Eq (RelLang.app (lhsRel,bv),
        synthRExpr cands)
    in
      Vector.map (crMap, synthRPred)
    end

  fun fillHolesInVC (T (tydbinds, anteP, conseqP)) hm = 
    let
      val rels = Vector.keepAllMap (tydbinds,
        fn (v,tyd as TyD.Tarrow _) => SOME (RI.fromString $
            Var.toString v, tyd)
          | _ => NONE)
      fun doItHole hole hm = 
        let
          val holeId = P.Hole.idOf hole
        in
          case HM.mem hm holeId of 
              true => hm
            | false => HM.add hm holeId (synthRPreds rels hole)
        end
      fun doIt vcp hm = case vcp of 
          Simple (Hole h) => doItHole h hm
        | Simple _ => hm
        | If (vcp1,vcp2) => doIt vcp2 (doIt vcp1 hm)
        | Iff (vcp1,vcp2) => doIt vcp2 (doIt vcp1 hm)
        | Conj vcps => Vector.fold(vcps,hm, 
          fn (vcp,hm') => doIt vcp hm')
        | Disj vcps => Vector.fold(vcps,hm, 
          fn (vcp,hm') => doIt vcp hm')
        | Not vcp => doIt vcp hm
    in
      doIt conseqP (doIt anteP hm)
    end

  fun fillHoles vcs = Vector.fold (vcs, HM.empty, fn (vc,hm) =>
    fillHolesInVC vc hm)
    
end

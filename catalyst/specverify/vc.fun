functor VerificationCondition (S : VERIFICATION_CONDITION_STRUCTS)
  : VERIFICATION_CONDITION =
struct
  open S
  structure P = Predicate
  structure BP = Predicate.BasePredicate
  structure RP = Predicate.RelPredicate
  structure RelId = RelId
  structure TS = TupSort
  structure PR = PrimitiveRelation
  structure SPS = SimpleProjSort
  structure PSS = ProjSortScheme
  structure PTS = ProjTypeScheme
  structure RefTy = RefinementType
  structure PRf = ParamRefType
  structure RefSS = RefinementSortScheme
  structure RefTyS = RefinementTypeScheme
  structure RI = RelId
  structure TyD = TypeDesc
  structure Env = TyDBinds
  structure L = Layout

  exception TrivialVC (* raised when antecedent has false *)

  type tydbind = Var.t * TyD.t
  type tydbinds = tydbind vector
  type bindings = {tbinds:tydbinds, rbinds:PRE.t}

  datatype simple_pred = True
                       | False
                       | Base of BP.t 
                       | Rel of RP.t

  datatype vc_pred =  Simple of simple_pred
                   |  If of vc_pred * vc_pred
                   |  Iff of vc_pred * vc_pred
                   |  Conj of vc_pred vector
                   |  Disj of vc_pred vector
                   |  Not of vc_pred

  datatype t = T of bindings * vc_pred * vc_pred
  
  val assert = Control.assert
  val empty = fn _ => Vector.new0 ()
  val len = Vector.length
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
      val decomposeTupleBind = RefTy.decomposeTupleBind
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
        fn (v,RefTyS.T{tyvars, refss, ...}) => 
          case Vector.length tyvars of
              0 =>  SOME (v,RefSS.toRefTy refss)
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

  fun fromTypeCheck (ve, pre, subTy, supTy) : t vector = 
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
              RefSS.fromRefTy $ RefTy.fromTyD t1))
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
                    | _ => (T ({tbinds=tybinds, rbinds=pre}, 
                        anteP, conseqP))::vcacc)
          end
      | (Tuple t1v,Tuple t2v) => 
          (*
           * Unimpl: records
           *)
          (Vector.concatV o Vector.map2) (t1v,t2v, 
            fn ((v1,t1),(v2,t2)) => fromTypeCheck (ve,pre,t1,t2))
      | (Arrow ((arg1,t11),t12),Arrow ((arg2,t21),t22)) => 
          let
            val vcs1 = fromTypeCheck (ve,pre,t21,t11)
            (*
             * Typecheck results modulo argvar
             *)
            val t12' = RefTy.applySubsts (Vector.new1 (arg2,arg1)) t12
            (*
             * Extend the environment with type for arg2
             *)
            val ve'  = VE.add ve (arg2, RefTyS.generalize 
              (Vector.new0 (), RefSS.fromRefTy t21))
            val vcs2 = fromTypeCheck (ve',pre,t12',t22)
          in
            Vector.concat [vcs1, vcs2]
          end
    end handle TrivialVC => Vector.new0 ()

  (* -- VC Elaboration -- *)
  (*
   * This rinst has relIds as its arguments.
   *)
  datatype rinst = RInst of {targs : TypeDesc.t vector,
                             sargs : TS.t vector,
                             rargs : RelId.t vector,
                             rel : RelId.t}
  type rsorted = {rel:RelId.t, sort : SPS.t}

  structure RelInstTable : APPLICATIVE_MAP where
    type Key.t = rinst and type Value.t = rsorted =
  struct
    structure Key = 
    struct
      type t = rinst
      val len = Vector.length
      val layout = fn (RInst {targs,sargs,rargs,rel}) => L.str $ 
        RelLang.ieToString $ RelLang.RInst {targs=targs, sargs=sargs,
          args=Vector.map (rargs,RelLang.instOfRel), rel=rel}
      val idStrEq = fn (id1,id2) => (RI.toString id1 = RI.toString id2)
      fun equal (RInst {rel=id1, targs=targs1, 
                        sargs=sargs1, rargs=rargs1}, 
                 RInst {rel=id2, targs=targs2, 
                        sargs=sargs2, rargs=rargs2}) =
        let
          val eq = (RI.equal (id1,id2)) andalso
              (len targs1 = len targs2) andalso
              (Vector.forall2 (targs2,targs1, TyD.sameType)) andalso
              (len sargs1 = len sargs2) andalso
              (Vector.forall2 (sargs2,sargs1, TS.equal)) andalso
              (len sargs1 = len sargs2) andalso
              (Vector.forall2 (sargs2,sargs1, TS.equal)) andalso
              (len rargs1 = len rargs2) andalso
              (Vector.forall2 (rargs2,rargs1, RI.equal))
        in
          eq
        end
        
    end
    structure Value =
    struct
      type t = rsorted
      fun layout {rel,sort} = L.str $ (RI.toString rel) ^"::" 
        ^(SPS.toString sort) 
    end
    structure Map = ApplicativeMap (structure Key = Key
                                   structure Value = Value)
    open Map
  end

  structure RIT = RelInstTable

  fun elaborate (re,pre,vc) =
    let
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
      val fst = fn (x,y) => x
      val snd = fn (x,y) => y
      fun mapFoldTuple b f (x,y) =
        ((fn (b',x') => 
            ((fn (b'',y') => (b'',(x',y'))) o (f b')) y) 
          o (f b)) x 
      fun mapSnd f (x,y) = (x,f y)

      val T ({tbinds=tydbinds, rbinds},anteP,conseqP) = vc

      val tyDB = Vector.fold (tydbinds,TyDBinds.empty, 
        fn ((v,tyd),tyDB) => TyDBinds.add tyDB v tyd)
      (*
       * initRIT contains params from typespecs. Variable rbinds is a
       * PRE and not RE, as it lets us use same VC.t to represent both
       * high-level and elaborated VCs.
       *)
      val initRIT = Vector.fold (PRE.toVector rbinds, RIT.empty, 
        fn ((r,{ty,...}),rit) =>
          let
            val PTS.T {sortscheme=PSS.T {sort = ProjSort.T {sort, 
              ...} , ...}, ...} = ty
            val value = {rel=r,sort=sort}
            val key = RInst {rel=r, targs=empty(), sargs=empty(),
              rargs=empty()}
          in
            (*
             * A rel-param is its own instantiation
             *)
            RIT.add rit key value
          end)

      fun getSymForRInst rit rinst = 
        (SOME $ #rel $ RIT.find rit rinst) 
          handle RIT.KeyNotFound _ => NONE

      exception Return of (RIT.t * RI.t)
      fun doItPRInst (ie as RelLang.RInst {targs,sargs,args,rel}) rit
          : (RIT.t * RI.t) = 
        let
          val rinst = RInst {rel=rel, targs=targs, sargs=sargs,
            rargs = Vector.map (args, 
              fn (RelLang.RInst {rel, ...}) => rel)}
          val _ = case getSymForRInst rit rinst of NONE => ()
            | SOME rel' => raise (Return (rit,rel'))
          val rel' = genSym rel
          val err = fn _ => Error.bug $ 
            "Unknown prim rel "^(RI.toString rel)
          val relTyS = (#ty $ PRE.find pre rel)
          val PSS.T {sort, ...} = PTS.instantiate (relTyS,targs)
          val ProjSort.T {sort=sps, ...} = sort
          val SPS.ColonArrow (prD,prR) = sps
          fun rangeOf (TyD.Tarrow (_,tyd)) = rangeOf tyd
            | rangeOf tyd = tyd
          val rinstSort = SPS.ColonArrow (rangeOf prD,prR)
          val rit' = RIT.add rit rinst {rel=rel',sort=rinstSort}
        in
          (rit',rel')
        end handle (Return a) => a

      fun doItIE (ie as RelLang.RInst {targs,sargs,args,rel}) rit 
          : (RIT.t * RI.t) =
        let
          val len = Vector.length
          val _ = (case #def $ PRE.find pre rel of 
              PRE.Prim _ => raise (Return $ doItPRInst ie rit)
            | _ => ()) handle PRE.ParamRelNotFound _ => ()
          val (syms,rit') = Vector.mapAndFold (args,rit,
            fn (ie,rit) => inv $ doItIE ie rit) 
          val rinst = RInst {targs=targs, sargs=sargs, rargs=syms,
              rel=rel}
          (*
           * Note: When rel is a rel-param, then syms is an empty
           * vector. Map rit, which is derived from initRIT, already
           * maps rel-params (from rbinds) to themselves.
           *)
          val _ = case getSymForRInst rit' rinst of NONE => ()
            (* If rel-param, always returns *)
            | SOME rel' => raise (Return (rit',rel')) 
          val rel' = genSym rel
          val err = fn _ => Error.bug $ 
            "Unknown rel "^(RI.toString rel)
          (* Obs : Pulling #ty out of case fails typecheck *)
          val relTyS = ((case len args of 
            0 => (#ty $ RE.find re rel) 
          | _ => (#ty $ PRE.find pre rel)) handle 
            PRE.ParamRelNotFound _ => err() ) handle 
            RE.RelNotFound _ => err()
          val relSS = PTS.instantiate (relTyS,targs)
          val ProjSort.T {sort, ...} = PSS.instantiate (relSS,sargs)
          val rit'' = RIT.add rit' rinst {rel=rel',sort=sort}
        in
          (rit'',rel')
        end handle (Return a) => a

      fun doItRExpr (rit:RelInstTable.t) rexpr
        : (RIT.t * RelLang.expr) =
        let
          val g = mapFoldTuple rit doItRExpr
        in
          case rexpr of
            RelLang.T _ => (rit,rexpr)
          | RelLang.X t => mapSnd RelLang.X (g t)
          | RelLang.U t => mapSnd RelLang.U (g t)
          | RelLang.D t => mapSnd RelLang.D (g t)
          | RelLang.R (ie,v) => 
            let
              val (rit',newRel) = doItIE ie rit
              (*
               * Obs: We would ideally like to have a modified
               * instexpr type which is just RI.t. However, this
               * requires entirely new SpecLang. 
               *)
              val ie' = RelLang.RInst {targs=empty(), sargs=empty(),
                args=empty(), rel=newRel}
            in
              (rit', RelLang.R (ie',v))
            end
        end

      fun elabRPred (tab : RelInstTable.t) rpred = case rpred of
          RP.Eq t => mapSnd RP.Eq (mapFoldTuple tab doItRExpr t)
        | RP.Sub t => mapSnd RP.Sub (mapFoldTuple tab doItRExpr t)
        | RP.SubEq t => mapSnd RP.SubEq (mapFoldTuple tab doItRExpr t)

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

      val (rinstTab,anteP') = elabVCPred initRIT anteP
      val (rinstTab,conseqP') = elabVCPred rinstTab conseqP

      (*
       * Encode svars as tyvars.
       *)
      exception SVarNotFound
      val sMap = HashTable.mkTable (fn v => HashString.hashString $
        SVar.toString v, SVar.eq) (117, SVarNotFound)
      val encodeSVar = fn v => case HashTable.find sMap v of
          SOME tyvar => tyvar
        | NONE => 
          let
            val bogus = SourcePos.bogus
            val tyvar = TyD.makeTvar $ Tyvar.newString 
              (SVar.toString v, {left=bogus, right=bogus})
            val _ = HashTable.insert sMap (v,tyvar)
          in
            tyvar
          end 
      val newtydbinds = Vector.map (RelInstTable.toVector rinstTab,
        fn (_,{rel=relId',sort}) =>
          let
            val SPS.ColonArrow (tyd,TS.Tuple tts) = sort
            val tydvec = Vector.fromList $ tyd :: (List.map (tts, 
              fn tts => case tts of TS.T tyd => tyd 
              | TS.S t =>  encodeSVar t))
            val boolTyD = TyD.makeTconstr (Tycon.bool,[])
            val relArgTyd = TyD.Trecord $ Record.tuple tydvec
            val relTyD = TyD.makeTarrow (relArgTyd,boolTyD)
            val rtov = Var.fromString o RelId.toString
            val relvid = rtov relId'
          in
            (relvid,relTyD)
          end)

      (* --- new tydbinds generation done --- *)

      fun instPrimRelDef def (RInst {rargs,rel, ...}) =
        let
          val rtov = Var.fromString o RelId.toString
          val argVars = Vector.map (rargs, rtov)
          val def' = PR.instantiate (def, argVars)
        in
          def'
        end

      fun instParamRelDef def (RInst {targs,sargs,rargs,rel}) =
        let
          val abs = Bind.instantiate (def, targs, rargs)
          val Bind.Abs (bv,expr) = abs
          val Bind.Expr {ground=(grel,gtargs,_), fr} = expr
          val grinst = RInst {targs=gtargs, sargs=empty(),
            rargs=empty(), rel=grel}
          val {rel=grAlias, ...} = RIT.find rinstTab grinst handle
            RIT.KeyNotFound _ => Error.bug "GRel Inst not found"
          val expr' = Bind.Expr {ground=(grAlias,empty(),bv), fr=fr}
          val abs' = Bind.Abs (bv,expr')
          val def' = Bind.fromAbs abs'
        in
          def'
        end

      exception Return of PRE.t
      (*
       * newPre contains instantiated definitions of instantiated
       * parametric/primitive relations.
       * rinstTab was already processed to generate newtydbinds.
       * Therefore, type declarations of all relations in newPre are
       * present in tbinds of elaborated VC.t.
       *)
      val newPre = Vector.fold (RIT.toVector rinstTab, PRE.empty,
        fn ((rinst, {rel=newRel,sort}),newPre) =>
          let
            val RInst {rel, rargs, ...} = rinst
            val {def,...} = PRE.find pre rel handle
              (*
               * r \in domain(rinstTab) /\ r \notin domain(pre) =>
               * r is a rel-param
               *)
              PRE.ParamRelNotFound _ => raise (Return newPre)
            val def'  = case (def, len rargs) of 
              (PRE.Prim pdef, _) => PRE.Prim $ instPrimRelDef pdef rinst
            | (PRE.Bind bdef, 0) => raise (Return newPre)
            | (PRE.Bind bdef, _) => PRE.Bind $ instParamRelDef bdef rinst
          in
            PRE.add newPre (newRel,{ty=PTS.simple (empty(),sort), 
              def=def'})
          end handle Return newPre => newPre)

      val tydbinds' = Vector.concat [tydbinds,newtydbinds]
      val bindings = {tbinds=tydbinds', rbinds=newPre}
      (*
      val _ = print "RelInstTable : \n"
      val _ = Control.message (Control.Top, fn _ =>
        RIT.layout rinstTab)
      *)
    in
      T (bindings,anteP',conseqP')
    end

  fun layout (vcs : t vector) =
    let
      fun laytTyDBinds tybinds = L.vector (Vector.map (tybinds,
        fn (v,tyd) => L.str ((Var.toString v) ^ " : " ^ 
          (TyD.toString tyd))))

      fun laytSimplePred sp = case sp of 
          True => L.str "true"
        | False => L.str "false"
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

      fun layoutVC (T ({tbinds=tybinds, rbinds=pre},vcp1,vcp2)) = 
        Pretty.nest ("bindings",
          L.align[
            laytTyDBinds tybinds,
            PRE.layout pre],
          L.align [
            L.indent(laytVCPred vcp1,3),
            L.str "=>",
            L.indent (laytVCPred vcp2,3)])
    in
      L.align $ Vector.toListMap (vcs, layoutVC)
    end

  fun layouts (vcs,output) =
    (output $ L.str "Verification Conditions:\n" ; output $ layout vcs)
    
end

functor ElaborateVarEnv (S : ELABORATE_VAR_ENV_STRUCTS) : ELABORATE_VAR_ENV = 
struct
  open S
  open SpecLang
  open ANormalCoreML
  structure VE = VarEnv (structure Var = Var
                         structure SpecLang = SpecLang)
  structure RE = RelEnv (structure SpecLang = SpecLang)
  structure TyD = TypeDesc
  structure TyDB = TyDBinds
  structure RelId = RelLang.RelId
  structure RelTy = RelLang.RelType
  structure RelTyS = RelLang.RelTypeScheme
  structure RefTy = RefinementType
  structure RefTyS = RefinementTypeScheme
  structure P = Predicate
  structure RP = P.RelPredicate
  structure BP = P.BasePredicate
  structure TypeSpec = RelSpec.TypeSpec
  structure L = Layout

  val assert = Control.assert
  fun $ (f,arg) = f arg
  infixr 5 $

  fun bootStrapFreeRels (re : RE.t) =
    let
      val splRelId1 = RelId.fromString "qRm"
      val splRelId2 = RelId.fromString "qRo"
      val domainTyvar = Tyvar.newNoname {equality = false}
      val rangeTyvar = Tyvar.newNoname {equality = false}
      val relTy1 = RelTy.Tuple $ Vector.new2 
        (TyD.makeTvar domainTyvar, TyD.makeTvar rangeTyvar)
      val relTyS1 = RelTyS.generalize (Vector.new2 (domainTyvar,
        rangeTyvar), relTy1)
      val map = Vector.new0 ()
      val re = RE.add re (splRelId1, {ty=relTyS1, map=map})
      val relTy2 = RelTy.Tuple $ Vector.new3 
        (TyD.makeTvar domainTyvar, TyD.makeTvar rangeTyvar,
         TyD.makeTvar rangeTyvar)
      val relTyS2 = RelTyS.generalize (Vector.new2 (domainTyvar,
        rangeTyvar), relTy2)
      val re = RE.add re (splRelId2, {ty=relTyS2, map=map})
      (* RId *)
      val idRelId = RelId.fromString "RId"
      val tyvar = Tyvar.newNoname {equality=false}
      val idRelTy = RelTy.Tuple $ Vector.new2 
        (TyD.makeTvar tyvar, TyD.makeTvar tyvar)
      val idRelTyS = RelTyS.generalize (Vector.new1 tyvar, idRelTy)
    in
      RE.add re (idRelId, {ty=idRelTyS, map=map})
    end

  fun bootStrapBools (ve: VE.t) = 
    let
      val boolTyD = TyD.makeTconstr (Tycon.bool,[])
      val tvid = Var.fromString $ Con.toString Con.truee
      val fvid = Var.fromString $ Con.toString Con.falsee
      val RefTy.Base (v,t,_) = RefTy.fromTyD boolTyD
      val eqPred1 = P.baseP $ BP.varBoolEq (v,true)
      val eqPred2 = P.baseP $ BP.varBoolEq (v,false)
      val empty = Vector.new0 ()
      val tTyS = RefTyS.generalize (empty, RefTy.Base (v,t,eqPred1))
      val fTyS = RefTyS.generalize (empty, RefTy.Base (v,t,eqPred2))
      val ve' = VE.add ve (tvid,tTyS)
      val ve'' = VE.add ve' (fvid,fTyS)
    in
      ve''
    end

  fun elabDatBind (ve : VE.t) {cons,tyvars,tycon} =
    let
      val destTyD = TyD.makeTconstr (tycon, 
        Vector.toListMap (tyvars, TyD.makeTvar))
      val elabCon = fn ({arg,con},ve) =>
        let
          val vid = Var.fromString (Con.toString con)
          val conTyD = case arg of 
              NONE => destTyD
            | SOME argTy => TyD.makeTarrow (Type.toMyType argTy, destTyD)
          val conTyS = RefTyS.generalize (tyvars, RefTy.fromTyD conTyD)
        in
          VE.add ve (vid,conTyS)
        end
    in
      Vector.fold (cons, ve, elabCon)
    end

  (* -- Function duplicated from SpecVerify -- *)
  val newLongVar = fn (var,fld) => Var.fromString $
    (Var.toString var)^"."^(Var.toString fld)

  fun unifyConArgs (ve : VE.t) (con : Con.t) (vars : Var.t vector) =
    let
      val conStr = Con.toString con
      val convid = Var.fromString conStr
      val lenstr = fn v => (Int.toString o Vector.length) v
      val conTy = RefTyS.specialize (VE.find ve convid)
        handle (VE.VarNotFound v) => Error.bug ("Could not find constructor "
          ^ conStr  ^ " in varenv\n")
      open RefTy
    in
      case conTy of 
        Base _ => (assert(Vector.isEmpty vars, 
          "Nullary constructor "^conStr^" applied to arguments"); 
          Vector.fromList [])
      | Arrow ((argv,Base (_,argTyD,_)),Base (_,datTyD,_)) => 
          (assert (Vector.length vars = 1, 
          conStr ^ " expects 1 arg. " ^ (lenstr vars) ^ " given");
          Vector.map (vars,fn (var) => 
            (argv, var, argTyD, TyD.sameType (argTyD,datTyD))))
        (*
         * We do not consider nested tuples in constructor
         * args as yet. Our syntax doesn't allow it.
         *)
      | Arrow ((argv,Tuple tv), Base (_,datTyD,_)) =>
          (assert (Vector.length tv = Vector.length vars,
          conStr ^ " expects "^ (lenstr tv) ^" args. " 
            ^ (lenstr vars) ^ " given");
         Vector.map2 (tv,vars,fn ((fldv, Base (_,argTyD,_)), var) =>
            (newLongVar (argv,fldv), var, argTyD, 
              TyD.sameType (argTyD,datTyD))))
      | _ => raise (Fail "Could not unify and determine rec args")
    end

  fun addRelToConTy (ve: VE.t) (con,valop,rexpr) (id:RelLang.RelId.t) =
    let
      val convid = Var.fromString (Con.toString con)
      val substs = case valop of NONE => Vector.fromList []
        | SOME vals => Vector.map (unifyConArgs ve con vals, 
          fn (cvar,var,_,_) => (cvar,var))
      val rexpr' = RelLang.applySubsts substs rexpr
      val newref = fn var => RP.Eq (RelLang.app(id,var),rexpr')
      val RefTyS.T {tyvars,refty,...} = VE.find ve convid
        handle (VE.VarNotFound v) => Error.bug ("Could not find constructor "
          ^ (Var.toString convid) ^ " in varenv\n")
      val annotConTy = case refty of
          RefTy.Base (bv,tyd,pred) => RefTy.Base (bv,tyd, 
            Predicate.conjR (pred,newref bv))
        | RefTy.Arrow (arg,RefTy.Base (bv,tyd,pred)) => RefTy.Arrow(arg,
            RefTy.Base (bv,tyd, Predicate.conjR (pred,newref bv)))
        | _ => raise (Fail "Constructor type is neither base not arrow")
      val newTyS = RefTyS.generalize (tyvars,annotConTy)
    in
      VE.add (VE.remove ve convid) (convid,newTyS)
    end

  (*
   * Synthesizes the type of rexpr in the given relational env.
   * Rel Env is constructed during elaboration, hence this function
   * is also part of elaboration.
   *)
  exception CantInferType
  fun typeSynthRExpr (re,tyDB,rexpr) : RelLang.RelType.t =
    let
      open RelLang
      open RelLang.RelType
      val typeSynthRExpr = fn expr => typeSynthRExpr (re,tyDB,expr)
      fun typeSynthRElem elem = case elem of
          Int i => TyD.makeTconstr (Tycon.intInf,[])
        | Bool b => TyD.makeTconstr (Tycon.bool,[])
        | Var v => TyDBinds.find tyDB v 
    in
      case rexpr of 
        T elemvec => Tuple $ Vector.map (elemvec, typeSynthRElem)
      | X (e1,e2) => crossPrdType (typeSynthRExpr e1, typeSynthRExpr e2)
      | U (e1,e2) => unionType (typeSynthRExpr e1, typeSynthRExpr e2)
      | D (e1,e2) => unionType (typeSynthRExpr e1, typeSynthRExpr e2)
      | R (relId,arg) => 
        let
          val relName = RelId.toString relId
          val argTy = TyDBinds.find tyDB arg
          val tyds = Vector.fromList (case argTy of 
              TyD.Tconstr (_,tyds) => tyds
            | _ => Error.bug "Instantiating relation on a variable\
              \ of non-algebraic datatype")
          val {ty = relTyS,...} = RE.find re relId handle 
            RE.RelNotFound _ => raise CantInferType 
          val Tuple formalTyDs = RelTyS.instantiate (relTyS,tyds) 
          val _ = assert (TyD.sameType (argTy, Vector.sub 
            (formalTyDs,0)), "Type of formal and actual arguments \
              \does not match for relation "^relName)
          val instType = Tuple $ Vector.dropPrefix (formalTyDs,1)
        in
          instType
        end
    end

  fun elabSRBind (re: RE.t)(ve : VE.t) {id,map} =
    let
      val map' = (Vector.concatV o Vector.map) (map, fn (con,valop,rterm) =>
        case rterm of 
          RelLang.Expr rexpr => Vector.fromList [(con,valop,rexpr)]
        | RelLang.Star relId => 
          let
            val {ty,map} = RE.find re relId 
              handle (RE.RelNotFound r) => raise (Fail 
                ("Ind of unknown relation : "^(RelLang.RelId.toString r)))
          in
            Vector.map (map, fn (con,valop,rexpr) => case valop of 
                NONE => (con,valop,rexpr)
              | SOME vars => 
                let
                  val recvars = Vector.map ((#yes o Vector.partition) 
                    (unifyConArgs ve con vars, fn (_,_,_,isrec) => isrec),
                      fn (cvar,_,_,_) => cvar)
                  val recRApps = Vector.map (recvars, fn var => 
                    RelLang.app (id,var))
                  val recRAppsUnion = Vector.fold (recRApps,
                    RelLang.emptyexpr(), RelLang.union)
                  val rexpr' = RelLang.union (rexpr, recRAppsUnion)
                in
                  (con,valop,rexpr')
                end)
          end)
      val relTySOp = Vector.fold (map, NONE, fn ((con,valop,rterm),relTySOp) => 
        case rterm of 
          RelLang.Expr rexpr => (case valop of NONE => relTySOp 
            | SOME vars => 
            let
              val convid = Var.fromString (Con.toString con)
              val RefTyS.T {tyvars,refty,...} = VE.find ve convid handle
                VE.VarNotFound _ => Error.bug ("Constructor " ^
                  (Con.toString con) ^ " not found in var env.")
              val datTyD = case refty of RefTy.Base (_,datTyD,_) => datTyD
                | RefTy.Arrow (_,RefTy.Base (_,datTyD,_)) => datTyD
                | _ => raise (Fail "Impossible case")
              val tyDB = Vector.fold (unifyConArgs ve con vars, TyDBinds.empty,
                fn ((_,var,tyD,_),tyDB) => TyDBinds.add tyDB var tyD)
              val rexprTyD = typeSynthRExpr (re,tyDB,rexpr)
              val relTyD = RelTy.crossPrdType (RelTy.Tuple $ 
                Vector.new1 datTyD, rexprTyD)
              val relTyS = RelTyS.generalize (tyvars,relTyD)
              val relTyS = case relTySOp of NONE => relTyS
                (*
                 * Type of current rexpr should be union-compatible
                 * with type of other rexprs of this relation.
                 *)
                | SOME relTyS' => RelTyS.unionTypeScheme 
                  (relTyS',relTyS)
            in
              SOME relTyS
            end handle CantInferType => relTySOp)
        | RelLang.Star relId => 
          let
            val {ty,...} = RE.find re relId 
              handle (RE.RelNotFound r) => raise (Fail 
                ("Ind of unknown relation : "^(RelLang.RelId.toString r)))
          in
            (* types of inductive and simple versions match *)
            SOME ty
          end)
      val ty' = case relTySOp of NONE => raise CantInferType
        | SOME relTyS => relTyS
    in
      RE.add re (id,{ty=ty',map=map'})
    end

  fun elabHolesInRefTy (refTy : RefTy.t) : RefTy.t = 
    let
      fun doItPhi (bv:Var.t) tyDB phi = 
        let
          fun doItTup (p1,p2) =
              (doItPhi bv tyDB p1, doItPhi bv tyDB p2)
        in
           case phi of
            P.Conj x => P.Conj $ doItTup x | P.Disj x => P.Disj $ doItTup x
          | P.If x => P.If $ doItTup x | P.Iff x => P.Iff $ doItTup x
          | P.Not t => P.Not $ doItPhi bv tyDB t
          | P.Hole hole => P.Hole $ P.Hole.make ([], bv, 
              P.Hole.idOf hole, tyDB)
          | _ => phi
        end

      fun doItRefTy tyDB refty = case refty of 
        RefTy.Base (bv,tyd,phi) => 
          let
            val phi'= doItPhi bv (TyDB.add tyDB bv tyd) phi
          in
            RefTy.Base (bv,tyd,phi')
          end 
      | RefTy.Arrow ((x,t1),t2) => 
        let
          val t1' = doItRefTy tyDB t1
          val tybinds = case t1 of
            RefTy.Tuple _ => RefTy.decomposeTupleBind (x,t1)
          | _ => Vector.new1 (x,t1)
          val tydbinds = Vector.map (tybinds, fn (v,ty) =>
            (v,RefTy.toTyD ty))
          val tyDB' = Vector.fold (tydbinds, tyDB, 
            fn ((x,tyd),tyDB) => TyDB.add tyDB x tyd)
          val t2' = doItRefTy tyDB' t2
        in
          RefTy.Arrow ((x,t1'),t2')
        end
      | RefTy.Tuple vts =>  RefTy.Tuple $ 
          Vector.map (vts, fn (v,t) => (v,doItRefTy tyDB t))
    in
      doItRefTy TyDBinds.empty refTy
    end

  (*
   * Produces a refTy' with base types taken from TyD and
   * refinements from refTy, given that user-provided base
   * types in refTy are unifiable with base types in tyd
   * Caution : tyvar unification is not uniform. 
   *)
  fun mergeTypes (tyd : TyD.t, refTy : RefTy.t) : RefTy.t =
    let
      open TyD
      open RefTy
      fun isTupleTyD (row : 'a Record.t) = 
        (*
         * SML tuples are records with consecutive numeric
         * fields starting with 1.
         *)
        let 
          val lbltydv = Record.toVector row 
        in
          case Vector.length lbltydv of 0 => true 
          | _ => Vector.foralli (lbltydv, fn (i,(lbl,_)) => 
              Field.toString lbl = Int.toString (i+1))
        end
      fun mergeErrorMsg (tyd,tyd') = "Cannot merge ML type " ^ (TyD.toString tyd)
        ^ " with type given in spec: " ^ (TyD.toString tyd')
      fun doMerge (tyd:TyD.t) (argBind as (argv: Var.t, refTy:RefTy.t)) = 
        case (tyd,refTy) of
          (_,Base (bv,tyd',pred)) => 
            let
              val _ = assert (unifiable (tyd,tyd'),
                mergeErrorMsg(tyd,tyd'))
              val newArgBind = (argv, Base (bv,tyd,pred))
            in
              (Vector.new0 (), newArgBind)
            end 
        | (Trecord tydr, Tuple argBinds') => (case isTupleTyD tydr of
            true =>
              let
                val (substss,newArgBinds') = (Vector.unzip o Vector.map2) 
                  (Record.toVector tydr, argBinds', 
                  fn ((lbl,tyd'),argBind' as (argv',refty')) => 
                    let
                      (*
                       * Argvar for tuple fields used in spec should be 
                       * substituted with field label.
                       *)
                      val newargv' = Var.fromString $ Field.toString lbl
                      val (substs,newArgBind) = doMerge tyd' (newargv',refty')
                      val substs = Vector.concat [Vector.new1 (newargv',argv'), 
                        substs]
                    in
                      (substs,newArgBind)
                    end)
                val substs = Vector.map (Vector.concatV substss,
                  fn (n,ol) =>  (newLongVar (argv,n), ol))
                val newArgBind = (argv, Tuple newArgBinds')
              in
                (substs, newArgBind)
              end
          | false => raise (Fail "Unimpl"))
        | (Tarrow (tyd1,tyd2), Arrow (argBind,resTy)) => 
            let
              val (substs,argBind') = doMerge tyd1 argBind
              val dummyArgVar = argv
              val (_,(_,resTy')) = doMerge tyd2 (dummyArgVar, 
                RefTy.applySubsts substs resTy)
              val newArgBind = (argv, Arrow (argBind',resTy'))
            in
              (Vector.new0 (), newArgBind)
            end
        | _ => Error.bug ("Types Merge Error. Cannot merge\n"
          ^ "1. "^(L.toString $ RefTy.layout refTy)^", \n"
          ^ "2. "^(TyD.toString tyd)^"\n")
      val dummyArgVar = Var.fromString "dummy"
      val (_,(_,refTy')) = doMerge tyd (dummyArgVar, refTy)
    in
      refTy'
    end


  (*
   * Forall top-level fun decs, elabDecs annotates their ML
   * types with type refinements 
   *)
  fun elabDecs (ve :VE.t, decs : Dec.t vector) : VE.t =
    let
      fun elabRecDecs (ve : VE.t) (tyvars : Tyvar.t vector)  decs = 
        Vector.fold (decs,ve, fn ({lambda : Lambda.t, var : Var.t}, ve) =>
          let
            val {arg,argType,body} = Lambda.dest lambda
            val argTyD = Type.toMyType argType
            val bodyTyD = Type.toMyType $ Exp.ty body
            val funTyD = TyD.makeTarrow (argTyD,bodyTyD)
            val funTyS = VE.find ve var handle (VE.VarNotFound _) => 
              RefTyS.generalize (Vector.new0 (), RefTy.fromTyD
                funTyD)
            val funRefTy = elabHolesInRefTy $ mergeTypes (funTyD, 
              RefTyS.specialize funTyS) 
            val funspec = RefTyS.generalizeAssump (tyvars,funRefTy,
              RefTyS.isAssumption funTyS)
          in
            VE.add ve (var,funspec)
          end)

      fun elabDec (ve : VE.t, dec : Dec.t) : VE.t = 
        case dec of
          Dec.Fun {decs,tyvars} => 
          let
            val extendedVE = elabRecDecs ve (tyvars()) decs
          in
            extendedVE
          end
        | Dec.Val {rvbs,tyvars,vbs} => 
          let
            val rvbsVE = elabDec (ve,Dec.Fun {decs=rvbs,tyvars=tyvars})
            (*val vbsVE = Vector.mapAndFold (vbs,rvbsVE,
              fn ({valbind,...},ve) => doItValBind
              (ve,tyvars(),valbind))*)
          in
            rvbsVE
          end 
        | _ => ve

      val extendedVE = Vector.fold (decs, ve, 
        fn (dec,ve) => elabDec (ve, dec))
    in
      extendedVE
    end

  fun elaborate (Program.T {decs=decs}) (RelSpec.T {reldecs, typespecs}) =
    let
      val veWithBool = bootStrapBools VE.empty
      val initialVE = Vector.fold (decs,veWithBool,fn (dec,ve) =>
        case dec of Dec.Datatype datbinds => Vector.fold (datbinds, ve,
          fn (datbind,ve)   => elabDatBind ve datbind) 
          | _ => ve)
      (*
       * More dirty hacks!
       *)
      val initialRE = bootStrapFreeRels RE.empty
      val elabRE = Vector.fold (reldecs, initialRE,
        fn(StructuralRelation.T srbind,re) => elabSRBind re initialVE srbind)
      val refinedVE = Vector.fold (RE.toVector elabRE, initialVE, 
        fn ((id,{ty,map}),ve) => Vector.fold (map, ve, 
          fn (conPatBind,ve) => addRelToConTy ve conPatBind id))
      val fullVE = Vector.fold (typespecs, refinedVE, 
        fn (TypeSpec.T (isAssume,f,refTy),ve) => 
            VE.add ve (f,RefTyS.generalizeAssump (Vector.new0 (), 
              refTy, isAssume)))
      val elabVE = elabDecs (fullVE, decs)
    in
      (elabVE,elabRE)
    end
end

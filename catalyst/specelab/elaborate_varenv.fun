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
  structure RelTy = RelLang.RelType
  structure RelTyS = RelLang.RelTypeScheme
  structure RefTy = RefinementType
  structure RefTyS = RefinementTypeScheme
  structure P = Predicate
  structure RP = P.RelPredicate
  structure BP = P.BasePredicate
  structure TypeSpec = RelSpec.TypeSpec

  val assert = Control.assert
  fun $ (f,arg) = f arg
  infixr 5 $

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

  fun unifyConArgs (ve : VE.t) (con : Con.t) (vars : Var.t vector) =
    let
      val conStr = Con.toString con
      val convid = Var.fromString conStr
      val lenstr = fn v => (Int.toString o Vector.length) v
      val conTy = RefTyS.specialize (VE.find ve convid)
        handle (VE.VarNotFound v) => Error.bug ("Could not find constructor "
          ^ conStr  ^ " in varenv\n")
      (* -- Function duplicated from SpecVerify -- *)
      val newLongVar = fn (var,fld) => Var.fromString $
        (Var.toString var)^"."^(Var.toString fld)
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

  fun elaborate (Program.T {decs=decs}) (RelSpec.T {reldecs, typespecs}) =
    let
      val veWithBool = bootStrapBools VE.empty
      val initialVE = Vector.fold (decs,veWithBool,fn (dec,ve) =>
        case dec of Dec.Datatype datbinds => Vector.fold (datbinds, ve,
          fn (datbind,ve)   => elabDatBind ve datbind) 
          | _ => ve)
      val elabRE = Vector.fold (reldecs, RE.empty, 
        fn(StructuralRelation.T srbind,re) => elabSRBind re initialVE srbind)
      val refinedVE = Vector.fold (RE.toVector elabRE, initialVE, 
        fn ((id,{ty,map}),ve) => Vector.fold (map, ve, 
          fn (conPatBind,ve) => addRelToConTy ve conPatBind id))
      val fullVE = Vector.fold (typespecs, refinedVE, 
        fn (TypeSpec.T (isAssume,f,refTy),ve) => VE.add ve
        (f,RefTyS.generalizeAssump (Vector.new0 (), refTy, isAssume)))
    in
      (fullVE,elabRE)
    end
end

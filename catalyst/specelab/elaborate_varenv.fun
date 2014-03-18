functor ElaborateVarEnv (S : ELABORATE_VAR_ENV_STRUCTS) : ELABORATE_VAR_ENV = 
struct
  open S
  open SpecLang
  open ANormalCoreML
  structure L = Layout
  structure VE = VarEnv (structure Var = Var
                         structure SpecLang = SpecLang)
  structure RE = RelEnv (structure SpecLang = SpecLang)
  structure PRE = ParamRelEnv (structure SpecLang = SpecLang)
  structure TyD = TypeDesc
  structure TyDB = TyDBinds
  structure RelId = RelId
  structure TS = TupSort
  structure SPS = SimpleProjSort
  structure PTS = ProjTypeScheme
  structure PSS = ProjSortScheme
  structure RefTy = RefinementType
  structure PRf = ParamRefType
  structure RefSS = RefinementSortScheme
  structure RefTyS = RefinementTypeScheme
  structure P = Predicate
  structure RP = P.RelPredicate
  structure BP = P.BasePredicate
  structure TypeSpec = RelSpec.TypeSpec
  structure SR = StructuralRelation
  structure SPSBinds = ApplicativeMap (
    structure Key = 
     struct 
      open RelId
      val equal = eq
     end
     structure Value = 
     struct
      type t = {dom : TyD.t option ref, range : SVar.t}
      val layout = fn _ => Layout.empty
     end)
  structure SPSB = SPSBinds

  val assert = Control.assert
  fun $ (f,arg) = f arg
  infixr 5 $
  val tyconEq = Tycon.equals 
  (* -- Function duplicated from SpecVerify -- *)
  val count = ref 0
  fun getUniqueId symbase =
    let val id = symbase ^ (Int.toString (!count))
        val _ = count := !count + 1
    in
      Var.fromString id 
    end
  fun genVar () =  getUniqueId "x_" 
  val newLongVar = fn (var,fld) => Var.fromString $
    (Var.toString var)^"."^(Var.toString fld)
  val empty = fn _ => Vector.new0 ()
  val emptycs = fn _ => []
  val mergecs = List.concat
  val solvecs = TS.solvecs
  val assertEmptyCs = fn cs => case cs of [] => ()
    | _ => raise (Fail "sort inference impossible")
    
  
  (*
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
    in
      RE.add re (splRelId2, {ty=relTyS2, map=map})
    end
  *)

  fun bootStrapBools (ve: VE.t) = 
    let
      val boolTyD = TyD.makeTconstr (Tycon.bool,[])
      val tvid = Var.fromString $ Con.toString Con.truee
      val fvid = Var.fromString $ Con.toString Con.falsee
      val RefTy.Base (v,t,_) = RefTy.fromTyD boolTyD
      val eqPred1 = P.baseP $ BP.varBoolEq (v,true)
      val eqPred2 = P.baseP $ BP.varBoolEq (v,false)
      val empty = Vector.new0 ()
      val tTyS = RefTyS.generalizeRefTy (empty, 
        RefTy.Base (v,t,eqPred1))
      val fTyS = RefTyS.generalizeRefTy (empty, 
        RefTy.Base (v,t,eqPred2))
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
          val conTyS = RefTyS.generalizeRefTy (tyvars, RefTy.fromTyD conTyD)
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
      val conTy = RefTyS.specializeRefTy (VE.find ve convid)
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
      | Arrow ((argv,Tuple tv), Base (_,datTyD,_)) =>
          (assert (Vector.length tv = Vector.length vars,
          conStr ^ " expects "^ (lenstr tv) ^" args. " 
            ^ (lenstr vars) ^ " given");
         Vector.map2 (tv,vars,fn ((fldv, Base (_,argTyD,_)), var) =>
            (newLongVar (argv,fldv), var, argTyD, 
              TyD.sameType (argTyD,datTyD))))
      | _ => raise (Fail "Could not unify and determine rec args")
    end

  fun addRelToConTy (ve: VE.t) (con,valop,rexpr) (id:RelId.t) =
    let
      val convid = Var.fromString (Con.toString con)
      val substs = case valop of NONE => Vector.fromList []
        | SOME vals => Vector.map (unifyConArgs ve con vals, 
          fn (cvar,var,_,_) => (cvar,var))
      val rexpr' = RelLang.applySubsts substs rexpr
      val  conRefTys = VE.find ve convid
        handle (VE.VarNotFound v) => Error.bug ("Could not find\
         \ constructor " ^ (Var.toString convid) ^ " in varenv\n")
      val RefTyS.T {tyvars,...} = conRefTys
      val targs = Vector.map (tyvars,TyD.makeTvar)
      val refty = RefTyS.specializeRefTy conRefTys
      val newref = fn var => RP.Eq (RelLang.appR(id,targs,var),rexpr')
      val annotConTy = case refty of
          RefTy.Base (bv,tyd,pred) => RefTy.Base (bv,tyd, 
            Predicate.conjR (pred,newref bv))
        | RefTy.Arrow (arg,RefTy.Base (bv,tyd,pred)) => RefTy.Arrow(arg,
            RefTy.Base (bv,tyd, Predicate.conjR (pred,newref bv)))
        | _ => raise (Fail "Constructor type is neither base not arrow")
      val newTyS = RefTyS.generalizeRefTy (tyvars,annotConTy)
    in
      VE.add (VE.remove ve convid) (convid,newTyS)
    end

  (*
   * Synthesizes the type of rexpr in the given relational env.
   * Rel Env is constructed during elaboration, hence this function
   * is also part of elaboration.
   *)
  exception CantInferType

  fun elabRExpr (re,pre,tyDB,spsB,rexpr) : (TS.cs list * TS.t * 
      RelLang.expr)=
    let
      open RelLang
      fun typeSynthRElem elem = case elem of
          Int i => TyD.makeTconstr (Tycon.intInf,[])
        | Bool b => TyD.makeTconstr (Tycon.bool,[])
        | Var v => TyDBinds.find tyDB v 
      fun doIt (e1,e2) cons f = 
        let 
          val (cs1,tupTy1,e1') = elabRExpr (re,pre,tyDB,spsB,e1)
          val (cs2,tupTy2,e2') = elabRExpr (re,pre,tyDB,spsB,e2)
          val (cs,tupTy) = f (tupTy1,tupTy2)
        in
          (mergecs [cs1,cs2,cs], tupTy, cons (e1',e2'))
        end
      val isParam = fn rid => SPSB.mem spsB rid
      fun doItParamApp (rinst as RInst {rel=rid,...},x) = 
        let
          val {dom,range=svar} = SPSB.find spsB rid
          val tyd = TyDB.find tyDB x handle 
            TyDB.KeyNotFound _ => raise (Fail ("Var "
              ^(Var.toString x)^" unknown"))
          val _ = case !dom of NONE => dom := SOME tyd
            | SOME tyd' => assert (TyD.sameType (tyd,tyd'),
                "Inconsistent application of param: "
                ^(RelId.toString rid))
          val expsort = TS.fromSVar svar
        in
          (emptycs(), expsort, R (rinst,x))
        end
      fun doItRInstApp (rinst as RInst {rel,args,...},x) =
        let
          val argRels = Vector.map (args, 
            fn (RInst {rel,...}) => rel)
          val _ = assert (Vector.forall (argRels, isParam),
            "Currently, only params should be used to instantiate\
            \ relations\n")
          val relName = RelId.toString rel
          val {ty=relTyS,...} = PRE.find pre rel handle
            PRE.ParamRelNotFound _ => raise (Fail ("Inst of unknown\
            \ relation " ^relName^" applied to "^(Var.toString x)))
          val tyd' = PTS.domain relTyS
          val tyd = TyDB.find tyDB x handle 
            TyDB.KeyNotFound _ => raise (Fail ("Var "
              ^(Var.toString x)^" unknown"))
          val targs = Vector.fromList (case (tyd,tyd') of 
              (TyD.Tconstr (tycon,targs), TyD.Tconstr (tycon', _)) =>
                (assert (tyconEq (tycon,tycon'),"Relation "^relName
                ^" applied to arg of wrong type"); targs)
            | (_,TyD.Tvar _) => [tyd]
            | _ => Error.bug ("RelApp type mismatch: " ^ relName
                ^ "applied to "^(Var.toString x)))
          val sargs = Vector.map (argRels, fn (rid) => 
              TS.fromSVar $ #range $ SPSB.find spsB rid) handle
                SPSB.KeyNotFound _ => Error.bug ("Unknown relation"
                  ^(RelId.toString rel))
          val relSS = PTS.instantiate (relTyS,targs)
          val ProjSort.T {paramsorts, sort = SPS.ColonArrow (_,expsort)} = 
            PSS.instantiate (relSS,sargs)
          val _ = assert (Vector.length paramsorts = Vector.length
            argRels, "Incorrectly instantiating "^relName)
          val _ = Vector.foreach2(argRels,paramsorts, 
            fn (rid,SPS.ColonArrow (tyd,_)) =>
            let
              val {dom,range} = SPSB.find spsB rid
              val _ = case !dom of NONE => dom := SOME tyd
                | SOME tyd' => assert (TyD.sameType (tyd,tyd'),
                "Invalid inst with: " ^(RelId.toString rid))
            in
              ()
            end)
          val newRInst = RInst {rel=rel, args=args, targs=targs,
            sargs=sargs}
        in
          (emptycs(), expsort, R (newRInst,x))
        end
    in
      case rexpr of
        U v => doIt v U TS.unionType | X v => doIt v X TS.crossPrdType
      | D v => doIt v D TS.unionType 
      | T els => (emptycs(), TS.Tuple $ Vector.toListMap (els, 
          (TS.T o typeSynthRElem)) ,rexpr)
      | R (rinst as RInst {rel=relId,args,...},x) => if isParam relId 
          then doItParamApp (rinst,x) 
          else doItRInstApp (rinst,x)
    end

  fun elabSRBind (re: RE.t)(pre: PRE.t)(ve : VE.t) {id,params,map} =
    let
      (* Initially, param domains are none *)
      val spsB = Vector.foldr (params, SPSBinds.empty, fn (r,spsB) => 
        SPSBinds.add spsB r {dom = ref NONE, range = SVar.new ()})
      val isParam = fn rid => SPSB.mem spsB rid

      (* First pass - type & sort annotate instantiations *)

      val (map', relTySOp) = Vector.mapAndFold (map, NONE, 
        fn ((con,valop,rterm), relTySOp) => case (valop,rterm) of
          (NONE,RelLang.Expr _) => (* must be Rnull *)
            ((con, valop, rterm), relTySOp) 
        | (NONE,RelLang.Star ie) =>
          let
            val _ = case relTySOp of NONE => ()
              | SOME _ => raise (Fail "Ind uasge wrong")
            val RelLang.RInst {rel,args,...} = ie
            val argRels = Vector.map (args, 
              fn (RelLang.RInst {rel,...}) => rel)
            val _ = assert (Vector.forall (argRels, isParam),
              "Currently, only params should be used to instantiate\
              \ relations\n")
            val {ty=relTyS,...} = PRE.find pre rel handle
              PRE.ParamRelNotFound _ => raise (Fail ("Ind of unknown\
                \ param relation: "^(RelId.toString rel)))
            val PTS.T {tyvars,sortscheme = relSS} = relTyS
            val targs = Vector.map (tyvars, TyD.makeTvar)
            val PSS.T {svars,sort} = relSS
            val sargs = Vector.map (svars, TS.fromSVar)
            val newRInst = RelLang.RInst {rel=rel, args=args, 
              targs=targs, sargs=sargs}
          in
            ((con, valop, RelLang.Star newRInst), SOME relTyS)
          end
        | (SOME vars, RelLang.Expr rexpr) => 
          let
            val convid = Var.fromString (Con.toString con)
            val RefTyS.T {tyvars,refss,...} = VE.find ve convid handle
              VE.VarNotFound _ => Error.bug ("Constructor " ^
                (Con.toString con) ^ " not found in var env.")
            val refty = RefSS.toRefTy refss
            val datTyD = case refty of RefTy.Base (_,datTyD,_) => datTyD
              | RefTy.Arrow (_,RefTy.Base (_,datTyD,_)) => datTyD
              | _ => raise (Fail "Impossible case")
            val tyDB = Vector.fold (unifyConArgs ve con vars, 
              TyDBinds.empty,
              fn ((_,var,tyD,_),tyDB) => TyDBinds.add tyDB var tyD)
            val (cs,tupTy,rexpr') = elabRExpr (re,pre,tyDB,spsB,rexpr)
            val _ = assertEmptyCs cs
            val relSPS = SPS.ColonArrow (datTyD, tupTy)
            val (svars, paramSPS) = Vector.unzip $ Vector.map 
              (SPSBinds.toVector spsB, fn (_,{dom,range=svar}) => 
                let
                  val tyD = case !dom of SOME tyD => tyD
                    | NONE => raise (Fail "Unused rel param\n")
                  val tupTy = TupSort.fromSVar svar
                in
                  (svar,SPS.ColonArrow (tyD,tupTy))
                end)
            val relPS = ProjSort.new (paramSPS,relSPS)
            val relSS = PSS.generalize (svars,relPS)
            val relTyS = PTS.generalize (tyvars,relSS)
          in
            ((con, valop, RelLang.Expr rexpr'), SOME relTyS)
          end handle CantInferType => ((con, valop, rterm),relTySOp)
        | _ => raise (Fail "Impossible case of rterm"))
      val pts = case relTySOp of NONE => raise CantInferType
        | SOME relTyS => relTyS 
      val bdef = Bind.makeBindDef (id,params,pts)
      val pre' = PRE.add pre (id,{ty=pts, def = bdef})

      (* Second pass - make ground def; expand inductive defs*)

      val map'' = (Vector.concatV o Vector.map) (map', 
      fn (con,valop,rterm) =>
        case Bind.makeGroundDef (params,rterm) of 
          RelLang.Expr rexpr => Vector.fromList [(con,valop,rexpr)]
        | RelLang.Star (RelLang.RInst {rel = relId, ...}) => 
          let
            val {ty=relTyS,map} = RE.find re relId 
              handle (RE.RelNotFound r) => raise (Fail 
                ("Ind of unknown ground rel : "^(RelId.toString r)))
            val PTS.T {tyvars,...} = relTyS
            val targs = Vector.map (tyvars,TyD.makeTvar)
          in
            Vector.map (map, fn (con,valop,rexpr) => case valop of 
                NONE => (con,valop,rexpr)
              | SOME vars => 
                let
                  val recvars = Vector.map ((#yes o Vector.partition) 
                    (unifyConArgs ve con vars, fn (_,_,_,isrec) => isrec),
                      fn (cvar,_,_,_) => cvar)
                  val recRApps = Vector.map (recvars, fn var => 
                    RelLang.appR (id,targs,var))
                  val recRAppsUnion = Vector.fold (recRApps,
                    RelLang.rNull(), RelLang.union)
                  val rexpr' = RelLang.union (rexpr, recRAppsUnion)
                in
                  (con,valop,rexpr')
                end)
          end)
      val ty' = Bind.groundRelTyS pts
      val re' = RE.add re (id,{ty=ty',map=map''})
    in
      (re',pre')
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
      val (_,(_,refTy')) = doMerge tyd (genVar (), refTy)
    in
      refTy'
    end

  (*
   * Forall top-level fun decs, elabDecs annotates their ML
   * types with type refinements. 
   *)
  fun elabDecs (ve : VE.t, decs : Dec.t vector) : VE.t =
    let
      fun elabRecDecs (ve : VE.t) (tyvars : Tyvar.t vector)  decs = 
        Vector.fold (decs,ve, fn ({lambda : Lambda.t, var :
          Var.t}, newVE) =>
          let
            val {arg,argType,body} = Lambda.dest lambda
            val argTyD = Type.toMyType argType
            val bodyTyD = Type.toMyType $ Exp.ty body
            val funTyD = TyD.makeTarrow (argTyD,bodyTyD)
            val funTyS = VE.find ve var (* VarNotFound handled below *)
            val funRefTy = mergeTypes (funTyD, RefTyS.specializeRefTy
                funTyS) 
            val RefSS.T{svars, prefty=PRf.T{params,...}} = 
              RefTyS.specialize funTyS
            val funRefSS = RefSS.T{svars=svars, prefty=PRf.T 
              {params=params, refty=funRefTy}}
            val funspec = RefTyS.generalizeAssump (tyvars,funRefSS,
              RefTyS.isAssumption funTyS)
          in
            VE.add (VE.remove newVE var) (var,funspec)
          end handle (VE.VarNotFound _) => newVE)
      fun elabDec (ve : VE.t, dec : Dec.t) : VE.t = case dec of
          Dec.Fun {decs,tyvars} => elabRecDecs ve (tyvars()) decs
        | Dec.Val {rvbs,tyvars,...} => elabDec (ve, Dec.Fun 
            {decs=rvbs,tyvars=tyvars})
        | _ => ve
      val extendedVE = Vector.fold (decs,ve, 
        fn (dec,ve) => elabDec (ve,dec))
    in
      extendedVE
    end

  fun elabTypeSpec re pre {params,refty} =
    let
      (* Initially, param domains are none *)
      val spsB = Vector.foldr (params, SPSBinds.empty, fn (r,spsB) => 
        SPSBinds.add spsB r {dom = ref NONE, range = SVar.new ()})
      val isParam = fn rid => SPSB.mem spsB rid
      fun doItRelPred tyDB rp = 
        let
          open RP
          fun doIt (r1,r2) cons =
            let
              val (cs,_,RelLang.U (r1',r2')) = elabRExpr
                (re,pre,tyDB,spsB, RelLang.U (r1,r2))
            in
              (cs, cons (r1',r2'))
            end
        in
          case rp of Eq x => doIt x Eq | Sub x => doIt x Sub
          | SubEq x => doIt x SubEq
        end
      fun doItPhi tyDB phi = 
        let
          fun doItTup (p1,p2) cons =
            let
              val (cs1,p1') = doItPhi tyDB p1
              val (cs2,p2') = doItPhi tyDB p2
              val cs = mergecs [cs1,cs2]
            in
              (cs,cons (p1',p2'))
            end
        in
           case phi of
            P.Conj x => doItTup x P.Conj | P.Disj x => doItTup x P.Disj
          | P.If x => doItTup x P.If | P.Iff x => doItTup x P.Iff
          | P.Not t => (fn (cs,t') => (cs,P.Not t')) $ doItPhi tyDB t
          | P.Base _ => (emptycs(),phi)
          | P.Rel rp => (fn (cs,rp') => (cs, P.Rel rp')) $
              doItRelPred tyDB rp
          | _ => (emptycs(),phi)
        end
      fun doItRefTy tyDB refty = case refty of 
          RefTy.Base (bv,tyd,phi) => 
            let
              val (cs,phi') = doItPhi (TyDB.add tyDB bv tyd) phi
            in
              (cs, RefTy.Base (bv,tyd,phi'))
            end 
        | RefTy.Arrow ((x,t1),t2) => 
          let
            val (cs1,t1') = doItRefTy tyDB t1
            val tyd1 = RefTy.toTyD t1
            val tyDB' = TyDB.add tyDB x tyd1
            val (cs2,t2') = doItRefTy tyDB' t2
          in
            (List.concat [cs1,cs2], RefTy.Arrow ((x,t1'),t2'))
          end
        | _ => (emptycs(), refty)
      val (cs,refty') = doItRefTy TyDB.empty refty
      val (solfn :SVar.t -> TS.t) = solvecs cs
      val sortedParams = Vector.map (SPSB.toVector spsB, 
        fn (r,{dom,range=svar}) => case !dom of 
          SOME tyd => (r,SPS.ColonArrow (tyd,solfn svar))
        | NONE => raise (Fail ("Unused rel param: "^
              (RelId.toString r))))
      val {unions, empty, ...} = List.set {equals=SVar.eq, 
        layout = fn _ => L.empty}
      val svars = Vector.fromList $ Vector.fold (sortedParams, empty, 
        fn ((_,SPS.ColonArrow (_,tupTy)),set) => 
          unions [set, TS.getSVars tupTy])
      (*
       * solfn has to be applied to sort arguments in refty.
       *)
      val prf = PRf.parametrize (sortedParams, 
        RefTy.mapSVar refty' solfn)
      val refSS = RefSS.generalize (svars,prf)
    in
      refSS
    end

  fun elaborate (Program.T {decs=decs}) (RelSpec.T {reldecs, typespecs}) =
    let
      val veWithBool = bootStrapBools VE.empty
      val initialVE = Vector.fold (decs,veWithBool,fn (dec,ve) =>
        case dec of Dec.Datatype datbinds => Vector.fold (datbinds, ve,
          fn (datbind,ve)   => elabDatBind ve datbind) 
          | _ => ve)
      val initialRE = (*bootStrapFreeRels*) RE.empty
      val (elabRE,elabPRE) = Vector.fold (reldecs, 
        (initialRE, PRE.empty), fn(SR.T srbind,(re,pre)) => 
          elabSRBind re pre initialVE srbind)
      val refinedVE = Vector.fold (RE.toVector elabRE, initialVE, 
        fn ((id,{ty,map}),ve) => Vector.fold (map, ve, 
          fn (conPatBind,ve) => addRelToConTy ve conPatBind id))
      val protoVE = Vector.fold (typespecs, VE.empty,
        fn (TypeSpec.T {isAssume,name,params,refty},ve) => 
          let
            val dummySPS = SPS.ColonArrow (TyD.makeTvar $
              Tyvar.newNoname {equality=false}, TS.Tuple 
                [TS.S $ SVar.new()])
            val sortedParams = Vector.map (params, 
              fn r => (r,dummySPS))
            val prefTy = PRf.parametrize (sortedParams,refty)
            val refSS = RefSS.generalize (empty(),prefTy)
            val refTyS = RefTyS.generalizeAssump
              (empty(),refSS,isAssume)
          in
            VE.add ve (name,refTyS)
          end)
      val typedVE = elabDecs (protoVE, decs)
      val fullVE = Vector.fold (VE.toVector typedVE, refinedVE,
        fn ((name,RefTyS.T {tyvars,isAssume,refss}),ve) =>
          let
            val RefSS.T {prefty, ...} = refss
            val PRf.T {params=sortedParams,refty} = prefty
            val (params,_) = Vector.unzip sortedParams
            val refss' = elabTypeSpec elabRE elabPRE {params=params,
              refty=refty}
            val refTyS = RefTyS.T {tyvars=tyvars, isAssume=isAssume, 
              refss=refss'}
          in
            VE.add ve (name,refTyS)
          end)
    in
      (fullVE,elabRE,elabPRE)
    end
end

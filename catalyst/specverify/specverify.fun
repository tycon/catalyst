functor SpecVerify (S : SPEC_VERIFY_STRUCTS) : SPEC_VERIFY = 
struct
  open S
 
  structure SpecLang = VE.SpecLang
  structure VC = VerificationCondition (open SpecLang
                                        structure VE = VE
                                        structure RE = RE
                                        structure PRE = PRE)
  open SpecLang
  open ANormalCoreML
  structure TyD = TypeDesc
  structure PTS = ProjTypeScheme
  structure RefTy = RefinementType
  structure PRf = ParamRefType
  structure RefSS = RefinementSortScheme
  structure RefTyS = RefinementTypeScheme
  structure P = Predicate
  structure BP = Predicate.BasePredicate
  structure RP = Predicate.RelPredicate
  structure L = Layout
  structure Tyvar =
  struct
    open Tyvar
    val equal= fn (t,t') => toString t = toString t'
    val eq = equal
  end

  type subst = Var.t*Var.t
  type substs = subst Vector.t
  fun $ (f,arg) = f arg
  infixr 5 $
  val assert = Control.assert
  val empty = fn _ => Vector.new0 ()
  val unifiable = TyD.unifiable
  fun toRefTyS refTy = RefTyS.generalize (Vector.new0(), 
    RefSS.fromRefTy refTy)
  val count = ref 4096
  fun getUniqueId symbase =
    let val id = symbase ^ (Int.toString (!count))
        val _ = count := !count + 1
    in
      Var.fromString id 
    end
  fun genVar () =  getUniqueId "sv_" 
  fun getUniqueMarker () = getUniqueId "_mark_"
  fun dummyRefTyS () = RefTyS.generalize (Vector.new0(),
    RefSS.fromRefTy $RefTy.fromTyD (TyD.makeTunknown()))
  val newLongVar = fn (var,fld) => Var.fromString $
    (Var.toString var)^"."^(Var.toString fld)
  fun varEq (v1,v2) = ((Var.toString v1) = (Var.toString v2))
  val varToExpVal = fn (var,tyvec) => 
    Exp.Val.Atom (Exp.Val.Var (var,tyvec))
  fun markVE ve = 
    let
      val marker = getUniqueMarker ()
    in
      (marker,VE.add ve (marker,dummyRefTyS()))
    end 

  val elabPatVal = fn (ve,tyvars,patval,expty:RefTy.t) =>
    let
      open Pat.Val
      val err = fn _ => String.concat 
        ["Expression type is not row type.\n",
          "Pattern is ",L.toString $ layout patval,"\n",
          "Type of expression is ",L.toString $ RefTy.layout expty,
            "\n" ]
      val len = Vector.length
      fun elabPatVal (ve,patval,expty:RefTy.t) : VE.t = 
        case (patval,expty) of
        (Atom (Wild),_) => ve
      | (Atom (Const c),_) => Error.bug ("unimpl constant pats")
      | (Atom (Var v),_) => VE.add ve (v, RefTyS.generalize 
          (tyvars,RefSS.fromRefTy expty))
      | (Tuple patatoms,_) => (case (len patatoms,expty) of 
          (* Unit tuples are atoms *)
          (1,_) => elabPatVal (ve,Atom (Vector.sub (patatoms,0)),expty)
        | (_, RefTy.Tuple refTyBinds) => Vector.fold2 (patatoms, 
            refTyBinds, ve, fn (patatom,(_,refTy),ve) =>
              elabPatVal (ve, Atom patatom, refTy))
        | _ => Error.bug $ err ())
      | (Record patmrec, RefTy.Tuple refTyBinds) => Vector.fold
          (Record.toVector patmrec, ve, fn ((lbl,patom),ve) =>
            let
              val indx = Vector.index (refTyBinds, fn (fldvar,_) =>
                  Field.toString lbl = Var.toString fldvar)
              val refTyBind as (_,refTy) = case indx of 
                  SOME i => Vector.sub (refTyBinds,i)
                | NONE => Error.bug $ "Record field not found\n"^(err ())
            in
              elabPatVal (ve, Atom patom, refTy)
            end)
      | _ => Error.bug $ err()
    in
      elabPatVal (ve,patval,expty)
    end

  (*
   * Decomposes single tuple bind of form v ↦ {x0:T0,x1:T1} to
   * multiple binds : [v.x0 ↦ T0, v.x1 ↦ T1]
   *)
  val decomposeTupleBind = RefTy.decomposeTupleBind

  (*
   * Invariant : bv and td of ty is unchanged.
   *)
  fun wellFormedType (marker : Var.t, markedVE : VE.t, ty : RefTy.t) 
    : RefTy.t =
    let
      (* we rely on the fact the VE is ordered *)
      val vevec = VE.toVector markedVE
      val indx = Vector.index (vevec, fn (v,_) => varEq (v,marker))
      val i = case indx of SOME i => i | _ => Error.bug "Marker absent"
      (*
       * tyvars are only generalized for function types and tuple types.
       * We do not need vars with such types as they are not referenced
       * from type refinements.
       *)
      val extyvec = Vector.map (Vector.prefix (vevec,i), 
        fn (v,rtys) => (v,RefSS.toRefTy $ RefTyS.specialize rtys))
      val flattened = Vector.concatV $ Vector.map (extyvec, fn (v,ty) 
        => case ty of RefTy.Tuple _ => decomposeTupleBind (v,ty)
        | _ => Vector.new1 (v,ty))
      val (tyDB,pred1)= Vector.fold (flattened,(TyDBinds.empty,
        Predicate.truee()), fn ((exvar,exty),(clos,pred')) =>
          case exty of
            RefTy.Base (bv,extd,pred) => (TyDBinds.add clos exvar extd,
              Predicate.conj (pred',Predicate.applySubst (exvar,bv) pred))
          | RefTy.Tuple _ => Error.bug "Tuple flattening incorrect\n"
          | _ => (clos,pred'))
      (*
       *val _ = print "TyDBinds:\n"
       *val _ = Layout.print (TyDBinds.layout tyDB,print)
       *val _ = print "\n"
       *)
    in
      RefTy.mapBaseTy ty (fn (v,td,pred) => (v,td,
        Predicate.exists (tyDB,Predicate.conj(pred1,pred))))
    end

  (*
   * For functions with dependent types, bound variables within argument
   * types are refered from refinements of result types. At application
   * sites, actual program vars are substituted for formal boundvars.
   * unifyArgs returns such substitutions.
   * unifyArgs also returns new type bindings for actual vars. This is for
   * the sake of constructor pattern matches, where the returned type binds
   * contain type bindings for matched pattern vars.
   *)
  fun unifyArgs (argBind as (argv : Var.t, argTy : RefTy.t) ,
      argExpVal : Exp.Val.t) : ((Var.t*RefTy.t) vector * substs) =
    let
      open Exp.Val
    in
      case (argTy,argExpVal) of
        (RefTy.Base _, Atom (Var (v,typv))) => 
          (Vector.new1 (v,argTy), Vector.new1 (v,argv))
      | (RefTy.Base _,Atom (Const c)) => Error.bug $ 
          "Unimpl const args"
      | (RefTy.Tuple argBinds',Tuple atomvec) => 
          (*
           * Unifies v:{1:T0,2:T1} with (v0,v1)
           * Returns binds = [v0 ↦ T0, v1 ↦ T1],
           *        substs = [v0/v.1, v1/v.2]
           *)
          let
            val (reftyss,substss) = (Vector.unzip o Vector.map2)
              (argBinds',atomvec, fn (argBind',atom) => 
                let
                  val (binds,substs') = unifyArgs (argBind', Atom atom)
                  val substs = Vector.map (substs', 
                    fn (n,o') => (n, newLongVar (argv,o')))
                in
                  (binds,substs)
                end)
          in
            (Vector.concatV reftyss, Vector.concatV substss)
          end
      | (RefTy.Tuple argBinds', Record atomrec) => 
          let
            val (reftyss,substss)= (Vector.unzip o Vector.map)
            (argBinds', fn (argBind') =>
              let
                val (argv',_) = argBind'
                val argvStr' = Var.toString argv'
                val lblatomvec = Record.toVector atomrec
                val indx = Vector.index (lblatomvec, fn (lbl,_) =>
                  Field.toString lbl = argvStr')
                val (_,atom) = case indx of 
                    SOME i => Vector.sub (lblatomvec,i)
                  | NONE => Error.bug ("Field " ^ (argvStr') ^ 
                      " could not be found.")
                val (binds,substs') = unifyArgs (argBind',Atom atom)
                  val substs = Vector.map (substs', 
                    fn (n,o') => (n, newLongVar (argv,o')))
              in
                (binds,substs)
              end)
          in
            (Vector.concatV reftyss, Vector.concatV substss)
          end
      | (RefTy.Tuple argBinds', Atom (Var (v,_))) => 
          let
            (* Unifying v0:{x0:T0,x1:T1} with v1 would return
             * v1 ↦ {x0:T0,x1:T1} as the only new bind. However,
             * all references v0 elements should now refer to v1
             * elements. Therefore, substs = [v1.x0/v0.x0, v1.x1/v0.x1]
             *)
            val binds = Vector.new1 (v,argTy)
            val substs = (Vector.concatV o Vector.map)
            (argBinds', fn (argBind') =>
              let
                val (argv',_) = argBind'
                val newVar = newLongVar (v,argv')
                val (_,substs') = unifyArgs (argBind',
                  Atom (Var (newVar, Vector.new0 ())))
                val substs = Vector.map (substs', 
                  fn (n,o') => (n, newLongVar (argv,o')))
              in
                substs
              end)
          in
            (binds,substs)
          end
      | (RefTy.Arrow _, Atom (Var (v,_))) => (Vector.new1 (v,argTy), 
          Vector.new1 (v,argv))
      | _ => raise Fail $ "Invalid argTy-argExpVal pair encountered"
    end

    fun unifyWithDisj (refTy1 : RefTy.t,refTy2 : RefTy.t) : RefTy.t =
      let
        open RefTy
      in
        case (refTy1,refTy2) of
          (Base (_,TyD.Tunknown,_),_) => refTy2
        | (_,Base (_,TyD.Tunknown,_)) => refTy1
        | (Base (bv1,td1,pred1),Base (bv2,td2,pred2)) =>
            let
              val _ = assert (TyD.sameType (td1,td2), "Typedescs from \
                \ two branches of case did not match. Two types are:\n \
                \ 1. "^(TyD.toString td1) ^"\n" ^ "\
                \ 2. "^(TyD.toString td2) ^ "\n")
              val pred1' = Predicate.applySubst (bv2,bv1) pred1
            in
              Base (bv2,td2,Predicate.dot (pred1',pred2))
            end
        | (Tuple t1,Tuple t2) => (Tuple o Vector.map2) (t1,t2, 
            fn ((v1,r1),(v2,r2)) => 
              let
                val _ = assert (varEq (v1,v2), "Labels of tuples from \
                  \ two branches of case did not match")
              in
               (v2,unifyWithDisj (r1,r2))
              end)
        | (Arrow _,Arrow _) => Error.bug "Unimpl : Case returning arrow"
        | _ => Error.bug "Case rules types not unifiable"
      end 

  fun typeSynthValExp (ve:VE.t, pre:PRE.t, valexp : Exp.Val.t) 
      : RefTy.t = 
    let
      open Exp
    in
      case valexp of
        Val.Atom (Val.Const c) => RefTy.fromTyD (TyD.makeTunknown ())
      | Val.Atom (Val.Var (v,typv)) =>  
        let
          val tydvec = Vector.map (typv,Type.toMyType)
          val vtys = VE.find ve v handle (VE.VarNotFound _) => Error.bug
            ((Var.toString v) ^ " not found in the current environment\n")
          val vss = RefTyS.instantiate (vtys,tydvec)  
          val vty = RefSS.toRefTy vss
          val _ = case vss of RefSS.T {prefty = PRf.T {params, ...}, 
            ...} => assert (Vector.isEmpty params, "Non-recursive use\
              \ of a param type, without instantiating rel params.")
          (*
           * Keep track of variable equality.
           * We currently cannot keep track of equality if rhs
           * is a nullary constructor or a function. Unimpl.
           *)
          val qualifiedvty = case (Vector.length tydvec, vty) of 
              (0,RefTy.Base (bv,td,pred)) => RefTy.Base 
                (bv,td,Predicate.conjP(pred,BP.varEq(bv,v)))
            | (_,RefTy.Tuple refTyBinds) => RefTy.Tuple $ Vector.map
                (refTyBinds, fn (fldvar,refty) => 
                  let
                    val newvar = newLongVar (v,fldvar)
                    val extendedVE = VE.add ve (newvar,toRefTyS refty)
                    val newvarexp = varToExpVal (newvar, Vector.new0 ())
                    val refty' = typeSynthValExp (extendedVE, pre, newvarexp)
                  in
                    (fldvar, refty')
                  end)
            | _ => vty (* Unimpl : refinements for fns. Cannot keep
                   track of equality for functions now.*)
        in
          qualifiedvty
        end
      | Val.Tuple atomvec => RefTy.Tuple $ Vector.mapi (atomvec, fn (i,atm) => 
          let
            val atmTy = typeSynthValExp (ve, pre, Val.Atom atm)
            val fldvar = Var.fromString $ Int.toString (i+1) (* tuple BVs *)
          in
            (fldvar, atmTy)
          end)
      | Val.Record atomrec => RefTy.Tuple $ Vector.map (Record.toVector atomrec, 
          fn (lbl,atm) => 
            let
              val atmTy = typeSynthValExp (ve, pre, Val.Atom atm)
              val fldvar = Var.fromString $ Field.toString lbl (* Record BVs *)
            in
              (fldvar, atmTy)
            end)
    end

  fun typeSynthExp (ve : VE.t, pre : PRE.t, exp : Exp.t) : 
        VC.t vector * RefTy.t =
    let
      open Exp
      val expTy = ty exp
      val trivialAns = fn _ => (Vector.new0(), RefTy.fromTyD $ 
        Type.toMyType expTy)
      val nopAns = fn _ => (Vector.new0(), RefTy.exnTyp ())
    in
      case node exp of
        App (f,valexp) => 
          let
            val fty  = typeSynthValExp (ve,pre,Val.Atom f)
            val (fargBind as (farg,fargty),fresty)  = case fty of 
                RefTy.Arrow x => x
              | _ => Error.bug ("Type of " ^ (Layout.toString $ 
                Exp.Val.layout $ Val.Atom f) ^ " not an arrow")
            val argTy = typeSynthValExp (ve,pre,valexp)
            (*
             *  Γ ⊢ argTy <: fargty
             *)
            val vcs = VC.fromTypeCheck (ve,pre,argTy,fargty)
            (*
             * Then, determine type of this expression by substituion of
             * actuals for formals.
             *)
            val (_,substs) = unifyArgs (fargBind,valexp)
            val resTy = RefTy.applySubsts substs fresty
          in
            (vcs,resTy)
          end
      | Case {test:Exp.Val.t,rules,...} => 
          let
            val (wfTypes,vcs) = Vector.mapAndFold (rules, Vector.new0(), 
              fn ({pat,exp,...},vcs) =>
              let
                val valbind = Dec.PatBind (pat,test)
                val (marker,markedVE) = markVE ve
                (* 
                 * tyvars used, if any, for type instantiations inside
                 * test are bound at any of the enclosing valbinds. Therefore,
                 * passing empty for tyvars is sound.
                 *)
                val (vcs1,extendedVE) = doItValBind (markedVE,
                  pre, Vector.new0(), valbind)
                val (vcs2,ty) = typeSynthExp (extendedVE,pre,exp)
                val wftype = wellFormedType (marker,extendedVE,ty)
              in
                (wftype, Vector.concat [vcs, vcs1, vcs2])
              end)
            val (wfTypes,wfType) = Vector.splitLast wfTypes
            val unifiedType = Vector.fold (wfTypes, wfType, unifyWithDisj)
          in
            (*
             * 1. alphaRename boundvars forall wfTypes to a single var
             * 2. assert that tyd is same for all
             * 3. fold all alpha-renamed preds of wftypes with Disj
             *)
            (vcs,unifiedType)
          end
      | EnterLeave _ => trivialAns ()
      | Handle _ => trivialAns ()
      | Lambda l => typeSynthLambda (ve, pre, l)
      | Let (decs,subExp) => 
        let
          val (marker,markedVE) = markVE ve
          val (vcs1,extendedVE) = doItDecs (markedVE,pre,decs)
          val (vcs2,subExpTy) = typeSynthExp (extendedVE,pre,subExp)
        in
          (Vector.concat [vcs1, vcs2], 
            wellFormedType (marker,extendedVE,subExpTy))
        end
      | PrimApp {args, prim, targs} => trivialAns ()
      | Nop => nopAns ()
      | Seq tv => typeSynthExp (ve,pre,Vector.last tv)
      | Value v => (Vector.new0(),typeSynthValExp (ve,pre,v))
    end

  and typeSynthLambda (ve : VE.t, pre :PRE.t, lam : Lambda.t) 
            : (VC.t vector * RefTy.t) =
    let
      val {arg,argType,body} = Lambda.dest lam
      val argRefTy = RefTy.fromTyD (Type.toMyType argType)
      val argBind = (arg,argRefTy)
      (*val _ = L.print (L.seq[Var.layout arg, L.str " :-> ",
        RefTy.layout argRefTy], print)*)
      val extendedVE = VE.add ve (arg,toRefTyS argRefTy)
      (*
       * Γ[arg↦argTy] ⊢ body => bodyTy
       *)
      val (bodyvcs,bodyRefTy) = typeSynthExp (extendedVE, pre, body)
    in
      (bodyvcs,RefTy.Arrow (argBind,bodyRefTy))
    end

  and typeCheckLambda (ve : VE.t, pre:PRE.t, lam : Lambda.t, 
      ty : RefTy.t) : VC.t vector =
    let
      val (argBind as (_,argRefTy),resRefTy) = case ty of 
          RefTy.Arrow v => v
        | _ => Error.bug "Function with non-arrow type"
      val {arg,argType,body} = Lambda.dest lam
      val extendedVE = VE.add ve (arg, toRefTyS argRefTy)
      (*
       * Substitute formal vars with actual vars
       *)
      val (binds, substs) = unifyArgs (argBind, (Exp.Val.Atom 
        (Exp.Val.Var (arg, Vector.new0 ()))))
      val _ = assert (Vector.length binds = 1, "Unification \
        \ of fn args incorrect.")
      val resRefTy' = RefTy.applySubsts substs resRefTy
    in
      (*
       * Γ[arg↦argRefTy] ⊢ body <= resRefTy
       *)
      typeCheckExp (extendedVE,pre,body,resRefTy')
    end

  and typeCheckExp (ve : VE.t, pre:PRE.t, exp: Exp.t, ty: RefTy.t) 
            : VC.t vector = case Exp.node exp of
      Exp.Lambda l => typeCheckLambda (ve,pre,l,ty)
    | _ => 
      let
        (*
         * Γ ⊢ exp => expRefTy
         *)
        val (expvcs,expRefTy) = typeSynthExp (ve,pre,exp)
        (*
         * Γ ⊢ expRefTy <: ty
         *)
        val newvcs = VC.fromTypeCheck (ve,pre,expRefTy,ty)
      in
        Vector.concat [expvcs,newvcs]
      end

  and doItValBind (ve,pre,tyvars,valbind) : (VC.t vector * VE.t) = 
    case valbind of
      Dec.ExpBind (patval,exp) =>
        let
          val (vcs,expty) = typeSynthExp (ve,pre,exp)
        in
          (vcs,elabPatVal (ve,tyvars,patval,expty))
        end
    | Dec.PatBind (pat,expval) =>
        let
          val patnode = Pat.node pat
          (*  unimpl : Exp.Val.ty *)
          val (vcs,expty) = typeSynthExp (ve,pre,Exp.make (Exp.Value expval,
            Type.var $ Tyvar.newNoname {equality=false}))
          (*
           * This function is an unfortunate consequence of having separate
           * Pat.Val and Exp.Val. Unimpl : Coalesce.
           *)
          fun patValToExpVal (patval:Pat.Val.t) : Exp.Val.t = 
            let
              fun patAtomToExpAtom patatom = case patatom of
                  Pat.Val.Const c => Exp.Val.Const c
                | Pat.Val.Var v => Exp.Val.Var (v,Vector.new0 ())
            in
              case patval of
              Pat.Val.Atom (Pat.Val.Wild) => Error.bug "Impossible case wild"
            | Pat.Val.Atom atm => Exp.Val.Atom (patAtomToExpAtom atm)
            | Pat.Val.Tuple atomvec => Exp.Val.Tuple $ Vector.map (atomvec,
                patAtomToExpAtom)
            | Pat.Val.Record atomrec => 
              let
                val lblexpatmvec = Vector.map (Record.toVector atomrec,
                  fn (lbl,atom) => (lbl,patAtomToExpAtom atom))
              in
                Exp.Val.Record $ Record.fromVector lblexpatmvec
              end
            end 
        in
          case patnode of 
            Pat.Value patval => 
            let
              val res = (vcs, elabPatVal (ve,tyvars,patval,expty))
            in
              res
            end
          | Pat.Con {arg : Pat.Val.t option,con,targs} => 
            let
              val rhsvar = case expval of 
                  Exp.Val.Atom (Exp.Val.Var (v,_)) => v
                | _ => Error.bug "A var is expected on rhs for conpat bind."
              val tydargs = Vector.map (targs,Type.toMyType)
              val convid = Var.fromString $ Con.toString con
              val conTyS = VE.find ve convid handle (VE.VarNotFound _) =>
                Error.bug ("Constructor "^(Var.toString convid) ^ 
                  " not found in current env")
              val conTy  = RefSS.toRefTy $ RefTyS.instantiate 
                (conTyS,tydargs)
              (*
               * extend ve with new bindings for matched arguments.
               *)
              val (ve',resTy) = case (conTy,arg) of 
                  (RefTy.Base _,NONE) => (ve,conTy)
                | (RefTy.Base _, SOME _) => Error.bug "Arguments to nullary \
                    \ cons"
                | (RefTy.Arrow _, SOME (Pat.Val.Atom (Pat.Val.Wild))) =>
                    (ve,conTy)
                | (RefTy.Arrow (conArgBind,conResTy), SOME argPatVal) => 
                    let
                      val argExpVal = patValToExpVal argPatVal
                      val (argTyMap,substs) = unifyArgs (conArgBind,argExpVal)
                      val ve' = Vector.fold (argTyMap, ve, fn ((arg,refTy),ve) =>
                        VE.add ve $ (arg,RefTyS.generalize (tyvars,
                          RefSS.fromRefTy refTy)))
                    in
                      (ve', RefTy.applySubsts substs conResTy)
                    end
                | _ => Error.bug "Impossible cons args"
              (*
               * Extend ve' with a new dummy var to keep track of
               * relationship between matched arguments and rhsvar.
               *)
              val RefTy.Base (bv,td,p) = RefTy.alphaRenameToVar resTy rhsvar
              val _ = assert (Var.toString bv = Var.toString rhsvar, 
                "RefTy alpha rename incorrect")
              val newTyS = RefTyS.generalize (tyvars, RefSS.fromRefTy
                  $ RefTy.Base (genVar(), td, p))
            in
              (vcs, VE.add ve'(genVar(), newTyS))
            end
        end

  and doItDecs (ve : VE.t, pre:PRE.t, decs : Dec.t vector) : 
      (VC.t vector * VE.t) =
    let
      fun elabRecDecs (ve : VE.t) (tyvars : Tyvar.t vector)  decs = 
        Vector.fold (decs,ve, 
          fn ({lambda : Lambda.t, var : Var.t},ve) =>
          if VE.mem ve var then ve else (* add trivial ref type *)
            let
              val {arg,argType,body} = Lambda.dest lambda
              val argTyD = Type.toMyType argType
              val bodyTyD = Type.toMyType $ Exp.ty body
              val funTyD = TyD.makeTarrow (argTyD,bodyTyD)
              val funTyS = RefTyS.generalizeAssump (tyvars,
                RefSS.fromRefTy $ RefTy.fromTyD funTyD, true)
            in
              VE.add ve (var,funTyS)
            end)

      fun doItDec (ve : VE.t, pre : PRE.t, dec : Dec.t) : (VC.t vector 
          * VE.t) = case dec of
          Dec.Fun {decs,tyvars} => 
            let
              val extendedVE = elabRecDecs ve (tyvars()) decs
              val vcs = (Vector.concatV o Vector.map) (decs,
                fn ({lambda,var}) => 
                  let
                    val ftys = VE.find extendedVE var
                      handle (VE.VarNotFound _) => Error.bug "ImpossibleCase!"
                    val RefSS.T {prefty = PRf.T {refty=fty,params},
                      ...} = RefTyS.specialize ftys
                    (*
                     * Unimpl. Mutually recursive functions with
                     * parametric types.
                     *)
                    val extendedVE = VE.add (VE.remove ve var) (var,
                      RefTyS.generalize (tyvars(),RefSS.fromRefTy fty))
                    val extendedPRE = Vector.fold (params, pre, 
                      fn ((r,sps),pre) => PRE.addUniterp pre (r, 
                        PTS.simple (empty(),sps)))
                    (*
                     * For recursive function lambdas are checked against 
                     * user-provided type or trivial type.
                     *)
                    val vcs = case RefTyS.isAssumption ftys of
                        false => (print ((Var.toString var)^" will be\
                          \ checked\n"); typeCheckLambda (extendedVE, 
                           extendedPRE, lambda, fty))
                      | true => Vector.new0 ()
                  in
                    vcs
                  end)
            in
              (vcs,extendedVE)
            end
        | Dec.Val {rvbs,tyvars,vbs} => 
            let
              val (rvcs,rvbsVE) = doItDec (ve,pre,
                Dec.Fun {decs=rvbs,tyvars=tyvars})
              val (vcss,vbsVE) = Vector.mapAndFold (vbs, rvbsVE,
                fn ({valbind,...},ve) =>
                  doItValBind (ve,pre,tyvars(),valbind))
            in
              (Vector.concat [rvcs,Vector.concatV vcss] , vbsVE)
            end
        | _ => (Vector.new0 (), ve)

      val (vcsvec,extendedVE) = Vector.mapAndFold (decs,ve, 
        fn (dec,ve) => doItDec (ve,pre,dec))

    in
      (Vector.concatV vcsvec, extendedVE)
    end

  fun doIt (ve, pre, Program.T{decs}) = #1 $ doItDecs (ve, pre, decs)

end

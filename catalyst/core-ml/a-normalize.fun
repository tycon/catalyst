(**
  In this note, elaboration means A-Normalization elaboration.

  Elaborating Expressions
  =======================
  In case of replacing exp with an elaborated let-expression,
  no futher changes need to be done to preserve the scoping of
  tyvars. Eg: In
    let val tyvarseq pat = exp in ... end
  replacing exp with
    let val x = exp1 in exp2 end 
  does not need generalization of free tyvars as all of them are 
  already captured by the the tyvarseq.

  However, if we replace the entire expression with
    let val <?1> var1 = exp1
        val tyvarseq pat = [var1 <?2> /exp1] exp2
     in ... end
   then, 
   1. Free tyvars in exp1 that were previously bound in tyvarseq
      have to be replaced with fresh tyvars, which should then be
      generalized at hole <?1>.
   2. Fresh tyvars bound in <?1> have to be instantiated with tyvars
      from tyvarseq. <?2> is filled with these tyvars.

  Elaborating Patterns
  ====================
  When replacing pats with atomic pats, the method is little 
  different from the above transformation. For eg, while replacing
    let val tyvarseq pat1 = exp1 in ... end
  with
    let val <?1> [x / pat2] pat1 = exp1
        val <?2> pat2 = x <?3>
    in ... end
  <?1> is tyvarseq (since type of x in pat1 is same as pat2). <?2>
  is tyvarseq, where each tyvar is replaced with fresh tyvar and
  <?3> is same as <?2>. Intuitively, we expect <?2> to contain fresh
  tyvars for only those tyvars from tyvarseq that occur free in pat1.
  But this is not the case as tyvars for all varibles defined through
  pat1 are bound in tyvarseq.
**)

functor ANormalize (S : A_NORMALIZE_STRUCTS) : A_NORMALIZE = 
struct
  open S
  structure C = CoreML
  structure A = ANormalCoreML
  structure Var = A.Var
  structure Tyvar = A.Tyvar
  structure Type = A.Type

  fun $ (f,arg) = f arg
  infixr 5 $

  val symbase = "anc_"

  val count = ref 0

  val genVar = fn _ => 
    let val id = symbase ^ (Int.toString (!count))
        val _ = count := !count + 1
    in
      Var.fromString id 
    end

  val inv = fn (x,y) => (y,x)

  
  fun applyTyVarSubsts substs tyvars = Vector.map (tyvars, fn tyvar =>
    Vector.foldr (substs,tyvar,fn ((new,old),tyvar) => 
      if Tyvar.sameName (old,tyvar) then new else tyvar))

  val applyTyVarSubstsInType  = Type.renameTyVars 

  fun doItObjs (objs : 'a vector) (f : 'a -> 'b list * 'c) : 
    ('c vector * 'b list) = 
    Vector.mapAndFold (objs, [], fn (obj, bsacc) =>
        let
          val (bs,obj') = f obj
        in
          (obj', List.concat [bsacc,bs])
        end)

  val varToPatVal = fn var => 
    A.Pat.Val.Atom (A.Pat.Val.Var var)

  val varToExpVal = fn (var,tyvec) => 
    A.Exp.Val.Atom (A.Exp.Val.Var (var,tyvec))

  val tyvarvToTyv = fn tyvars => Vector.map (tyvars, fn tyvar =>
    (Type.var o Tyvar.newLike) tyvar)

  fun getValBindForPat (pat : A.Pat.t) (tyvars : Tyvar.t vector): 
      (Var.t * A.Dec.t) =
    let
      open A
      val patty = Pat.ty pat
      val newvar = genVar ()
      val newtyvars = Vector.map (tyvars, fn tyvar =>
        Tyvar.newNoname {equality = (Tyvar.isEquality tyvar)})
      val tyvarsubsts = Vector.zip (newtyvars,tyvars)
      val newpatty = applyTyVarSubstsInType tyvarsubsts patty
      (* 
       * Bug : patty might contain old tyvars
       * Fixed: newpatty.
       *)
      val subexpval = varToExpVal (newvar,
        Vector.map (newtyvars, fn tyv => Type.var tyv))
      val subexp = Exp.make (Exp.Value (subexpval), newpatty)
      val valbind = Dec.PatBind (pat,subexpval)
      val spatvb = Vector.new1 ({valbind = valbind, 
        lay = fn _ => Layout.empty, nest = []})
      val spatdec = Dec.Val ({rvbs = Vector.fromList [],
        tyvars = fn _ => newtyvars, vbs = spatvb})
    in
      (newvar, spatdec)
    end

  fun doItPatToVar (pat : C.Pat.t) (tyvars : Tyvar.t vector) : 
      (A.Pat.Val.atom * A.Dec.t list) =
    let
      open A
      val patty = C.Pat.ty pat
      val (spat,sspatdecs) = doItPat pat tyvars
      val spatty = Pat.ty spat
      val (atom,spatdecs) = case Pat.node spat of
          Pat.Value (Pat.Val.Atom v) => (v, sspatdecs)
        | _ => 
          let
            val (newvar, spatdec) = getValBindForPat spat tyvars
            val atom = Pat.Val.Var newvar
          in
            (atom,spatdec::sspatdecs)
          end
    in
      (atom, spatdecs)
    end

  and doItPatToAtomicPat (pat : C.Pat.t) (tyvars : Tyvar.t vector) : 
      (A.Pat.Val.t * A.Dec.t list) =
    let
      open A
      val patty = C.Pat.ty pat
      val (spat,sspatdecs) = doItPat pat tyvars
      val spatty = Pat.ty spat
      val (value,spatdecs) = case Pat.node spat of
          Pat.Value v => (v, sspatdecs)
        | _ => 
          let
            val (newvar, spatdec) = getValBindForPat spat tyvars
            val value = Pat.Val.Atom (Pat.Val.Var newvar)
          in
            (value,spatdec::sspatdecs)
          end
    in
      (value, spatdecs)
    end


  (*
   * doItPat takes complex pattern of form C(C<x>), splits it
   * and returns (C(t0), [{rvbs = [], tyvars = newtyvarseq, 
   *  vbs = [{pat=C<x>, exp = t0(newtyvarseq), lay = newlay,
   *    nest = ""}] }]).
   * The type of (exp t0) should be same as type of pattern C<x>.
   * Length of newtyvarseq = Length of tyvars.
   *)
  and doItPat (pat : C.Pat.t) (tyvars : Tyvar.t vector) : 
      (A.Pat.t * A.Dec.t list)  = 
    case C.Pat.node pat of
        C.Pat.Con ({arg = NONE, con, targs}) => 
          let
            val patty = C.Pat.ty pat
            val patnode = A.Pat.Con ({arg = NONE, con = con, 
              targs = targs})
          in
            (A.Pat.make (patnode,patty),[])
          end
      | C.Pat.Con ({arg = SOME arg,con,targs}) => 
        let
          open A
          val patty = C.Pat.ty pat
          val (value,subpatdecs) = doItPatToAtomicPat arg tyvars 
          val pat = Pat.Con ({arg = SOME value, con = con, 
            targs = targs})
        in
          (Pat.make (pat,patty), subpatdecs)
        end
      | C.Pat.Layered (var,pat') =>
        let
          val patty = C.Pat.ty pat
          val (spat,spatdecs) = doItPat pat' tyvars
        in
          raise (Fail "Unimpl layered patterns")
        end
      | C.Pat.List argvec =>
        let
          (*Convert it to cons application*)
          open C
          val patty = Pat.ty pat
          val conopt = Type.deConOpt patty
          val targs = case conopt of SOME (_,tv) => tv
            | _ => Error.bug "List type expected. Got something else."
          val nilpat = Pat.make (Pat.Con {arg = NONE, con = Con.nill,
            targs = targs}, patty)
          fun makeConsPat (arg,rest) = 
            let
              val argpatnode = Pat.Tuple (Vector.new2 (arg,rest))
              val argpatty = Type.tuple (Vector.new2 (Pat.ty arg,
                Pat.ty rest))
              val argpat = Pat.make (argpatnode,argpatty)
            in
              Pat.make (Pat.Con {arg = SOME argpat, con = Con.cons, 
                targs = targs},patty)
            end
          val conspat = Vector.foldr (argvec,nilpat,makeConsPat)
        in
          doItPat conspat tyvars
        end
      | C.Pat.Record (patrec) => 
        let
          open A
          val patty = C.Pat.ty pat
          val (atomlist, subpatdecslist) = Vector.foldr(Record.toVector patrec, 
            ([],[]), fn ((lbl,pat), (atoms,spatdecslist)) =>
              let
                val (atom,spatdecs) = doItPatToVar pat tyvars
              in
                ((lbl,atom)::atoms, spatdecs::spatdecslist)
              end)
          val atomrec = Record.fromVector (Vector.fromList atomlist)
          val patnode = Pat.Value (Pat.Val.Record atomrec)
        in
          (Pat.make (patnode,patty), List.concat subpatdecslist)
        end
      | C.Pat.Tuple argvec => (case Vector.length argvec of
          (* optimization for unit tuples *)
          1 => doItPat (Vector.sub (argvec,0)) tyvars
        | _ => 
          let
            open A
            val patty = C.Pat.ty pat
            val (atomlist, subpatdecslist) = Vector.foldr (argvec, ([],[]), 
              fn (arg, (atoms,spatdecslist)) => 
                let
                  val (atom,spatdecs) = doItPatToVar arg tyvars
                in
                  (atom::atoms, spatdecs::spatdecslist)
                end)
            val patnode = Pat.Value (Pat.Val.Tuple (Vector.fromList atomlist))
          in
            (Pat.make (patnode,patty),List.concat subpatdecslist)
          end)
      | C.Pat.Var var => 
        let
          val patty = C.Pat.ty pat
          val patnode = A.Pat.Value (A.Pat.Val.Atom (A.Pat.Val.Var var))
        in
          (A.Pat.make (patnode,patty), [])
        end
      | C.Pat.Wild => (A.Pat.make (A.Pat.Value $ A.Pat.Val.Atom A.Pat.Val.Wild, 
          C.Pat.ty pat),[])
      | C.Pat.Const cth =>
        let
          val patty = C.Pat.ty pat
          val patnode = A.Pat.Value (A.Pat.Val.Atom 
            (A.Pat.Val.Const (cth())))
        in
          (A.Pat.make (patnode,patty), [])
        end

  fun applyTyVarSubstsInLam substs (lam : A.Lambda.t) =
    let
      open A
      val {arg,argType,body} = Lambda.dest lam
      val argType' = applyTyVarSubstsInType substs argType
      val body' = applyTyVarSubstsInExp substs body
    in
      Lambda.make {arg = arg, argType = argType', body = body'}
    end
      
  and applyTyVarSubstsInExpVal substs (expval : A.Exp.Val.t) =
    let
      open A.Exp
      fun applyTyVarSubstsInExpAtom substs (expatom : A.Exp.Val.atom) =
        case expatom of
          Val.Var (v,tyv) => Val.Var (v,Vector.map (tyv,
            applyTyVarSubstsInType substs))
        | _ => expatom
    in
      case expval of
        Val.Atom atom => Val.Atom (applyTyVarSubstsInExpAtom substs atom)
      | Val.Tuple atomv => Val.Tuple (Vector.map (atomv,
          applyTyVarSubstsInExpAtom substs))
      | Val.Record atomrec => (Val.Record o A.Record.fromVector) (Vector.map (
          A.Record.toVector atomrec, fn (lbl,atom) => (lbl,
            applyTyVarSubstsInExpAtom substs atom)))
    end

  and applyTyVarSubstsInExp (substs : (Tyvar.t * Tyvar.t) vector)
    (exp : A.Exp.t) : A.Exp.t = 
    let
      open A
      val expty = Exp.ty exp
      val expnode = Exp.node exp
      val newexpty = applyTyVarSubstsInType substs expty
      val newexpnode = case expnode of
        Exp.App (Exp.Val.Var (f,tyargs),arg) =>
          Exp.App (Exp.Val.Var (f, Vector.map (tyargs, 
            applyTyVarSubstsInType substs)), arg)
      | Exp.Case {kind,lay,nest,rules,test} =>
        let
          val newrules = Vector.map (rules, fn {exp,lay,pat} =>
            {exp = applyTyVarSubstsInExp substs exp, lay=lay, pat=pat})
          val newtest = applyTyVarSubstsInExpVal substs test
        in
          Exp.Case {kind=kind, lay=lay, nest=nest,
            rules=newrules, test=newtest}
        end
      | Exp.Lambda lam => Exp.Lambda (applyTyVarSubstsInLam substs lam)
      | Exp.Let (decs,exp) => 
        let
          val newdecs = Vector.map (decs, applyTyVarSubstsInDec substs)
          val newexp = applyTyVarSubstsInExp substs exp
        in
          Exp.Let (newdecs,newexp)
        end
      | Exp.PrimApp {args,prim,targs} => 
        let
          val newargs = Vector.map (args, applyTyVarSubstsInExpVal substs)
          val newtargs = Vector.map (targs, applyTyVarSubstsInType substs)
        in
          Exp.PrimApp {args = newargs, prim = prim, targs = newtargs}
        end
      (*| Exp.Raise v => Exp.Raise (applyTyVarSubstsInExpVal substs
      v)*)
      | Exp.Seq expv => Exp.Seq (Vector.map (expv, 
          applyTyVarSubstsInExp substs))
      | Exp.Value v => Exp.Value (applyTyVarSubstsInExpVal substs v)
      | rest => rest
    in
      Exp.make (newexpnode,newexpty)
    end

  and applyTyVarSubstsInDec substs dec =
    let
      open A
      (*
       * foreach [new/old] in substs, If old \in tyvars, then 
       * remove that substitution. Else, if new \in tyvars, then
       * our new tyvar generator is not generating fresh names, so
       * raise error. Else, retain the substitution.
       *)
      fun freeSubsts substs tyvars = 
        Vector.fromList (Vector.foldr (substs, [], 
          fn ((new,old),substs) => 
            if Vector.exists (tyvars, fn tyvar => Tyvar.sameName (tyvar,new)) 
            then Error.bug "new tyvar gen unsound"
            else if Vector.exists (tyvars, 
              fn tyvar => Tyvar.sameName (tyvar,old))
            then substs
            else (new,old) :: substs))
      fun applyTyVarSubstsInValBind substs (valbind : A.Dec.valbind) =
        case valbind of
          Dec.ExpBind (patval,exp) => Dec.ExpBind (patval, 
            applyTyVarSubstsInExp substs exp)
        | Dec.PatBind (pat,expval) => Dec.PatBind (pat,
            applyTyVarSubstsInExpVal substs expval)
    in
      case dec of
        Dec.Fun {decs,tyvars} =>
        let
          val substs = freeSubsts substs (tyvars())
          val newdecs = Vector.map (decs, fn {lambda,var} => 
            {lambda = applyTyVarSubstsInLam substs lambda, var = var})
        in
          Dec.Fun {decs = newdecs, tyvars = tyvars}
        end
      | Dec.Val {rvbs,tyvars,vbs} => 
        let
          val substs = freeSubsts substs (tyvars())
          val rvbs' = Vector.map (rvbs, fn {lambda,var} => 
            {lambda = applyTyVarSubstsInLam substs lambda, var = var})
          val vbs' = Vector.map (vbs, fn {valbind,lay,nest} =>
            {valbind = applyTyVarSubstsInValBind substs valbind,
               lay = lay, nest = nest})
        in
          Dec.Val {rvbs = rvbs', tyvars = tyvars, vbs = vbs'}
        end
      | _ => dec
    end

  fun getValBindForExp (exp : A.Exp.t) (tyvars : Tyvar.t vector) :
    (A.Dec.t * Var.t) =
    let
      open A
      val expty = Exp.ty exp
      val newvar = genVar ()
      val newtyvars = Vector.map (tyvars, fn tyvar =>
        Tyvar.newNoname {equality = (Tyvar.isEquality tyvar)})
      val tyvarsubsts = Vector.zip (newtyvars,tyvars)
      val newexp = applyTyVarSubstsInExp tyvarsubsts exp
      val newexpty = applyTyVarSubstsInType tyvarsubsts expty
      val subpatval = varToPatVal newvar
      val subpat = Pat.make (Pat.Value (subpatval),newexpty)
      val subvalbind = Dec.ExpBind (subpatval,newexp)
      (*
       * Bug: exp and expty still refer to old tyvars
       * Fixed. We now have newexp and newexpty.
       *)
      val vbs = Vector.new1 ({valbind = subvalbind,
        lay = fn _ => Layout.empty, nest = []})
      val dec = Dec.Val ({rvbs = Vector.fromList [],
        tyvars = fn _ => newtyvars, vbs = vbs})
    in
      (dec, newvar)
    end

  fun doItExpToVar (exp : C.Exp.t) (tyvars : Tyvar.t vector) : 
    (A.Dec.t list * Var.t) = 
    let
      (* Never transparent as doItExp returns var*tyvec *)
      val (ssexpdecs,sexp) = doItExp exp tyvars
      val (sexpdec,newvar) = getValBindForExp sexp tyvars
      val sexpdecs = List.concat [ssexpdecs, [sexpdec]]
    in
      (sexpdecs, newvar)
    end

  and doItExpToValue (exp : C.Exp.t) (tyvars : Tyvar.t vector) : 
    (A.Dec.t list * A.Exp.Val.t) = 
    let
      val (ssexpdecs,sexp) = doItExp exp tyvars
      val sexpty = A.Exp.ty sexp
      val (sexpdec,value) = case A.Exp.node sexp of
          A.Exp.Value v => ([],v)
        | _ => 
          let
            val (sexpdec,newvar) = getValBindForExp sexp tyvars
            val value = varToExpVal (newvar,tyvarvToTyv tyvars)
          in
            ([sexpdec], value)
          end
      val sexpdecs = List.concat [ssexpdecs,sexpdec]
    in
      (sexpdecs,value)
    end

  and doItExp (exp : C.Exp.t) (tyvars : Tyvar.t vector) : 
    (A.Dec.t list * A.Exp.t) = 
    let
      val expty = C.Exp.ty exp
      val expnode = C.Exp.node exp
      open A
    in
      case expnode of
          C.Exp.App (e1,e2) => 
          let
            val (fdecs, f) = doItExpToVar e1 tyvars
            val Exp.Val.Atom fvar = varToExpVal (f,tyvarvToTyv tyvars)
            val ftyargv = tyvarvToTyv tyvars
            val (argdecs,argval) = doItExpToValue e2 tyvars
            val expnode = Exp.App (fvar, argval)
          in
            (List.concat [fdecs,argdecs], Exp.make (expnode,expty))
          end
        | C.Exp.Case {kind, lay, nest, noMatch, rules, test, ...} =>
          (*
           * A correct elaboration of case-match statement should be done in
           * following steps:
           * 1. Let wildexp be expression bound to wild card pattern match.
           * 2. For each rule in rules, let exp be corresponding Exp.t, 
           *  2.1 Elaborate the match pattern to get an ordered (Var.t,Pat.t) list.
           *  2.2 Fold over the list constructing a nested case-match statement
           *      with Var.t as test and with two rules - first rule with Pat.t as match
           *      and second rule with wild card match. For wild card match,
           *      corresponding Exp.t is wildexp <?1><?2>. 
           *  2.3 For first match in innermost case-match expression, make exp
           *      as corresponding match expression.
           * 3. Collapse rules with equivalent match expressions at the toplevel - 
           *    3.1 Start unifying pats in nested match expressions. Whenever there
           *        is a failure, add a new rule with unmatched expression.
           * <?1> Make sure that freevars of wildexp are not captured by
           *      bindings introduced in pat-match
           * <?2> This duplication is semantics-preserving even in presence of 
           *      side-effects. Why? Despite duplication, wildexp is executed only
           *      once - when all cases fail - i.e., precisely when it is executed in
           *      unelaborated expression.
           *)
          (*
           * However, we are not going to do this. For every rule, we will naively 
           * elaborate Pat.t and construct a LET expression with resultant
           * bindings and Exp.t. We will also not collapse duplicate
           * case-matches at the top level. The reason why this naive
           * translation works for our analysis is because 
           * 1. case-matches and let bindings are treated similarly - as assumptions 
           *    constituting the typing context - by our analysis. Therefore, a
           *    nested case-match expression and a series of let bindings, both
           *    result in same stack of assumptions. 
           * 2. We are going to analyze all the rules anyway.
           * 
           *)
          let
            val (predecs, testval) = doItExpToValue test tyvars
            val newrules = Vector.map (rules, fn ({exp,lay,pat}) =>
              let
                (* 
                 * tyvars are for the dec which binds current expression. We are
                 * not going to let patdecs/expdecs escape. So we don't pass tyvars to 
                 * doItPat/doItExp.
                 *)
                val (newpat,patdecs) = doItPat pat (Vector.fromList [])
                val (mexpdecs, mexp) = doItExp exp (Vector.fromList [])
                val mexpnode = Exp.node mexp and mexpty = Exp.ty mexp
                val newexpnode = case (patdecs,mexpdecs,mexpnode) of
                    ([],[],_) => mexpnode
                  | (_,_,Exp.Let (decv,e)) => (case mexpdecs of [] => 
                        Exp.Let (Vector.concat [Vector.fromList patdecs, decv],e)
                      | _ => Error.bug ("Assumption abt let failed."))
                  | _ => Exp.Let (Vector.concat [Vector.fromList patdecs, 
                        Vector.fromList mexpdecs], mexp)
                (* The above transformation should be type preserving. *)
                val newexpty = mexpty 
                val newexp = Exp.make (newexpnode,newexpty)
                val newrule = {exp = newexp, lay = lay, pat = newpat}
              in
                newrule
              end)
            val expnode = Exp.Case {kind = kind, lay = lay, nest = nest, 
              rules = newrules, test = testval}
          in
            (predecs, Exp.make (expnode,expty))
          end
        | C.Exp.Lambda lam => ([], Exp.make (Exp.Lambda (doItLambda lam), 
            expty))
        | C.Exp.Var (fth,tyvth) => ([], Exp.make (Exp.Value ( varToExpVal
            (fth(),tyvth())), expty))
        | C.Exp.Seq expv => 
          let
            val (predecs,newexpv) = doItExps expv tyvars
          in
            (predecs, Exp.make (Exp.Seq newexpv,expty))
          end
        | C.Exp.Let (decv,exp) =>
          let
            val (predecs,newexp) = doItExp exp tyvars
            val newdecv = Vector.concat [doItDecs decv, Vector.fromList predecs]
            val newexpnode = Exp.Let (newdecv,newexp)
          in
            ([], Exp.make (newexpnode,expty))
          end
        | C.Exp.List expv => 
          let
            (*Convert it to cons application*)
            open C
            val conopt = Type.deConOpt expty
            val targs = case conopt of SOME (_,tv) => tv
              | _ => Error.bug "List exp type expected. Got something else."
            val nilexp = Exp.make (Exp.Con (Con.nill,targs), expty)
            fun makeConsAppExp (arg,rest) = 
              let
                val argexpnode = (Exp.Record o Record.tuple) 
                  (Vector.new2 (arg,rest))
                val argexpty = Type.tuple (Vector.new2 (Exp.ty arg,
                  Exp.ty rest))
                val consexpty = Type.arrow (argexpty,expty)
                val consexp = Exp.make (Exp.Con (Con.cons,targs),consexpty)
                val argexp = Exp.make (argexpnode,argexpty)
                val consAppExpNode = Exp.App (consexp,argexp)
                val consAppExpTy = expty
              in
                Exp.make (consAppExpNode,consAppExpTy)
              end
            val consAppExp = Vector.foldr (expv,nilexp,makeConsAppExp)
          in
            doItExp consAppExp tyvars
          end
        | C.Exp.Record exprec => 
          let
            val (lblvarv, predecs) = doItObjs (Record.toVector exprec) 
              (fn (lbl,exp) => 
                let
                  val (predecs, var) = doItExpToVar exp tyvars
                in
                  (predecs, (lbl,var))
                end)
            val atomrec = Record.fromVector (Vector.map (lblvarv,
              fn (lbl,var) => (lbl,Exp.Val.Var (var,tyvarvToTyv tyvars))))
            val expnode = Exp.Value (Exp.Val.Record atomrec)
          in
            (predecs, Exp.make (expnode,expty))
          end
        | C.Exp.Const cth => ([], Exp.make (Exp.Value (Exp.Val.Atom (
            Exp.Val.Const (cth()))) ,expty))
        | C.Exp.Raise exp => ([], Exp.make (Exp.Nop, expty))
          (*let
            val (predecs,value) = doItExpToValue exp tyvars
            val expnode = Exp.Raise value
          in
            (predecs, Exp.make (expnode,expty))
          end*)
        | C.Exp.EnterLeave (exp,si) =>
          let
            val (predecs,value) = doItExpToValue exp tyvars
            val expnode = Exp.EnterLeave (value,si)
          in
            (predecs, Exp.make(expnode, expty))
          end
        | C.Exp.Con (c,tv) => 
          (*
           * We treat con as any other variable henceforth
           *)
          let
            val var = Var.fromString (Con.toString c)
            val expnode = Exp.Value (varToExpVal (var,tv))
          in
            ([],Exp.make(expnode,expty))
          end
        | C.Exp.Handle ({catch,handler,try}) => 
          let
            val (predecs1,handler') = doItExp handler tyvars
            val (predecs2,try') = doItExp try tyvars
            val expnode = Exp.Handle ({catch = catch, handler = handler', 
              try = try'})
          in
            (List.concat [predecs1,predecs2], Exp.make (expnode,expty))
          end
        | C.Exp.PrimApp ({args, prim, targs}) =>
          let
            val (predecs,valv) = inv (doItObjs args (fn arg => 
              doItExpToValue arg tyvars))
            val expnode = Exp.PrimApp ({args = valv, prim = prim, 
              targs = targs})
          in
            (predecs, Exp.make(expnode,expty))
          end
    end

  and doItExps (exps : C.Exp.t vector) (tyvars : Tyvar.t vector) : 
      (A.Dec.t list * A.Exp.t vector) =
      inv (doItObjs exps (fn exp => doItExp exp tyvars))

  and doItLambda (lam : C.Lambda.t) : A.Lambda.t  = 
    let
      val {arg,argType,body,mayInline} = C.Lambda.dest lam
      val bodyTy = C.Exp.ty body
      (*
       * All the tyvars free in bodyexp are already bound 
       * at the enclosing dec for this lambda. No need to
       * generalize any tyvars.
       *)

      val tyvars = Vector.fromList []
      val (newdecs,newexp) = doItExp body tyvars
            val newbody = case newdecs of
          [] => newexp 
        | _ =>  A.Exp.make (A.Exp.Let (Vector.fromList newdecs, 
          newexp),bodyTy)
    in
      A.Lambda.make {arg = arg, argType = argType, 
        body = newbody}
    end

  and doItDec (dec:C.Dec.t) : A.Dec.t list = case dec of
      C.Dec.Datatype r => [A.Dec.Datatype r]
    | C.Dec.Exception r => [A.Dec.Exception r]
    | C.Dec.Fun {decs = lamvec, tyvars} =>
        let val lv' = Vector.map (lamvec, fn {lambda,var} => 
          {lambda = doItLambda lambda,var=var})
        in
          [A.Dec.Fun {decs = lv',tyvars = tyvars}]
        end
    | C.Dec.Val {rvbs, vbs, tyvars, ...} =>
        let 
          val rvbs' = Vector.map (rvbs, fn {lambda,var} => 
            {lambda = doItLambda lambda, var = var})
          (*
           * for each vb in vbs, generate a vb' with simplified pattern
           * and simplified expression, while extracting subexpvbs and 
           * subpatvbs
           *)
          val  (subexpdecs, vbs', subpatdecs)= Vector.fold (vbs, ([],[],[]),
            fn ({exp,lay,nest,pat, ...},(pre, cur, post)) => 
              let
                (*
                 * The following code does not typecheck.
                val (elabExp,elabPat,binder) = case C.Pat.node pat of
                    C.Pat.Con _ => (doItExpToValue,doItPat,A.Dec.PatBind)
                  | C.Pat.List _ => (doItExpToValue,doItPat,A.Dec.PatBind)
                  | C.Pat.Layered _ => (doItExpToValue,doItPat,A.Dec.PatBind)
                  | _ => (doItExp,doItPatToAtomicPat,A.Dec.ExpBind)
                 * This makes a case for existential typing. In presence of
                 * existential types, the above expression could be given the
                 * following type:
                 * ∃ T1,T2. (f: Exp.t → T1, g : Pat.t → T2, h : T1*T2 → Exp.valbind))
                 *)
                fun doItPatBind (pat,exp) =
                  let
                    val (subexpdecs,newexpval) = doItExpToValue exp (tyvars())
                    val (subpat,subpatdecs) = doItPat pat (tyvars())
                    val valbind = A.Dec.PatBind (subpat,newexpval)
                  in
                    (subexpdecs,valbind,subpatdecs)
                  end
                fun doItExpBind (pat,exp) =
                  let
                    val (subexpdecs,newexp) = doItExp exp (tyvars())
                    val (subpatval,subpatdecs) = doItPatToAtomicPat pat (tyvars())
                    val valbind = A.Dec.ExpBind (subpatval,newexp)
                  in
                    (subexpdecs,valbind,subpatdecs)
                  end
                val doItValBind = case C.Pat.node pat of
                    C.Pat.Con _ => doItPatBind
                  | C.Pat.List _ => doItPatBind
                  | C.Pat.Layered _ => doItPatBind
                  | _ => doItExpBind
                val (subexpdecs,valbind,spatdecs) = doItValBind (pat,exp)
                val vb' = {valbind = valbind, lay = lay, nest = nest}
              in
                (List.concat [subexpdecs,pre], 
                 List.concat [cur,[vb']],
                 List.concat [spatdecs,post])
              end)
          val expdec = [A.Dec.Val ({rvbs = rvbs', tyvars = tyvars,
              vbs = Vector.fromList vbs'})]
        in
          List.concat [subexpdecs, expdec, subpatdecs]
        end

  and doItDecs decs =
    let
      val decss = Vector.toListMap (decs,doItDec)
      val decsv = Vector.fromList (List.concat decss)
    in
      decsv
    end

  val doIt = fn (C.Program.T{decs}) => A.Program.T{decs = doItDecs decs}
end

functor SpecLang (S : SPEC_LANG_STRUCTS) : SPEC_LANG = 
struct
  open S
  structure L = Layout
  structure TyD = TypeDesc

  fun $ (f,arg) = f arg
  infixr 5 $
  val assert = Control.assert
  fun varStrEq (v1,v2) = (Var.toString v1 = Var.toString v2)
  fun varSubst (subst as (new,old)) v = if varStrEq (v,old) 
    then new else v
  fun vecToStr f vec = case Vector.length vec of
    0 => "" | _ => " "^(Vector.toString f vec)
  val empty = fn () => Vector.new0 ()
  fun mkMapper eqns cmp restore = fn t' => 
    case List.peekMap (eqns, 
      fn (t,ts) => if cmp (t,t') then SOME ts else NONE) of
        SOME ts => ts | NONE => restore t'

  structure Tyvar =
  struct
    open Tyvar
    val equal= fn (t,t') => toString t = toString t'
    val eq = equal
  end

  structure RelId =
  struct
    open Var
    val equal= fn (r1,r2) => toString r1 = toString r2
    val eq = fn (r1,r2) => toString r1 = toString r2
  end

  structure SVar = 
  struct
    type t = Var.t

    val symbase = "'t"

    val count = ref 0

    val new = fn _ => 
      let val id = symbase ^ (Int.toString (!count))
          val _ = count := !count + 1
      in
        Var.fromString id 
      end

    fun eq (v1,v2) = (Var.toString v1 = Var.toString v2)
    val toString = Var.toString 
  end

  structure TupSort =
  struct
    datatype tT = T of TypeDesc.t | S of SVar.t
    datatype t = Tuple of tT list
    datatype cs = Eq of t*t 

    fun tTToString (T tyd) = TyD.toString tyd
      | tTToString (S svar) = SVar.toString svar

    fun toString (Tuple tts) = List.toString tTToString tts

    val toString = fn t => "{"^(toString t)^"}" 

    val fromSVar = fn t => Tuple [S t]

    val getSVars = fn (Tuple ts) => List.keepAllMap (ts,
      fn tT => case tT of S t => SOME t | _ => NONE)

    fun tTeq (T tyd1,T tyd2) = TyD.sameType (tyd1,tyd2)
      | tTeq (S v1,S v2) = SVar.eq (v1,v2)
      | tTeq _ = false

    fun equal (Tuple tydv1, Tuple tydv2) = 
      (List.length tydv1 = List.length tydv2) andalso
      List.forall2 (tydv1,tydv2,tTeq)
      
    fun unionType (t1 as Tuple tydv1, t2 as Tuple tydv2) =
      case (List.length tydv1, List.length tydv2) of
        (0,_) => ([],t2) | (_,0) => ([],t1)
      | (n1,n2) => if equal (t1,t2) then ([],t1)
          else ([Eq (t1,t2)],t1)

    fun crossPrdType (t1 as Tuple tyds1, t2 as Tuple tyds2) =
      case (List.length tyds1, List.length tyds2) of
        (0,_) => ([],t1) | (_,0) => ([],t2) 
      | _ => ([],Tuple $ List.concat [tyds1,tyds2])

    fun instSVars (Tuple ttl) mapSVar  = Tuple $ List.concat $ 
      List.map (ttl, fn tt => case tt of T _ => [tt] | S t => 
        (fn (Tuple ttl') => ttl') (mapSVar t))

    fun mapTyD (Tuple ttl) f = Tuple (List.map (ttl, fn tt =>
      case tt of T tyd => T $ f tyd | _ => tt))

    (*
     * Tuple Sort constraint solving
     *)
    val eq = fn (Eq x) => x

    type sol = (SVar.t * t)

    fun trySolveConstraint (c : cs) : sol option =
      let
        val len = List.length
        fun assertNotCirc (v,rt) = 
          let
            val rhsvs = getSVars rt
            val cStr = fn _ => (SVar.toString v) ^ " = "
              ^ (toString rt)
            val _ = assert (List.forall (rhsvs, fn rhsv =>
              not $ SVar.eq (v,rhsv)), "Unsolvable circular\
                \ relty constraint: " ^ (cStr ()))
          in
            ()
          end
        val newEq = case eq c of
            (*
             * An empty set is sub-set of every set. So, comparing a
             * tuple sort with empty sort will yeild no new
             * information.
             *)
            (Tuple [S v1], Tuple []) => NONE
          | (Tuple [], Tuple [S v2]) => NONE
          | (Tuple [S v1], rty2) => (assertNotCirc (v1,rty2);
              SOME (v1,rty2))
          | (rty1, Tuple [S v2]) => (assertNotCirc (v2,rty1);
              SOME (v2,rty1))
          | _ => NONE
      in
        newEq
      end

    fun clearTautologies (cs : cs list) : cs list =
      let
        val taut = fn (tt1,tt2) => case (tt1,tt2) of
            (T tyd1,T tyd2) => (assert (TyD.sameType (tyd1,tyd2),
              "Typechecking rexpr failed"); true)
          | (S v1, S v2) => SVar.eq (v1,v2)
          | _ => false
      in
        List.keepAll (cs, fn (Eq (Tuple ts1,Tuple ts2)) =>
          case (ts1,ts2) of ([tt1],[tt2]) => not $ taut (tt1,tt2)
          | _ => true)
      end
   
    (*
     * Applies reltyvar eqn (v=rt) in cs. 
     * Post-condition : v does not occur in cs. 
     *)
    fun applyRelTyVarEqn eqn (cs :cs list) : cs list =
      List.map (cs, fn (Eq(rt1,rt2)) => 
        let
          val mapSVar = mkMapper [eqn] SVar.eq fromSVar
          val rt1' = instSVars rt1 mapSVar
          val rt2' = instSVars rt2 mapSVar
        in
          Eq (rt1',rt2')
        end)

    fun elabcs (cs : cs list) : cs list = 
      let
        val len = List.length
        val {no,yes} = List.partition (cs, 
          fn (Eq (Tuple ts1,Tuple ts2)) => len ts1 = len ts2)
        val yes' = case yes of [] => raise (Fail "unsolvable tupsort\
          \ constraints") 
          | (Eq (Tuple ts1,Tuple ts2))::yes' => List.concat [
              List.map2 (ts1,ts2, fn (t1,t2) => 
                Eq (Tuple [t1], Tuple [t2])), 
              yes']
      in
        List.concat [yes',no]
      end
    
    (*
     * Solves a constraint, applies the solution to the rest, and
     * clears any new tautologies. Repeats this process until there are
     * no more constraints, or residue is unsolvable.
     * Invariant : 
     * sol : {v:Reltyvar} -> {rt:TupSort.t | v notin TupSort.relTyVarsIn rt}
     * , is a partial function.
     *)
    local
      exception Return of sol list
    in
      fun relTyVarEqns (cs : cs list) : sol list =
        let
          val cs = clearTautologies cs
          val _ = case cs of [] => raise (Return [])
            | _ => ()
          val solEqnOp = Vector.loop (Vector.fromList cs, 
            fn c => case trySolveConstraint c of
                SOME sol => SOME $ SOME sol
              | _ =>NONE, 
            fn () => NONE)
          val (eqns,residue) = case solEqnOp of 
            NONE => ([], elabcs cs)
          | SOME (solEqn as (t,ts))=> 
            let
              val newcs = applyRelTyVarEqn solEqn cs
              val moreEqns = relTyVarEqns newcs
              val moreEqFn = mkMapper moreEqns SVar.eq fromSVar
              (* Account for transitive dependencies *)
              val solEqn' = case moreEqns of [] => solEqn 
                | _ => (t,instSVars ts moreEqFn)
            in
              (solEqn'::moreEqns,[])
            end
          val newEqns = relTyVarEqns residue
          val newEqFn = mkMapper newEqns SVar.eq fromSVar
          val eqns' = case newEqns of [] => eqns
            | _ => List.map (eqns, fn (t,ts) =>
              (t, instSVars ts newEqFn))
        in
          List.concat [eqns', newEqns]
        end handle Return l => l
    end

    (*
     * Solves constraints and returns solution.
     *)
    fun solvecs cs = mkMapper (relTyVarEqns cs) SVar.eq fromSVar

  end

  structure TS = TupSort

  structure SimpleProjSort =
  struct
    datatype t = ColonArrow of TypeDesc.t * TupSort.t

    fun toString (ColonArrow (tyD,tupsort)) =
      (TyD.toString tyD)^" :-> "^(TupSort.toString tupsort)

    fun layout t = L.str $ toString t

    fun new (tyd,rt) = ColonArrow (tyd,rt)

    fun domain (ColonArrow (d,_)) = d

    fun range (ColonArrow (_,r)) = r
   
    fun mapTyD (ColonArrow (tyd,ts)) f =
      ColonArrow (f tyd, TS.mapTyD ts f)
  end

  structure SPS = SimpleProjSort

  structure ProjSort =
  struct
    datatype t =  T of {paramsorts : SimpleProjSort.t vector,
                              sort : SimpleProjSort.t}
    fun toString (T {paramsorts,sort}) = 
      case Vector.length paramsorts of
      0 => SimpleProjSort.toString sort
    | _ => (Vector.toString SimpleProjSort.toString paramsorts) 
      ^ " :-> " ^ (SimpleProjSort.toString sort)

    fun new (paramsorts, sort) = T {paramsorts = paramsorts,
      sort = sort}

    fun simple sps = T {paramsorts = Vector.new0 (), 
      sort = sps}

    fun domain (T{sort, ...}) = SimpleProjSort.domain sort

    fun range (T{sort, ...}) = SimpleProjSort.range sort
  end

  structure PS = ProjSort

  structure ProjSortScheme =
  struct
    datatype t = T of {svars : SVar.t vector,
                        sort : ProjSort.t}

    fun toString (T{svars,sort}) = 
      (Vector.toString SVar.toString svars)^". "^
        (ProjSort.toString sort)

    fun simple sps = T {svars = Vector.new0 (),
      sort = ProjSort.simple sps}

    fun generalize (svars,s) = T {svars=svars, sort=s} 

    fun instantiate (T {svars,sort}, tsv) =
      let
        val svarMap = Vector.zip (svars,tsv) handle _ =>
          Error.bug "PSS inst error1"
        val mapSVar = fn t => case Vector.peekMap (svarMap, 
          fn (t',ts) => if SVar.eq (t,t') then SOME ts else NONE) of
            SOME ts => ts | NONE => Error.bug "PSS inst error2"
        val PS.T {paramsorts,sort = SPS.ColonArrow (tyd,ts)} = sort
        val sort' = SPS.ColonArrow (tyd, TS.instSVars ts mapSVar)
        val ps' = Vector.map (paramsorts, 
          fn (SPS.ColonArrow (tyd,ts)) => SPS.ColonArrow (tyd,
            TS.instSVars ts mapSVar))
      in
        PS.T {paramsorts=ps', sort=sort'}
      end
  end

  structure PSS = ProjSortScheme

  structure ProjTypeScheme =
  struct
    datatype t = T of {tyvars : Tyvar.t vector,
                       sortscheme : ProjSortScheme.t}
    
    fun toString (T {tyvars, sortscheme}) = 
        (Vector.toString Tyvar.toString tyvars) 
      ^ ". " ^ (ProjSortScheme.toString sortscheme)

    fun paramSorts (T {sortscheme = PSS.T {sort = ProjSort.T
      {paramsorts, ...}, ...}, ...}) = paramsorts

    fun groundSort (T {sortscheme = PSS.T {sort = ProjSort.T
      {sort, ...}, ...}, ...}) = sort

    fun simple (tyvars,sps) = T {tyvars = tyvars, 
      sortscheme = PSS.simple sps}

    fun generalize (tyvars,ss) = T {tyvars=tyvars, sortscheme=ss} 

    fun instantiate (T {tyvars,sortscheme=ss},tydv) =
      let
        val tyvmap = Vector.zip (tydv,tyvars) 
          handle _ => Error.bug "PTS: insufficient/more type args"
        val f = TyD.instantiateTyvars tyvmap
        val  PSS.T {sort = PS.T{paramsorts, 
          sort = SPS.ColonArrow (tyd,ts)}, svars} = ss
        val sort' = SPS.ColonArrow (f tyd, TS.mapTyD ts f)
        val ps' = Vector.map (paramsorts,
          fn (SPS.ColonArrow (tyd,ts)) => SPS.ColonArrow 
            (f tyd, ts)) (* params co-domain is always an svar *)
        val s' = PS.T {paramsorts = ps', sort = sort'}
        val ss' = PSS.T {svars=svars, sort=s'}
      in
        ss'
      end

    fun domain (T {sortscheme = PSS.T {sort = ProjSort.T
      {sort = SPS.ColonArrow (tyd,_), ...}, ...}, ...}) = tyd
  end

  structure PTS = ProjTypeScheme

  structure RelLang =
  struct
    datatype elem = Int of int
                  | Bool of bool
                  | Var of Var.t
    datatype instexpr = RInst of {targs : TypeDesc.t vector,
                                  sargs : TupSort.t vector,
                                  args : instexpr vector,
                                  rel : RelId.t}
    datatype expr = T of elem vector
                  | X of expr * expr
                  | U of expr * expr
                  | D of expr * expr
                  | R of instexpr * Var.t

    datatype term = Expr of expr
                  | Star of instexpr

    val rId = fn x => T $ Vector.new1 (Var x)
    val rNull = fn _ => T $ empty()

    val instOfRel = fn rel =>
      let
        val empty = fn _ => Vector.new0 ()
      in
        RInst {targs=empty(), sargs=empty(), args=empty(), rel=rel}
      end

    val instOfPolyRel = fn rel => fn targs =>
      let
        val empty = fn _ => Vector.new0 ()
      in
        RInst {targs=targs, sargs=empty(), args=empty(), rel=rel}
      end

    val appR = fn (rid,targs,x) => R (instOfPolyRel rid targs, x)

    val elemToString = fn el => case el of
        Int i => Int.toString i
      | Bool b => Bool.toString b
      | Var v => Var.toString v
  
    fun ieToString (RInst {targs, sargs, args, rel}) = 
      let
        val tstr = vecToStr TyD.toString targs
        val sstr = vecToStr TupSort.toString sargs
        val rstr = vecToStr ieToString args
      in
        (* R T theta R *)
        "(" ^ (RelId.toString rel)^tstr^sstr^rstr
        ^ ")"
      end 

    fun exprToString exp = case exp of
        T (elvec) => "{(" ^ (Vector.fold (elvec,"",fn(e,acc) => 
          (elemToString e) ^ acc)) ^ ")}"
      | X (e1,e2) => "(" ^ (exprToString e1) ^ " X " 
          ^ (exprToString e2) ^ ")"
      | U (e1,e2) => "(" ^ (exprToString e1) ^ " U " 
          ^ (exprToString e2) ^ ")"
      | D (e1,e2) => "(" ^ (exprToString e1) ^ " - " 
          ^ (exprToString e2) ^ ")"
      | R (ie,arg) => (ieToString ie) ^ "(" ^ (Var.toString arg) ^ ")"
    
    val exprToString = exprToString

    val termToString = fn trm => case trm of
        Expr e => exprToString e
      | Star ie => (ieToString ie) ^ "*"

    (*fun app (relId,var) = R(relId,var)*)
    fun union (e1,e2) = U (e1,e2)
    fun crossprd (e1,e2) = X (e1,e2)
    fun diff (e1,e2) = D (e1,e2)
    fun rNull _ = T (Vector.fromList [])
    (*
     * We need this function as we represent multi-arg primitive
     * relations as parametric relations. This is a hack. 
     *)
    fun ieApplySubsts substs (RInst {rel,args,targs,sargs}) = 
      let
        val doIt = ieApplySubsts substs
        val vtor = RelId.fromString o Var.toString
        fun subst v = Vector.fold (substs, v, fn ((new,old),r) =>
          if (Var.toString old = RelId.toString r) 
            then vtor new else r)
      in
        RInst {rel=subst rel, args=Vector.map (args,doIt), 
          targs=targs, sargs=sargs}
      end
    fun applySubsts substs rexpr =
      let
        val doIt = applySubsts substs
        (* caution : telescoped substitutions *)
        fun subst v = Vector.fold (substs, v, fn ((new,old),v) =>
          if (Var.toString old = Var.toString v) then new else v)
        fun elemSubst elem = case elem of
            Var v => Var (subst v)
          | c => c
      in
      case rexpr of 
          T elemv => T (Vector.map (elemv,elemSubst))
        | X (e1,e2) => X (doIt e1, doIt e2)
        | U (e1,e2) => U (doIt e1, doIt e2)
        | D (e1,e2) => D (doIt e1, doIt e2)
        | R (ie,argvar) => R (ieApplySubsts substs ie, subst argvar)
      end

    fun mapInstExpr t f = 
      let
        val g = fn x => mapInstExpr x f
        val doIt = fn cons => fn (x1,x2) => cons (g x1, g x2)
      in
        case t of X x => doIt X x | U x => doIt U x
        | D x => doIt D x | T _ => t
        | R (ie,x) => R (f ie,x)
      end

    fun mapTyD t f =
      let
        fun doItIE (RInst {targs,sargs,args,rel}) = 
          let
            val targs' = Vector.map (targs,f)
            val sargs' = Vector.map (sargs, fn ts => TS.mapTyD ts f)
            val args' = Vector.map (args,doItIE)
          in
            RInst {targs=targs', sargs=sargs', args=args',rel=rel}
          end
      in
        mapInstExpr t doItIE
      end

    fun mapSVar t f =
      let
        fun doItIE (RInst {targs,sargs,args,rel}) = 
          let
            val sargs' = Vector.map (sargs, fn ts => TS.instSVars ts f)
            val args' = Vector.map (args,doItIE)
          in
            RInst {targs=targs, sargs=sargs', args=args',rel=rel}
          end
      in
        mapInstExpr t doItIE
      end

    fun mapRel t f =
      let
        fun doItIE (RInst {targs,sargs,args,rel}) = 
          let
            val rel' = f rel
            val args' = Vector.map (args,doItIE)
          in
            RInst {targs=targs, sargs=sargs, args=args',rel=rel'}
          end
      in
        mapInstExpr t doItIE
      end
  end

  structure StructuralRelation =
  struct
    datatype t = T of {id : RelId.t,
                       params : RelId.t vector,
                       map : (Con.t * Var.t vector option 
                                * RelLang.term) vector}

    fun new data = T data

    fun conMapToString map =
      let
        val conmap = "{" ^ (Vector.toString (fn (c,vlo,trm) =>
            let
              val cstr = Con.toString c
              val vseq = case vlo of NONE => ""
                | SOME vl => Vector.toString Var.toString vl
              val trmstr = RelLang.termToString trm
            in
              cstr ^ vseq ^ " => " ^ trmstr
            end) map) ^ "}\n"
      in
        conmap
      end

    val toString = fn T{id,params,map} =>
      let 
        val relid = RelId.toString id
        val relstr = case Vector.length params of
          0 => relid 
        | _ => relid ^ (Vector.foldr (params, "", 
          fn (rid,acc) => acc ^ " " ^ (RelId.toString rid)))
        val conmap = conMapToString map
      in
        "relation (" ^ relstr ^ ") = " ^ conmap
      end
  end

  structure PrimitiveRelation =
  struct
    datatype def = Nullary of RelLang.expr
                 | Nary of Var.t * def
    datatype t = T of {id : RelId.t,
                       def : def}

    fun defToString (Nullary rexpr) = RelLang.exprToString rexpr
      | defToString (Nary (v,def)) = "\\"^(Var.toString v)^"."
          ^(defToString def)

    val toString =fn (T {id,def}) => "primitive relation "
      ^(RelId.toString id)^" = "^(defToString def)

    fun applySubsts (Nary (v,def)) substs =
      Nary (v, applySubsts def substs)
      | applySubsts (Nullary rexpr) substs = 
          Nullary $ RelLang.applySubsts substs
          rexpr

    fun instantiate (Nary (v,def), arg::args) substs =
          instantiate (def,args) ((arg,v)::substs)
      | instantiate (def, []) substs = applySubsts def
          (Vector.fromList substs)
      | instantiate _ _ = Error.bug "Invalid primitive relation\
        \ instantiation"

    val instantiate = fn (def,args) => instantiate (def, 
        Vector.toList args) []

    val symbase = "pv_"

    val count = ref 0

    val genVar = fn _ => 
      let val id = symbase ^ (Int.toString (!count))
          val _ = count := !count + 1
      in
        Var.fromString id 
      end
    
    fun alphaRename (Nary (v,def)) substs =
      let
        val newV = genVar ()
      in
        Nary (newV, alphaRename def $ (newV,v)::substs)
      end
      | alphaRename (Nullary rexpr) substs =
          Nullary $ RelLang.applySubsts
          (Vector.fromList substs) rexpr

    val alphaRename =fn def => alphaRename def []
  end

  structure TyDBinds =
  struct
    structure Key = 
    struct
      type t = Var.t
      val layout = L.str o Var.toString
      fun equal (v1,v2) = (Var.toString v1) = (Var.toString v2)
    end
    structure Map = ApplicativeMap (structure Key = Key
                                   structure Value = TypeDesc)
    open Map
  end

  structure TyDB = TyDBinds

  structure Predicate =
  struct
    structure BasePredicate =
    struct
      datatype expr = Int of int
                    | Bool of bool
                    | Var of Var.t
      datatype t =  Iff of t * t
                  | Eq of expr * expr

      fun toString bp = case bp of
          Eq (Int i1,Int i2) => (Int.toString i1) ^ " = " 
            ^ (Int.toString i2)
        | Eq (Bool b1,Bool b2) => (Bool.toString b1) ^ " = " 
            ^ (Bool.toString b2)
        | Eq (Var v1, Var v2) => (Var.toString v1) ^ " = " 
            ^ (Var.toString v2)
        | Eq (Var v, Bool b) => (Var.toString v) ^ " = " 
            ^ (Bool.toString b)
        | Iff (t1,t2) => (toString t1) ^ " <=> " ^ (toString t2) 

      fun varEq (v1,v2) = Eq (Var v1,Var v2)

      fun varBoolEq (v,b) = Eq (Var v,Bool b)

      fun applySubst subst t = 
      let
        val varSubst = varSubst subst
      in
        case t of
            Eq (Var v1, Var v2) => Eq (Var $ varSubst v1, Var $ varSubst v2)
          | Eq (Var v, e) => Eq (Var $ varSubst v, e)
          | Eq (e, Var v) => Eq (e, Var $ varSubst v)
          | Iff (t1,t2) => Iff (applySubst subst t1, applySubst subst t2)
      end
    end
    structure RelPredicate =
    struct
      type expr = RelLang.expr
      datatype t = Eq of expr * expr
                 | Sub of expr * expr
                 | SubEq of expr * expr
                 
      fun toString rp = case rp of
          Eq (e1,e2) => (RelLang.exprToString e1) ^ " = "
            ^ (RelLang.exprToString e2)
        | Sub (e1,e2) => (RelLang.exprToString e1) ^ " C "
            ^ (RelLang.exprToString e2)
        | SubEq (e1,e2) => (RelLang.exprToString e1) ^ " C= "
            ^ (RelLang.exprToString e2)

      fun exprMap rp (f : RelLang.expr -> RelLang.expr) = case rp of
          Eq (e1,e2) => Eq (f e1, f e2)
        | Sub (e1,e2) => Sub (f e1, f e2)
        | SubEq (e1,e2) => SubEq (f e1, f e2)

      fun applySubst subst t = exprMap t 
        (RelLang.applySubsts $ Vector.new1 subst)

      fun mapTyD t f = 
        let
          val g = RelLang.mapTyD
          val doIt = fn (x1,x2) => (g x1 f, g x2 f)
        in
          case t of Eq x => Eq $ doIt x | Sub x => Sub $ doIt x
          | SubEq x => SubEq $ doIt x
        end 

      fun mapSVar t f = 
        let
          val g = RelLang.mapSVar
          val doIt = fn (x1,x2) => (g x1 f, g x2 f)
        in
          case t of Eq x => Eq $ doIt x | Sub x => Sub $ doIt x
          | SubEq x => SubEq $ doIt x
        end 
         
    end
    datatype t =  True
               |  False
               |  Base of BasePredicate.t 
               |  Rel of RelPredicate.t
               |  Exists of TyDBinds.t * t
               |  Not of t
               |  Conj of t * t
               |  If of t * t
               |  Iff of t * t
               |  Disj of t * t
               |  Dot of t * t

    fun layout t = case t of
        True => L.str "true" 
      | False => L.str "false" 
      | Base bp => L.str $ BasePredicate.toString bp
      | Rel rp => L.str $ RelPredicate.toString rp 
      | Exists (binds,t) => Pretty.nest ("exist",(TyDBinds.layout binds),
          layout t)
      | Not t => L.seq [L.str "not (", layout t, L.str ")"]
      | Conj (e1,e2) => L.align $ L.separateLeft ([(layout e1), (
          layout e2)],"/\\ ")
      | Disj (e1,e2) => L.align $ L.separateLeft ([(layout e1), (
          layout e2)],"\\/ ")
      | If (e1,e2) => L.align $ L.separateLeft ([(layout e1), (
          layout e2)]," => ")
      | Iff (e1,e2) => L.align $ L.separateLeft ([(layout e1), (
          layout e2)]," <=> ")
      | Dot (e1,e2) => L.align $ L.separateLeft ([(layout e1), (
          layout e2)]," o ")

    fun truee _ = True

    fun falsee _ = False

    fun isFalse False = true | isFalse _ = false

    fun baseP p = Base p

    fun conj (t1,t2) = Conj (t1,t2)

    fun conjR (t,r) = Conj (t,Rel r)

    fun conjP (t,p) = Conj (t,Base p)

    fun applySubst (subst as (new,old)) t = case t of
        True => True
      | False => False
      | Base bp => Base (BasePredicate.applySubst subst bp)
      | Rel rp => Rel (RelPredicate.applySubst subst rp)
      | Exists (tyDB,t) => if (TyDBinds.mem tyDB old)
            then Error.bug "Attempted substitution on existentially \
              \ quantified variable"
            else Exists (tyDB,applySubst subst t)
      | Not t => Not $ applySubst subst t
      | Conj (t1,t2) => Conj (applySubst subst t1, applySubst subst t2)
      | Disj (t1,t2) => Disj (applySubst subst t1, applySubst subst t2)
      | If (t1,t2) => If (applySubst subst t1, applySubst subst t2)
      | Iff (t1,t2) => Iff (applySubst subst t1, applySubst subst t2)
      | Dot (t1,t2) => Dot (applySubst subst t1, applySubst subst t2)

    (* telescoped substitutions *)
    fun applySubsts substs t = Vector.foldr (substs, t, fn (subst,t) =>
      applySubst subst t)

    fun exists (tyb,t) = Exists (tyb,t)

    fun dot (t1,t2) = Dot (t1,t2)

    fun mapRP t f =case t of 
      Rel rp => Rel $ f rp
    | Exists (tyDB,t) => Exists (tyDB, mapRP t f)
    | Not t => Not $ mapRP t f
    | Conj (t1,t2) => Conj (mapRP t1 f, mapRP t2 f)
    | Disj (t1,t2) => Disj (mapRP t1 f, mapRP t2 f)
    | If (t1,t2) => If (mapRP t1 f, mapRP t2 f)
    | Iff (t1,t2) => Iff (mapRP t1 f, mapRP t2 f)
    | Dot (t1,t2) => Dot (mapRP t1 f, mapRP t2 f)
    | _ => t 

    fun mapTyD t f = case t of 
      Rel rp => Rel $ RelPredicate.mapTyD rp f
    | Exists (tyDB,t) => Exists (TyDBinds.map tyDB 
      (fn (v,tyd) => (v,f tyd)), mapTyD t f)
    | Not t => Not $ mapTyD t f
    | Conj (t1,t2) => Conj (mapTyD t1 f, mapTyD t2 f)
    | Disj (t1,t2) => Disj (mapTyD t1 f, mapTyD t2 f)
    | If (t1,t2) => If (mapTyD t1 f, mapTyD t2 f)
    | Iff (t1,t2) => Iff (mapTyD t1 f, mapTyD t2 f)
    | Dot (t1,t2) => Dot (mapTyD t1 f, mapTyD t2 f)
    | _ => t

    fun mapSVar t f = mapRP t (fn rp => RelPredicate.mapSVar rp f)
  end

  structure P = Predicate

  structure RefinementType =
  struct
    datatype t = Base of Var.t * TypeDesc.t * Predicate.t
          | Tuple of (Var.t * t) vector
          | Arrow of (Var.t * t) * t
          (* Records are tuples with fixed bound var *)
          (* Needs extension for {'a | r} list *)

    val symbase = "v_"

    val count = ref 0

    val genVar = fn _ => 
      let val id = symbase ^ (Int.toString (!count))
          val _ = count := !count + 1
      in
        Var.fromString id 
      end

    fun fromTyD tyD =
      let
        open TypeDesc
      in
        case tyD of
          Tarrow (td1,td2) => Arrow ((genVar (), fromTyD td1),
            fromTyD td2)
        | Trecord tdrec => Tuple (Vector.map (Record.toVector tdrec, 
            fn (lbl,td) => (Var.fromString $ Field.toString lbl, 
              fromTyD td)))
        | tyD => Base (genVar(), tyD, Predicate.truee())
      end

    fun toTyD t = case t of
        Base (v,t,p) => t
      | Tuple tv => TyD.makeTrecord $ Vector.map (tv,fn (v,t) => 
          (Field.Symbol $ Field.Symbol.fromString $ Var.toString v, 
           toTyD t))
      | Arrow ((v1,t1),t2) => TyD.makeTarrow (toTyD t1, toTyD t2)
    
    fun layout rty = case rty of
          Base(var,td,pred) => L.seq [L.str ("{" ^ (Var.toString var) 
            ^ ":" ^ (TypeDesc.toString td) ^ " | "), 
            Predicate.layout pred, L.str "}"]
        | Tuple tv => L.vector $ Vector.map (tv, fn (v,t) => 
            L.seq [L.str $ Var.toString v, L.str ":", layout t])
        | Arrow ((v1,t1 as Arrow _),t2) => L.align $ L.separateLeft (
            [L.seq [L.str "(", L.str $ Var.toString v1, L.str ":",
              layout t1, L.str ")"], 
            layout t2]," -> ")
        | Arrow ((v1,t1),t2) => L.align $ L.separateLeft (
            [L.seq [L.str $ Var.toString v1, L.str ":", layout t1], 
            layout t2]," -> ")

    fun mapBaseTy t f = case t of
        Base (v,t,p) => Base $ f (v,t,p)
      | Tuple tv => Tuple $ Vector.map (tv,fn (v,t) => 
          (v,mapBaseTy t f))
      | Arrow ((v1,t1),t2) => Arrow ((v1,mapBaseTy t1 f), 
          mapBaseTy t2 f)

    fun mapTyD t f = mapBaseTy t (fn (v,t,p) => 
      (v,f t, P.mapTyD p f)) 
      
    fun applySubsts substs refty = 
      mapBaseTy refty (fn (bv,t,pred) =>
        if Vector.exists (substs,fn(n,ol) => varStrEq (ol,bv))
          then Error.bug "Attempted substitution of bound var"
          else (bv,t,Predicate.applySubsts substs pred))

    fun alphaRenameToVar refty newbv = case refty of
        Base (bv,t,p) => Base (newbv,t,
          Predicate.applySubst (newbv,bv) p)
      | _ => Error.bug "alphaRename attempted on non-base type"

    fun alphaRename refty = alphaRenameToVar refty (genVar())

    val exnTyp = fn _ => Base (genVar(),TypeDesc.makeTunknown (),
      Predicate.falsee())

    fun mapSVar t f = mapBaseTy t (fn (v,t,p) =>
      (v,t, Predicate.mapSVar p f))

    val newLongVar = fn (var,fld) => Var.fromString $
        (Var.toString var)^"."^(Var.toString fld)
    (*
     * Decomposes single tuple bind of form v ↦ {x0:T0,x1:T1} to
     * multiple binds : [v.x0 ↦ T0, v.x1 ↦ T1]
     *)
    fun decomposeTupleBind (tvar : Var.t, tty as Tuple 
      refTyBinds) : (Var.t*t) vector =
      let
        val bindss = Vector.map (refTyBinds, 
          fn (refTyBind as (_,refTy)) => 
            case refTy of 
              Tuple _ => decomposeTupleBind refTyBind
            | _ => Vector.new1 refTyBind)
        val binds = Vector.map (Vector.concatV bindss, 
          fn (v,ty) => (newLongVar (tvar,v), ty))
      in
        binds
      end

  end

  structure RefTy = RefinementType

  structure ParamRefType =
  struct
    datatype t = T of {params : (RelId.t * SimpleProjSort.t) vector,
                        refty : RefinementType.t}
                         
    fun layout (T {params, refty}) = 
      let
        fun typedParamLyt (r,sprojty) = L.str $ 
          (RelId.toString r) ^ " :: " ^
          (SPS.toString sprojty)
        val paramslyt = L.vector $ Vector.map (params, typedParamLyt)
        val reftylyt = RefinementType.layout refty
      in
        L.seq [paramslyt, L.str ". ", reftylyt]
      end

    fun parametrize (sortedParams,refTy) = T {params=sortedParams,
      refty=refTy}

    fun mapTyD (T {params,refty}) f = 
      let
        val params' = Vector.map (params, fn (r,sps) => 
          (r, SPS.mapTyD sps f))
        val refty' = RefTy.mapTyD refty f
      in
        T {params=params', refty=refty'}
      end
  end

  structure PRf = ParamRefType

  structure RefinementSortScheme =
  struct
    datatype t = T of {svars: SVar.t vector,
                       prefty : ParamRefType.t }

    fun layout (T {svars, prefty}) = 
      let
        val svlyt = L.vector $ Vector.map (svars,fn sv =>
          L.str $ SVar.toString sv)
        val prflyt = ParamRefType.layout prefty
      in
        L.seq [svlyt, L.str ". ", prflyt]
      end

    val fromRefTy = fn refTy => T {svars = empty(), prefty = 
      ParamRefType.T {params=empty(), refty=refTy}}

    val toRefTy = fn (T {prefty = ParamRefType.T {refty, ...} 
      ,...}) => refty

    val generalize = fn (svars,prefty) => 
      T {svars=svars, prefty=prefty}

    fun mapTyD (T {svars,prefty}) f = T {svars=svars,
      prefty=PRf.mapTyD prefty f}
  end

  structure RefSS = RefinementSortScheme

  structure RefinementTypeScheme =
    struct
      datatype t = T of {tyvars : Tyvar.t vector,
                        refss : RefinementSortScheme.t,
                        (* ICFP taking its toll *)
                        isAssume : bool}
    
      val generalizeRefTy = fn (tyvars, refTy) =>
        T {tyvars = tyvars, refss = RefSS.fromRefTy refTy, 
            isAssume = false}

      val generalize = fn (tyvars, refss) =>
        T {tyvars = tyvars, refss = refss, isAssume = false}

      val generalizeAssump = fn (tyvars, refss, isAssume) =>
        T {tyvars = tyvars, refss = refss, isAssume = isAssume}

      val isAssumption = fn(T {isAssume, ...}) => isAssume

      val specialize = fn (T {tyvars,refss,...}) =>
        refss

      val specializeRefTy = fn (T {tyvars,refss,...}) =>
        RefSS.toRefTy refss

      fun layout (T {tyvars,refss,isAssume}) =
        let
          val flaglyt = (if isAssume then L.str "Assumption: " else
            L.empty)
          val tyvlyt = L.vector $ Vector.map (tyvars,fn tyv =>
            L.str $ Tyvar.toString tyv)
          val refsslyt = RefinementSortScheme.layout refss
        in
          L.seq [flaglyt,tyvlyt,refsslyt]
        end

      fun instantiate (T{tyvars,refss,...},tydvec) =
        let
          val len = Vector.length
          val _ = assert (len tyvars = len tydvec,
            "insufficient number of type args")
          val tyvmap = Vector.zip (tydvec,tyvars)
          (*
           * It is possible that we encounter a tyvar
           * that is not generalized in this RefTyS.
           * We do not panic.
           *)
        in
          RefSS.mapTyD refss $ TyD.instantiateTyvars tyvmap
        end

    end

  structure RefTyS = RefinementTypeScheme

  structure RelSpec =
  struct
    structure TypeSpec =
    struct
      datatype t = T of {isAssume : bool,
                         name:Var.t,
                         params: RelId.t vector,
                         refty : RefinementType.t}
      val layout = fn T {name=var,params,refty,...} => L.seq [
        L.str ((Var.toString var) ^ " : "),
        L.str (Vector.toString RelId.toString params),
        RefinementType.layout refty]
    end
    datatype t = T of {reldecs : StructuralRelation.t vector,
                       primdecs : PrimitiveRelation.t vector,
                       typespecs : TypeSpec.t vector}
    val layout = fn T ({reldecs,primdecs,typespecs,...}) =>
      let 
        val srs = Vector.toString StructuralRelation.toString reldecs
        val prs = Vector.toString PrimitiveRelation.toString primdecs
        val tslyt = L.align $ Vector.toListMap (typespecs,
          TypeSpec.layout)
      in
        L.align [L.str srs, L.str prs, tslyt]
      end
  end 

  structure Bind =
  struct
    datatype transformer = Fr of Var.t vector * RelLang.expr 

    datatype expr = Expr of {ground : RelId.t * TypeDesc.t vector * Var.t,
                             fr : transformer}

    datatype abs = Abs of Var.t * expr

    datatype def = Def of  {tyvars : Tyvar.t vector,
                            params : RelId.t vector,
                            abs : abs}
                 | BogusDef

    val symbase = "v_"

    val count = ref 0

    val genVar = fn _ => 
      let val id = symbase ^ (Int.toString (!count))
          val _ = count := !count + 1
      in
        Var.fromString id 
      end

    fun frToString (Fr (vs,rexpr)) =
      let
        val vsStr = Vector.toString Var.toString vs
        val reStr = RelLang.exprToString rexpr
      in
        "\\"^vsStr^". "^reStr
      end

    fun bindExprToString (Expr {ground = (r,tydv,x),fr}) =
      let
        val tydStr = Vector.toString TyD.toString tydv
        val vStr = Var.toString x
        val gStr = (RelId.toString r)^" "^tydStr^" ("^vStr^")"
        val frStr = frToString fr
      in
        "bind ("^gStr^","^frStr^")"
      end

    fun absToString (Abs (v,bindex)) =
      let
        val bStr = bindExprToString bindex
      in
        "\\"^(Var.toString v)^". "^bStr
      end

    fun defToString (Def {tyvars,params=rs,abs}) = 
      let
        val tyvStr = Vector.toString Tyvar.toString tyvars
        val absStr = absToString abs
        val rsStr = fn _ => Vector.toString RelId.toString rs
      in
        case Vector.length rs of 0 => absStr
        | _ => tyvStr ^" \\"^(rsStr ())^". "^absStr
      end
      | defToString (BogusDef) = "-- NA --"

    fun groundRelTyS pts = 
      let
        val PTS.T {tyvars, sortscheme = PSS.T {sort = ProjSort.T 
          {paramsorts, sort=groundSort}, ...}} = pts
        (* SVar to Tyvar map *)
        val svarMap = Vector.map (paramsorts,
          fn (SPS.ColonArrow (a, TS.Tuple [TS.S t])) => (t,a))
        val mapSVar = fn t => case Vector.peekMap (svarMap, 
          fn (t',a) => if SVar.eq (t,t') then SOME a else NONE) of
            SOME a => a | NONE => Error.bug "SVar impossible case"
        val SPS.ColonArrow (tyd,TS.Tuple tts) = groundSort
        val tyds = List.map (tts,fn tt => case tt of 
          TS.S t => TS.T $ mapSVar t | _ => tt)
      in
        PTS.simple (tyvars, SPS.ColonArrow (tyd, TS.Tuple tyds))
      end

    fun makeGroundDef (params,rterm) : RelLang.term = 
      let
        open RelLang
        val empty = fn _ => Vector.new0 ()
        val isParam = fn rid => Vector.exists (params, fn p =>
          RelId.toString rid = RelId.toString p)
        fun doItExp exp = case exp of
            U (e1,e2) => U (doItExp e1, doItExp e2)
          | X (e1,e2) => X (doItExp e1, doItExp e2)
          | D (e1,e2) => D (doItExp e1, doItExp e2)
          | R (RInst {rel,targs,sargs,args},x) => if isParam rel 
              then RelLang.rId x
              else (R (RInst {rel=rel, targs=targs, sargs=empty(), 
                  args=empty()},x))
          | _ => exp
              
      in
        case rterm of 
          Expr exp => Expr $ doItExp exp
        | Star (RInst {rel,targs,sargs,args}) => Star $ 
            RInst {rel=rel, targs=targs, sargs=sargs, 
              args=empty()}
      end


    fun makeBindDef (id,params,pts) : def =
      let
        val PTS.T {sortscheme = PSS.T {sort = PS.T {paramsorts = 
          paramSorts, sort=groundSort}, ...}, tyvars} = pts
        (* SVar to RelId map *)
        val svarMap = Vector.map2 (paramSorts,params, 
          fn (SPS.ColonArrow (_,TS.Tuple [TS.S t]),rid) => (t,rid))
        val mapSVar = fn t => case Vector.peekMap (svarMap, 
          fn (t',rid) => if SVar.eq (t,t') then SOME rid else NONE) of
            SOME rid => rid | NONE => Error.bug "SVar impossible case"
        val SPS.ColonArrow (_,TS.Tuple tts) = groundSort
        val (bvs,rApps) = List.unzip $ List.map (tts, fn tt => 
          let
            val v = genVar ()
          in
            case tt of TS.T tyd => (v,RelLang.rId v)
            | TS.S t => (v, RelLang.appR (mapSVar t, empty (), v))
          end)
        val SOME xexpr = List.foldr (rApps, NONE, 
          fn (rApp,xop) => case xop of NONE => SOME $ rApp 
          | SOME xexpr => SOME $ RelLang.crossprd (rApp,xexpr))
        val bv = genVar ()
        val fr = Fr (Vector.fromList bvs,xexpr)
        val targs = Vector.map (tyvars, TyD.makeTvar)
        val bindex = Expr {ground = (id,targs,bv), fr=fr}
        val bindabs = Abs (bv,bindex)
      in
        Def {tyvars=tyvars, params=params, abs=bindabs}
      end

  fun instantiate (Def {tyvars,params,abs},tydvec,ridvec) : abs =
    let
      val tsubsts = Vector.zip (tydvec,tyvars) handle _ =>
        Error.bug $ "Bind : tyvar inst error"
      val rmap = Vector.toList $ Vector.zip (params,ridvec) 
        handle _ => Error.bug $ "Bind : params inst error"
      val err = fn _ => Error.bug "Bind: inst error"
      val mapt = TyD.instantiateTyvars tsubsts 
      val mapr = mkMapper rmap RelId.equal err
      val Abs (bv,Expr {ground=(gr,targs,_), fr=Fr (xs,rexpr)}) = abs
      val targs' = Vector.map (targs, mapt)
      val ground' = (gr,targs',bv)
      val rexpr' = RelLang.mapRel rexpr mapr
      val expr' = Expr {ground=ground', fr = Fr (xs,rexpr')}
      val abs' = Abs (bv, expr')
    in
      abs'
    end
    | instantiate (BogusDef, _, _) = Error.bug "Cannot instantiate \
      \ bogus bind def"

  fun fromAbs (abs:abs) : def =
    Def {tyvars=empty (), params=empty (), abs=abs}
  end
end

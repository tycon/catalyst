functor SpecLang (S : SPEC_LANG_STRUCTS) : SPEC_LANG = 
struct
  open S
  structure L = Layout

  fun $ (f,arg) = f arg
  infixr 5 $
  val assert = Control.assert
  fun varStrEq (v1,v2) = (Var.toString v1 = Var.toString v2)
  fun varSubst (subst as (new,old)) v = if varStrEq (v,old) 
    then new else v

  structure RelLang =
  struct
    structure RelId = Var

    datatype elem = Int of int
                  | Bool of bool
                  | Var of Var.t
    datatype expr = T of elem vector
                  | X of expr * expr
                  | U of expr * expr
                  | D of expr * expr
                  | R of RelId.t * Var.t

    datatype term = Expr of expr
                  | Star of RelId.t
    structure RelType =
    struct
      (*
       * Type of rexpr is always a tuple.
       * Type is empty tuple if and only if rexpr is {()}
       *)
      datatype t = Tuple of TypeDesc.t vector

      fun toString (Tuple tydv) = Vector.toString 
        TypeDesc.toString tydv

      fun equal (Tuple tydv1, Tuple tydv2) = 
        (Vector.length tydv1 = Vector.length tydv2) andalso
        Vector.forall2 (tydv1,tydv2,TypeDesc.sameType)
        
      fun unionType (t1 as Tuple tydv1, t2 as Tuple tydv2) =
        case (Vector.length tydv1, Vector.length tydv2) of
          (0,_) => t2 | (_,0) => t1
        | (n1,n2) => (assert (equal (t1,t2),"Union \
            \ incompatible types\n"); t1)

      fun crossPrdType (t1 as Tuple tyds1, t2 as Tuple tyds2) =
        case (Vector.length tyds1, Vector.length tyds2) of
          (0,_) => t1 | (_,0) => t2 
        | _ => Tuple $ Vector.concat [tyds1,tyds2]
    end

    structure RelTypeScheme = 
    struct
      datatype t = T of {tyvars : Tyvar.t vector,
                         relty : RelType.t}

      fun generalize (tyvars,tyd) =
        T {tyvars=tyvars,relty=tyd}

      fun specialize (T{relty,...}) = relty

      fun toString (T{tyvars, relty}) = 
        (Vector.toString Tyvar.toString tyvars)^" "^
          (RelType.toString relty)

      fun instantiate (T{tyvars,relty = RelType.Tuple tys},tydvec) =
        let
          val len = Vector.length
          val _ = assert (len tyvars = len tydvec,
            "insufficient number of type args for reltys inst")
          val instTyD = TypeDesc.instantiateTyvars $ Vector.zip 
            (tydvec,tyvars)
          val tydvec' = Vector.map (tys, instTyD)
        in
          RelType.Tuple tydvec'
        end 

      fun equalTyvars (tyv1,tyv2) = 
        (Vector.length tyv1 = Vector.length tyv2) andalso 
        (Vector.forall2 (tyv1,tyv2, fn (tyvar1,tyvar2) => 
            TypeDesc.sameType (TypeDesc.makeTvar tyvar1, 
              TypeDesc.makeTvar tyvar2)))

      fun unionTypeScheme (T{tyvars=tyv1,relty=relty1},
        T{tyvars=tyv2,relty=relty2}) = 
        let
          val _ = assert (equalTyvars (tyv1,tyv2), "Union \
                \ incompatible type schemes\n")
        in
          T {tyvars = tyv1, relty = RelType.unionType (relty1,relty2)}
        end

      fun crossPrdTypeScheme (T{tyvars=tyv1,relty=relty1},
        T{tyvars=tyv2,relty=relty2}) =
        T {tyvars = Vector.concat [tyv1,tyv2], 
           relty = RelType.crossPrdType (relty1,relty2)}
    end

    val elemToString = fn el => case el of
        Int i => Int.toString i
      | Bool b => Bool.toString b
      | Var v => Var.toString v

    fun exprToString exp = case exp of
        T (elvec) => "{(" ^ (Vector.fold (elvec,"",fn(e,acc) => 
          (elemToString e) ^ acc)) ^ ")}"
      | X (e1,e2) => "(" ^ (exprToString e1) ^ " X " 
          ^ (exprToString e2) ^ ")"
      | U (e1,e2) => "(" ^ (exprToString e1) ^ " U " 
          ^ (exprToString e2) ^ ")"
      | D (e1,e2) => "(" ^ (exprToString e1) ^ " - " 
          ^ (exprToString e2) ^ ")"
      | R (rel,arg) => (RelId.toString rel) ^ "(" ^ (Var.toString arg) ^ ")"
    
    val exprToString = exprToString

    val termToString = fn trm => case trm of
        Expr e => exprToString e
      | Star r => (RelId.toString r) ^ "*"

    fun app (relId,var) = R(relId,var)
    fun union (e1,e2) = U (e1,e2)
    fun crossprd (e1,e2) = X (e1,e2)
    fun diff (e1,e2) = D (e1,e2)
    fun emptyexpr _ = T (Vector.fromList [])
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
        | R (relId,argvar) => R (relId, subst argvar)
      end
  end

  structure StructuralRelation =
  struct
    datatype t = T of {id : RelLang.RelId.t,
                       map : (Con.t * Var.t vector option * RelLang.term) vector}

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

    val toString = fn T{id,map} =>
      let val relid = Var.toString id
          val conmap = conMapToString map
      in
        "relation " ^ relid ^ " = " ^ conmap
      end
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
      datatype t =   Eq of expr * expr
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
  end

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

    fun mapTyD t f = mapBaseTy t (fn (v,t,p) => (v,f t,p)) 
      
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

  end

  structure RefinementTypeScheme =
    struct
      datatype t = T of {tyvars : Tyvar.t vector,
                        refty : RefinementType.t,
                        isAssume : bool}
    
      val generalize = fn (tyvars, refty) =>
        T {tyvars = tyvars, refty = refty, isAssume = false}
      val generalizeAssump = fn (tyvars, refty, isAssume) =>
        T {tyvars = tyvars, refty = refty, isAssume = isAssume}
      val isAssumption = fn(T {isAssume, ...}) => isAssume
      val specialize = fn (T {tyvars,refty,...}) =>
        refty
      fun layout (T {tyvars,refty,isAssume}) =
        let
          val flaglyt = (if isAssume then L.str "Assumption: " else
            L.empty)
          val tyvlyt = L.vector $ Vector.map (tyvars,fn tyv =>
            L.str $ Tyvar.toString tyv)
          val reftylyt = RefinementType.layout refty
        in
          L.seq [flaglyt,tyvlyt,reftylyt]
        end
      fun instantiate (T{tyvars,refty,...},tydvec) =
        let
          val len = Vector.length
          val _ = assert (len tyvars = len tydvec,
            "insufficient number of type args")
          val substs = Vector.zip (tydvec,tyvars)
          (*
           * It is possible that we encounter a tyvar
           * that is not generalized in this RefTyS.
           * We do not panic.
           *)
        in
          RefinementType.mapTyD refty 
            (TypeDesc.instantiateTyvars substs)
        end
    end

  structure RelSpec =
  struct
    structure TypeSpec =
    struct
      datatype t = T of bool * Var.t * RefinementType.t
      val layout = fn T(_,var,refty) => L.seq [
        L.str ((Var.toString var) ^ " : "),
        RefinementType.layout refty]
    end
    datatype t = T of {reldecs : StructuralRelation.t vector,
                       typespecs : TypeSpec.t vector}
    val layout = fn T ({reldecs,typespecs,...}) =>
      let 
        val srs = Vector.toString StructuralRelation.toString reldecs
        val tslyt = L.align $ Vector.toListMap (typespecs,
          TypeSpec.layout)
      in
        L.align [L.str srs,tslyt]
      end
  end 
end

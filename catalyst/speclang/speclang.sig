signature SPEC_LANG_STRUCTS =
sig
  include ATOMS
end
signature SPEC_LANG = 
sig
  include SPEC_LANG_STRUCTS

  structure RelId : 
  sig
    include ID
    val eq : t*t -> bool
    val equal : t*t -> bool
  end

  structure SVar :
  sig
    type t
    val new : unit -> t
    val eq : t*t -> bool
    val toString : t -> string
  end

  structure TupSort :
  sig
    datatype tT = T of TypeDesc.t | S of SVar.t
    datatype t = Tuple of tT list
    datatype cs = Eq of t*t 
    val toString : t -> string
    val fromSVar : SVar.t -> t
    val getSVars : t -> SVar.t list
    val equal : (t*t) -> bool
    val unionType : (t*t) -> (cs list * t)
    val crossPrdType : (t*t) -> (cs list * t)
    val instSVars : t -> (SVar.t -> t) -> t
    val solvecs : cs list -> (SVar.t -> t)
  end
  
  structure SimpleProjSort : 
  sig
    datatype t = ColonArrow of TypeDesc.t * TupSort.t
    val toString : t -> string
    val layout : t -> Layout.t
    val new : TypeDesc.t * TupSort.t -> t
    val domain : t -> TypeDesc.t
    val range : t -> TupSort.t
    (*
    val unify : t*t -> RelTyConstraint.t
    val mapTyD : t -> (TypeDesc.t -> TypeDesc.t) -> t
    val foldTyD : t -> 'a -> (TypeDesc.t * 'a -> 'a) -> 'a
    val mapRelTy : t -> (TupSort.t -> TupSort.t) -> t
    val foldRelTy : t -> 'a -> (TupSort.t * 'a -> 'a) -> 'a
    val instTyvars : (Tyvar.t * TypeDesc.t) vector * t -> t
    val instSVars : (SVar.t * TupSort.t) vector * t -> t
    *)
  end

  structure ProjSort :
  sig
    datatype t =  T of {paramsorts : SimpleProjSort.t vector,
                              sort : SimpleProjSort.t}
    val toString : t -> string
    val new : SimpleProjSort.t vector * SimpleProjSort.t -> t
    val simple : SimpleProjSort.t -> t
    val domain : t -> TypeDesc.t
    val range : t -> TupSort.t
    (*
    val paramSorts : t -> SimpleProjSort.t vector
    val foldTyD : t -> 'a -> (TypeDesc.t * 'a -> 'a) -> 'a
    val foldRelTy : t -> 'a -> (TupSort.t * 'a -> 'a) -> 'a
    val instTyvars : (Tyvar.t * TypeDesc.t) vector * t -> t
    val instSVars : (SVar.t * TupSort.t) vector * t -> t
    *)
  end

  structure ProjSortScheme : 
  sig
    datatype t = T of {svars : SVar.t vector,
                        sort : ProjSort.t}
    val toString : t -> string
    val simple : SimpleProjSort.t -> t
    val instantiate : t * TupSort.t vector -> ProjSort.t
    val generalize : SVar.t vector * ProjSort.t -> t
    (*
    val generalizeWith : SVar.t vector * ProjSort.t -> t
    val specialize : t -> ProjSort.t
    val instantiate : (SVar.t * TupSort.t) vector * t ->
      ProjSort.t
    val foldTyD : t -> 'a -> (TypeDesc.t * 'a -> 'a) -> 'a
    val instTyvars : (Tyvar.t * TypeDesc.t) vector * t -> t
    val domain : t ->TypeDesc.t
    *)
  end

  structure ProjTypeScheme :
  sig
    datatype t = T of {tyvars : Tyvar.t vector,
                       sortscheme : ProjSortScheme.t}
    val toString : t -> string
    val instantiate : t * TypeDesc.t vector -> ProjSortScheme.t
    val simple : Tyvar.t vector * SimpleProjSort.t -> t
    val domain : t ->TypeDesc.t
    val generalize : Tyvar.t vector * ProjSortScheme.t -> t
    (*
    val generalize : ProjSortScheme.t -> t
    val specialize : t -> ProjSortScheme.t
    val tyvars : t -> Tyvar.t vector
    val instantiate : (Tyvar.t * TypeDesc.t) vector * t ->
        ProjSortScheme.t
    *)
  end

  structure RelLang : 
  sig
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
    val elemToString : elem -> string
    val ieToString : instexpr -> string
    val exprToString : expr -> string
    val termToString : term -> string
    val rId : Var.t -> expr
    val rNull: unit -> expr
    val instOfRel : RelId.t -> instexpr
    val appR : RelId.t * TypeDesc.t vector * Var.t -> expr
    val union : expr * expr -> expr
    val crossprd : expr * expr -> expr
    val diff : expr * expr -> expr
    val applySubsts : (Var.t * Var.t) vector -> expr -> expr
    (* More utils in the structure. Not exposed through signature. 
       Check before adding new. *)
  end

  structure StructuralRelation :
  sig
    
    datatype t = T of {id : RelId.t,
                       params : RelId.t vector,
                       map : (Con.t * Var.t vector option * RelLang.term)
                             vector}
    val conMapToString : (Con.t * Var.t vector option * RelLang.term) vector 
      -> string
    val toString : t -> string
  end

  structure PrimitiveRelation :
  sig
    datatype def = Nullary of RelLang.expr
                 | Nary of Var.t * def
    datatype t = T of {id : RelId.t,
                       def : def}
    val instantiate : def * Var.t vector -> def
    val defToString : def -> string
    val alphaRename : def -> def
  end

  structure TyDBinds : APPLICATIVE_MAP where 
    type Key.t = Var.t and
    type Value.t = TypeDesc.t

  structure Predicate : 
  sig
    structure BasePredicate :
    sig
      datatype expr =  Int of int
                    | Bool of bool
                    | Var of Var.t
      datatype t =  Iff of t * t
                  | Eq of expr * expr
      val toString : t -> string
      val varEq : Var.t * Var.t -> t
      val varBoolEq : Var.t * bool -> t
      val applySubst : (Var.t * Var.t) -> t -> t
    end

    structure RelPredicate :
    sig
      type expr = RelLang.expr
      datatype t = Eq of expr * expr
                 | Sub of expr * expr
                 | SubEq of expr * expr
      val toString : t -> string
      val applySubst : (Var.t * Var.t) -> t -> t
    end
    datatype t =  True
               |  False
               |  Base of BasePredicate.t 
               |  Rel of RelPredicate.t
               |  Exists of TyDBinds.t * t
               |  Not of t
               |  Conj of t * t
               |  Disj of t * t
               |  If of t * t
               |  Iff of t * t
               |  Dot of t * t

    val layout : t -> Layout.t 
    val truee : unit -> t
    val falsee : unit -> t
    val isFalse : t -> bool
    val baseP : BasePredicate.t -> t 
    val conj : t*t -> t
    val conjR : t*RelPredicate.t -> t
    val conjP : t*BasePredicate.t -> t
    val applySubst : Var.t * Var.t -> t -> t
    val applySubsts : (Var.t * Var.t) vector -> t -> t
    val exists : TyDBinds.t * t -> t
    val dot: t*t -> t
    end

  structure RefinementType : 
  sig
    datatype t = Base of Var.t * TypeDesc.t * Predicate.t
               | Tuple of (Var.t * t) vector
               | Arrow of (Var.t * t) * t
    val layout : t -> Layout.t 
    val fromTyD : TypeDesc.t -> t
    val toTyD : t -> TypeDesc.t
    val applySubsts : (Var.t * Var.t) vector -> t -> t
    val alphaRename : t -> t
    val alphaRenameToVar : t -> Var.t -> t
    val mapBaseTy : t -> ((Var.t * TypeDesc.t * Predicate.t) -> 
          (Var.t * TypeDesc.t * Predicate.t)) -> t
    val mapTyD : t -> (TypeDesc.t -> TypeDesc.t) -> t
    val exnTyp : unit -> t
    val mapSVar : t -> (SVar.t -> TupSort.t) -> t
    (* pre-condition: input t must be a Tuple _ *)
    val decomposeTupleBind : Var.t*t -> (Var.t*t) vector
  end

  structure ParamRefType :
  sig
    datatype t = T of {params : (RelId.t * SimpleProjSort.t) vector,
                        refty : RefinementType.t}
    val layout : t -> Layout.t
    val parametrize : (RelId.t * SimpleProjSort.t) vector *
      RefinementType.t -> t
    val mapTyD : t -> (TypeDesc.t -> TypeDesc.t) -> t
  end

  structure RefinementSortScheme :
  sig
    datatype t = T of {svars: SVar.t vector,
                       prefty : ParamRefType.t }
    val toRefTy : t -> RefinementType.t
    val fromRefTy : RefinementType.t -> t
    val generalize : (SVar.t vector * ParamRefType.t) -> t
    val mapTyD : t -> (TypeDesc.t -> TypeDesc.t) -> t
  end

  structure RefinementTypeScheme :
    sig
      datatype t = T of {tyvars : Tyvar.t vector,
                        refss : RefinementSortScheme.t,
                        (* ICFP taking its toll *)
                        isAssume : bool}
      val generalizeRefTy : Tyvar.t vector * RefinementType.t -> t
      val specializeRefTy : t -> RefinementType.t
      val generalize : Tyvar.t vector * RefinementSortScheme.t -> t
      val specialize : t -> RefinementSortScheme.t
      val generalizeAssump : Tyvar.t vector * RefinementSortScheme.t 
        * bool -> t
      val isAssumption : t -> bool
      val instantiate : t * TypeDesc.t vector -> RefinementSortScheme.t
      (*
      val specialize: t -> RefinementType.t
      *)
      val layout : t -> Layout.t 
    end

  structure RelSpec : 
  sig
    structure TypeSpec:
    sig
      datatype t = T of {isAssume : bool,
                         name:Var.t,
                         params: RelId.t vector,
                         refty : RefinementType.t}
      val layout : t -> Layout.t
    end
    datatype t = T of {reldecs : StructuralRelation.t vector,
                       primdecs : PrimitiveRelation.t vector,
                       typespecs : TypeSpec.t vector}
    val layout : t -> Layout.t
  end

  structure Bind :
  sig

    datatype transformer = Fr of Var.t vector * RelLang.expr 

    datatype expr = Expr of {ground : RelId.t * TypeDesc.t vector * Var.t,
                             fr : transformer}

    datatype abs = Abs of Var.t * expr

    datatype def = Def of  {tyvars : Tyvar.t vector,
                            params : RelId.t vector,
                            abs : abs}
                 | BogusDef

    val defToString : def -> string
    val makeGroundDef : RelId.t vector * RelLang.term -> RelLang.term
    val makeBindDef : RelId.t * RelId.t vector * ProjTypeScheme.t
      -> def
    val groundRelTyS : ProjTypeScheme.t -> ProjTypeScheme.t
    val instantiate : def * TypeDesc.t vector * RelId.t vector -> abs
    val fromAbs : abs -> def
  end
end

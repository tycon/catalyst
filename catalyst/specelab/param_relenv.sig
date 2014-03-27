signature PARAM_REL_ENV_STRUCTS = 
sig
  structure SpecLang : SPEC_LANG
end
signature PARAM_REL_ENV = 
sig
  include PARAM_REL_ENV_STRUCTS

 datatype def = Prim of SpecLang.PrimitiveRelation.def
           | Bind of SpecLang.Bind.def
  
  type reldesc = { ty : SpecLang.ProjTypeScheme.t,
                   def : def}
  exception ParamRelNotFound of SpecLang.RelId.t
  type t
  val empty : t
  val mem : t -> SpecLang.RelId.t -> bool
  val find : t -> SpecLang.RelId.t -> reldesc
  val add : t -> (SpecLang.RelId.t * reldesc) -> t
  val addUniterp : t -> (SpecLang.RelId.t * SpecLang.ProjTypeScheme.t) 
    -> t
  val remove : t -> SpecLang.RelId.t -> t
  val toVector : t -> (SpecLang.RelId.t * reldesc) vector
  val layout : t -> Layout.t
end

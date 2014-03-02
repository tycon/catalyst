signature REL_ENV_STRUCTS = 
sig
  structure SpecLang : SPEC_LANG
end
signature REL_ENV = 
sig
  include REL_ENV_STRUCTS
  
  type reldesc = { ty : SpecLang.RelLang.RelTypeScheme.t,
                  map : (SpecLang.Con.t * SpecLang.Var.t vector option *
                  SpecLang.RelLang.expr) 
                    vector}
  exception RelNotFound of SpecLang.RelLang.RelId.t
  type t
  val empty : t
  val mem : t -> SpecLang.RelLang.RelId.t -> bool
  val find : t -> SpecLang.RelLang.RelId.t -> reldesc
  val add : t -> (SpecLang.RelLang.RelId.t * reldesc) -> t
  val remove : t -> SpecLang.RelLang.RelId.t -> t
  val toVector : t -> (SpecLang.RelLang.RelId.t * reldesc) vector
  val layout : t -> Layout.t
end

signature REL_ENV_STRUCTS = 
sig
  structure SpecLang : SPEC_LANG
end
signature REL_ENV = 
sig
  include REL_ENV_STRUCTS
  
  type reldesc = {ty : SpecLang.ProjTypeScheme.t,
                  map : (SpecLang.Con.t * SpecLang.Var.t vector option *
                  SpecLang.RelLang.expr) 
                    vector}
  exception RelNotFound of SpecLang.RelId.t
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

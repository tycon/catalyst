signature VERIFICATION_CONDITION_STRUCTS =
sig
  include SPEC_LANG
  structure VE : VAR_ENV
  structure RE : REL_ENV
  structure PRE : PARAM_REL_ENV
  sharing type RefinementTypeScheme.t = VE.tyscheme
  sharing type Var.t = VE.Var.t
  sharing type RelId.t = RE.SpecLang.RelId.t
  sharing type RelId.t = PRE.SpecLang.RelId.t
  sharing ProjTypeScheme = RE.SpecLang.ProjTypeScheme
  sharing Bind = RE.SpecLang.Bind
  sharing RE.SpecLang = PRE.SpecLang
end
signature VERIFICATION_CONDITION =
sig
  include VERIFICATION_CONDITION_STRUCTS

  datatype simple_pred =  True
                       |  False
                       |  Base of Predicate.BasePredicate.t 
                       |  Rel of Predicate.RelPredicate.t

  datatype vc_pred =  Simple of simple_pred
                   |  If of vc_pred * vc_pred
                   |  Iff of vc_pred * vc_pred
                   |  Conj of vc_pred vector
                   |  Disj of vc_pred vector
                   |  Not of vc_pred

  type tydbind = Var.t * TypeDesc.t

  type tydbinds = tydbind vector
  type bindings = {tbinds: tydbinds, rbinds:PRE.t}

  datatype t = T of bindings * vc_pred * vc_pred
  
  val fromTypeCheck : VE.t * PRE.t * RefinementType.t * 
    RefinementType.t -> t vector

  val elaborate : RE.t * PRE.t * t -> t

  val layout : t vector -> Layout.t

  val layouts: t vector * (Layout.t -> unit) -> unit
end

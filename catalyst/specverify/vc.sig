signature VERIFICATION_CONDITION_STRUCTS =
sig
  include SPEC_LANG
  structure VE : VAR_ENV
  structure RE : REL_ENV
  sharing type RefinementTypeScheme.t = VE.tyscheme
  sharing type Var.t = VE.Var.t
  sharing type RelLang.RelId.t = RE.SpecLang.RelLang.RelId.t
  sharing type RelLang.RelTypeScheme.t = RE.SpecLang.RelLang.RelTypeScheme.t
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

  datatype t = T of tydbinds * vc_pred* vc_pred
  
  val fromTypeCheck : VE.t * RefinementType.t * RefinementType.t -> t vector

  val elaborate : RE.t * t -> t

  val layout : t vector -> Layout.t

  val layouts: t vector * (Layout.t -> unit) -> unit
end

signature VC_SOLVE_STRUCTS =
sig
  structure VC : VERIFICATION_CONDITION
end
signature VC_SOLVE =
sig
  include VC_SOLVE_STRUCTS

  structure HoleMap: APPLICATIVE_MAP where
    type Key.t = string (* VC.Predicate.Hole.id*) and 
    type Value.t = VC.Predicate.RelPredicate.t vector

  val solve : VC.t vector -> HoleMap.t
end

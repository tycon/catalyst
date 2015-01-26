signature RUNNER_STRUCTS =
sig
  structure CoreML : CORE_ML
  structure VE : VAR_ENV
  structure RE : REL_ENV
  sharing VE.SpecLang = RE.SpecLang
  sharing CoreML.TypeDesc = VE.SpecLang.TypeDesc
  sharing CoreML.Var = VE.SpecLang.Var
  sharing CoreML.Con = VE.SpecLang.Con
  sharing VE.Var = VE.SpecLang.Var
  structure HM : APPLICATIVE_MAP where
    type Key.t = VE.SpecLang.Predicate.Hole.id and 
    type Value.t = VE.SpecLang.Predicate.RelPredicate.t vector

end
signature RUNNER =
sig
  include RUNNER_STRUCTS
  val setInputFile : string -> unit
  val refineHM : CoreML.Dec.t vector -> VE.t -> RE.t -> HM.t -> HM.t
end

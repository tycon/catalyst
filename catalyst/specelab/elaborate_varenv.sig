signature ELABORATE_VAR_ENV_STRUCTS =
sig
  structure SpecLang : SPEC_LANG
  structure ANormalCoreML : A_NORMAL_CORE_ML
  sharing SpecLang.Con = ANormalCoreML.Con
  sharing SpecLang.TypeDesc = ANormalCoreML.TypeDesc
  sharing SpecLang.Tycon = ANormalCoreML.Tycon
  sharing SpecLang.Tyvar = ANormalCoreML.Tyvar
  sharing SpecLang.Var = ANormalCoreML.Var
  sharing SpecLang.Record = ANormalCoreML.Record
  sharing SpecLang.Const = ANormalCoreML.Const
  sharing SpecLang.Prim = ANormalCoreML.Prim
  sharing SpecLang.SourceInfo = ANormalCoreML.SourceInfo
end
signature ELABORATE_VAR_ENV =
sig
  include ELABORATE_VAR_ENV_STRUCTS
  structure VE : VAR_ENV
  structure RE : REL_ENV
  sharing VE.SpecLang = RE.SpecLang

  val elaborate : ANormalCoreML.Program.t -> SpecLang.RelSpec.t -> (VE.t * RE.t) 
end

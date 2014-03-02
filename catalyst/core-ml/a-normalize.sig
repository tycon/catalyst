(* 
 * Author : GK
 *)

signature A_NORMALIZE_STRUCTS = 
   sig
      structure CoreML: CORE_ML
      structure ANormalCoreML : A_NORMAL_CORE_ML
      sharing CoreML.Con = ANormalCoreML.Con
      sharing CoreML.Type = ANormalCoreML.Type
      sharing CoreML.Tycon = ANormalCoreML.Tycon
      sharing CoreML.Tyvar = ANormalCoreML.Tyvar
      sharing CoreML.Var = ANormalCoreML.Var
      sharing CoreML.Record = ANormalCoreML.Record
      sharing CoreML.Const = ANormalCoreML.Const
      sharing CoreML.Prim = ANormalCoreML.Prim
      sharing CoreML.SourceInfo = ANormalCoreML.SourceInfo
   end

signature A_NORMALIZE = 
   sig
      include A_NORMALIZE_STRUCTS

      val doIt: CoreML.Program.t -> ANormalCoreML.Program.t
   end

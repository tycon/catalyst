signature SPEC_FRONTEND_STRUCTS =
sig
  structure Spec : SPEC_LANG
end

signature SPEC_FRONTEND = 
sig
   include SPEC_FRONTEND_STRUCTS

   val lexAndParseSpecFile : File.t -> Spec.RelSpec.t
end

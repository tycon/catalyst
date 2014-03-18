(* Copyright (C) 2011 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Compile (S: COMPILE_STRUCTS): COMPILE =
struct

open S

(*---------------------------------------------------*)
(*              Intermediate Languages               *)
(*---------------------------------------------------*)

structure Symbol = Symbol ()
structure Field = Field (structure Symbol = Symbol)
structure Record = Record (val isSorted = false
                           structure Field = Field)
structure SortedRecord = Record (val isSorted = true
                                 structure Field = Field)
structure Tyvar = Tyvar ()
structure Ast = Ast (structure Record = Record
                     structure SortedRecord = SortedRecord
                     structure Symbol = Symbol
                     structure Tyvar = Tyvar)
local
   open Ast.Tycon
in
   structure CharSize = CharSize
   structure IntSize = IntSize
   structure RealSize = RealSize
   structure WordSize = WordSize
end
structure Atoms = Atoms (structure CharSize = CharSize
                         structure Field = Field
                         structure IntSize = IntSize
                         structure RealSize = RealSize
                         structure Record = Record
                         structure SortedRecord = SortedRecord
                         structure Tyvar = Tyvar
                         structure WordSize = WordSize)
local
   open Atoms
in
   structure Const = Const
   structure ConstType = Const.ConstType
   structure Ffi = Ffi
   structure WordX = WordX
end
structure TypeEnv = TypeEnv (Atoms)
structure CoreML = CoreML (open Atoms
                           structure Type =
                              struct
                                 open TypeEnv.Type

                                 val makeHom =
                                    fn {con, var} =>
                                    makeHom {con = con,
                                             expandOpaque = true,
                                             var = var}

                                 fun layout t = 
                                    layoutPrettyAux 
                                    (t, {expandOpaque = true,
                                         localTyvarNames = false})
                              end)
structure ANormalCoreML = ANormalCoreML (open Atoms
                               structure Type =
                                  struct
                                     open TypeEnv.Type

                                     val makeHom =
                                        fn {con, var} =>
                                        makeHom {con = con,
                                                 expandOpaque = true,
                                                 var = var}

                                     fun layout t = 
                                        layoutPrettyAux 
                                        (t, {expandOpaque = true,
                                             localTyvarNames = false})
                                  end)
(*-----------------------------------------------------*)
(*             Specification Language                  *)
(*-----------------------------------------------------*)

structure SpecLang = SpecLang (open Atoms)

(*-----------------------------------------------------*)
(*                  Processing Passes                  *)
(*-----------------------------------------------------*)

structure FrontEnd = FrontEnd (structure Ast = Ast)
structure MLBFrontEnd = MLBFrontEnd (structure Ast = Ast
                                     structure FrontEnd = FrontEnd)
structure DeadCode = DeadCode (structure CoreML = CoreML)
structure Elaborate = Elaborate (structure Ast = Ast
                                 structure CoreML = CoreML
                                 structure TypeEnv = TypeEnv)
structure SpecFrontend = SpecFrontend (structure Spec = SpecLang)
local
   open Elaborate
in
   structure Env = Env
end
structure LookupConstant = LookupConstant (structure Const = Const
                                           structure ConstType = ConstType
                                           structure Ffi = Ffi)
structure ANormalize = ANormalize (structure CoreML = CoreML
                                   structure ANormalCoreML = ANormalCoreML)
structure ElaborateVarEnv = ElaborateVarEnv (structure SpecLang = SpecLang
                                   structure ANormalCoreML = ANormalCoreML)
structure VE = ElaborateVarEnv.VE
structure RE = ElaborateVarEnv.RE
structure PRE = ElaborateVarEnv.PRE

structure SpecVerify = SpecVerify (structure VE = VE
                                   structure RE = RE
                                   structure PRE = PRE
                                   structure ANormalCoreML = ANormalCoreML)

structure VC = SpecVerify.VC

val (z3_log,z3_log_close) = (fn stream => 
  (fn str => (Out.output (stream,str);
      Out.flush stream), 
   fn () => Out.close stream)) 
   (Out.openOut "catalyst.z3")

structure VCE = VCEncode (structure VC = VC
                          val z3_log = z3_log)

(* ------------------------------------------------- *)
(*                 Lookup Constant                   *)
(* ------------------------------------------------- *)

val commandLineConstants: {name: string, value: string} list ref = ref []
fun setCommandLineConstant (c as {name, value}) =
   let
      fun make (fromString, control) =
         let
            fun set () =
               case fromString value of
                  NONE => Error.bug (concat ["bad value for ", name])
                | SOME v => control := v
         in
            set
         end
      val () =
         case List.peek ([("Exn.keepHistory", 
                           make (Bool.fromString, Control.exnHistory))],
                         fn (s, _) => s = name) of
            NONE => ()
          | SOME (_,set) => set ()
   in
      List.push (commandLineConstants, c)
   end

val allConstants: (string * ConstType.t) list ref = ref []
val amBuildingConstants: bool ref = ref false

val lookupConstant =
   let
      val zero = Const.word (WordX.fromIntInf (0, WordSize.word32))
      val f =
         Promise.lazy
         (fn () =>
          if !amBuildingConstants
             then (fn ({name, default, ...}, t) =>
                   let
                      (* Don't keep constants that already have a default value.
                       * These are defined by _command_line_const and set by
                       * -const, and shouldn't be looked up.
                       *)
                      val () =
                         if isSome default
                            then ()
                         else List.push (allConstants, (name, t))
                   in
                      zero
                   end)
          else
             File.withIn
             (concat [!Control.libTargetDir, "/constants"], fn ins =>
              LookupConstant.load (ins, !commandLineConstants)))
   in
      fn z => f () z
   end

(* ------------------------------------------------- *)   
(*                   Primitive Env                   *)
(* ------------------------------------------------- *)

local
   structure Con = TypeEnv.Con
   structure Tycon = TypeEnv.Tycon
   structure Type = TypeEnv.Type
   structure Tyvar = TypeEnv.Tyvar

   val primitiveDatatypes =
      Vector.new3
      ({tycon = Tycon.bool,
        tyvars = Vector.new0 (),
        cons = Vector.new2 ({con = Con.falsee, arg = NONE},
                            {con = Con.truee, arg = NONE})},
       let
          val a = Tyvar.newNoname {equality = false}
       in
          {tycon = Tycon.list,
           tyvars = Vector.new1 a,
           cons = Vector.new2 ({con = Con.nill, arg = NONE},
                               {con = Con.cons,
                                arg = SOME (Type.tuple
                                            (Vector.new2
                                             (Type.var a,
                                              Type.list (Type.var a))))})}
       end,
       let
          val a = Tyvar.newNoname {equality = false}
       in
          {tycon = Tycon.reff,
           tyvars = Vector.new1 a,
           cons = Vector.new1 {con = Con.reff, arg = SOME (Type.var a)}}
       end)

   val primitiveExcons =
      let
         open CoreML.Con
      in
         [bind, match, overflow]
      end

   structure Con =
      struct
         open Con

         fun toAst c =
            Ast.Con.fromSymbol (Symbol.fromString (Con.toString c),
                                Region.bogus)
      end

   structure Env =
      struct
         open Env 

         structure Tycon =
            struct
               open Tycon

               fun toAst c =
                  Ast.Tycon.fromSymbol (Symbol.fromString (Tycon.toString c),
                                        Region.bogus)
            end
         structure Type = TypeEnv.Type
         structure Scheme = TypeEnv.Scheme

         fun addPrim (E: t): unit =
            let
               val _ =
                  List.foreach
                  (Tycon.prims, fn {kind, name, tycon, ...} =>
                   extendTycon
                   (E, Ast.Tycon.fromSymbol (Symbol.fromString name,
                                             Region.bogus),
                    TypeStr.tycon (tycon, kind),
                    {forceUsed = false, isRebind = false}))
               val _ =
                  Vector.foreach
                  (primitiveDatatypes, fn {tyvars, tycon, cons} =>
                   let
                      val cons =
                         Env.newCons
                         (E, Vector.map (cons, fn {con, ...} =>
                                         {con = con, name = Con.toAst con}))
                         (Vector.map
                          (cons, fn {arg, ...} =>
                           let
                              val resultType =
                                 Type.con (tycon, Vector.map (tyvars, Type.var))
                           in
                              Scheme.make
                              {canGeneralize = true,
                               ty = (case arg of
                                        NONE => resultType
                                      | SOME t => Type.arrow (t, resultType)),
                               tyvars = tyvars}
                           end))
                   in
                      extendTycon
                      (E, Tycon.toAst tycon,
                       TypeStr.data (tycon,
                                     TypeStr.Kind.Arity (Vector.length tyvars),
                                     cons),
                       {forceUsed = false, isRebind = false})
                   end)
               val _ =
                  extendTycon (E,
                               Ast.Tycon.fromSymbol (Symbol.unit, Region.bogus),
                               TypeStr.def (Scheme.fromType Type.unit,
                                            TypeStr.Kind.Arity 0),
                               {forceUsed = false, isRebind = false})
               val scheme = Scheme.fromType Type.exn
               val _ = List.foreach (primitiveExcons, fn c =>
                                     extendExn (E, Con.toAst c, c, SOME scheme))
            in
               ()
            end
      end

   val primitiveDecs: CoreML.Dec.t list =
      let
         open CoreML.Dec
      in
         List.concat [[Datatype primitiveDatatypes],
                      List.map
                      (primitiveExcons, fn c =>
                       Exception {con = c, arg = NONE})]
      end

in

   fun addPrim E =
      (Env.addPrim E
       ; primitiveDecs)
end

(* ------------------------------------------------- *)
(*                 parseAndElaborateMLB              *)
(* ------------------------------------------------- *)

fun quoteFile s = concat ["\"", String.escapeSML s, "\""]

structure MLBString:>
   sig
      type t

      val fromFile: File.t -> t
      val fromString: string -> t
      val lexAndParseMLB: t -> Ast.Basdec.t
   end =
   struct
      type t = string

      val fromFile = quoteFile

      val fromString = fn s => s

      val lexAndParseMLB = MLBFrontEnd.lexAndParseString
   end

val lexAndParseMLB = MLBString.lexAndParseMLB

val lexAndParseMLB: MLBString.t -> Ast.Basdec.t = 
   fn input =>
   let
      val ast = lexAndParseMLB input
      val _ = Control.checkForErrors "parse"
   in
      ast
   end

fun sourceFilesMLB {input} =
   Ast.Basdec.sourceFiles (lexAndParseMLB (MLBString.fromFile input))

val elaborateMLB = Elaborate.elaborateMLB

val displayEnvDecs =
   Control.Layouts
   (fn ((_, decs),output) =>
    (output (Layout.str "\n\n")
     ; Vector.foreach
       (decs, fn (dec, dc) =>
        (output o Layout.record)
        [("deadCode", Bool.layout dc),
         ("decs", List.layout CoreML.Dec.layout dec)])))

fun parseAndElaborateMLB (input: MLBString.t)
   : Env.t * (CoreML.Dec.t list * bool) vector =
   Control.pass
   {display = displayEnvDecs,
    name = "parseAndElaborate",
    stats = fn _ => Layout.empty,
    style = Control.ML,
    suffix = "core-ml",
    thunk = (fn () =>
             (Const.lookup := lookupConstant
              ; elaborateMLB (lexAndParseMLB input, {addPrim = addPrim})))}



(* ------------------------------------------------- *)
(*                      compile                      *)
(* ------------------------------------------------- *)

exception Done

fun elaborate {input: MLBString.t}: CoreML.Program.t =
   let
      val (E, decs) = parseAndElaborateMLB input
      val _ = Env.processDefUse E
      val _ =
               let
                  open Control
               in
                  if !keepElaboratedEnv
                     then saveToFile ({suffix = "env"}, No, E,
                                      Layout Env.layout)
                  else ()
               end
      val _ = if !Control.elaborateOnly then raise Done else ()
      (* parseAndElaborateMLB returns a bool flag *)
      (* which we want to get rid of using dead-code pass *)
      val decs =
         Control.pass
         {display = Control.Layouts (fn (decss,output) =>
                                     (output (Layout.str "\n\n")
                                      ; Vector.foreach (decss, fn decs =>
                                        List.foreach (decs, fn dec =>
                                        output (CoreML.Dec.layout dec))))),
          name = "deadCode",
          suffix = "core-ml",
          style = Control.ML,
          stats = fn _ => Layout.empty,
          thunk = fn () => let
                              val {prog = decs} =
                                 DeadCode.deadCode {prog = decs}
                           in
                              decs
                           end}
      val decs = Vector.concatV (Vector.map (decs, Vector.fromList))
      val coreML = CoreML.Program.T {decs = decs}
    in
      coreML
    end

val elaborateMLB =
   fn {input: File.t} =>
   (ignore (elaborate {input = MLBString.fromFile input}))
   handle Done => ()

local
   open Atoms
   open Control
   structure Type = TypeEnv.Type
   val primitiveDatatypes =
      Vector.new1
      (let
          val a = Tyvar.newNoname {equality = false}
       in
          {tycon = Tycon.list,
           tyvars = Vector.new1 a,
           cons = Vector.new3 ({con = Con.nill, arg = NONE},
                               {con = Con.cons,
                                arg = SOME (Type.tuple
                                            (Vector.new2
                                             (Type.var a,
                                              Type.list (Type.var a))))},
                               {con = Con.fromString "cons",
                                arg = SOME (Type.tuple
                                            (Vector.new2
                                             (Type.var a,
                                              Type.list (Type.var a))))})}
       end
       (*,{tycon = Tycon.bool,
        tyvars = Vector.new0 (),
        cons = Vector.new2 ({con = Con.truee, arg = NONE},
                            {con = Con.falsee, arg = NONE})
       }*))
   fun genMLB {input: File.t list}: MLBString.t =
      let
         val basis = "$(SML_LIB)/basis/default.mlb"
      in
         MLBString.fromString
         (case input of
             [] => basis
           | _ =>
                let
                   val input = List.map (input, quoteFile)
                in
                   String.concat
                   ["local\n",
                    basis, "\n",
                    "in\n",
                    String.concat (List.separate (input, "\n")), "\n",
                    "end\n"]
                end)
      end
in
   val elaborateSML =
      fn {input: File.t list} =>
      (ignore (elaborate {input = genMLB {input = input}}))
      handle Done => ()

   val elaborateSMLWithSpec =
      fn {spec : File.t} => fn {input: File.t list} =>
        let val CoreML.Program.T{decs} = elaborate {input = genMLB {input = input}}
            val specast = SpecFrontend.lexAndParseSpecFile (spec)
            val catexpi = Vector.index (decs,fn dec => case dec of
                CoreML.Dec.Exception {arg,con} => String.compare 
                  (Con.toString con, "Catalyst") = EQUAL
              | _ => false)
            val starti = case catexpi of SOME i => i
              | NONE => Error.bug "Exception Catalyst not declared"
            val userDecs = Vector.dropPrefix (decs,starti+1)
            val primitiveDecs = Vector.map (primitiveDatatypes,
              (CoreML.Dec.Datatype o Vector.new1))
            val coreML = CoreML.Program.T{decs = Vector.concat 
              [primitiveDecs, userDecs]}
            val ancoreML = Control.pass 
              {
                display = Control.NoDisplay,
                name = "A-Normalize",
                stats = fn _ => Layout.empty,
                style = Control.ML,
                suffix = "ancore-ml",
                thunk = (fn () => ANormalize.doIt coreML)
              }
            val _ =
               let open Control
               in
                  if !keepCoreML
                     then (saveToFile ({suffix = "core-ml"}, No, coreML,
                                      Layouts CoreML.Program.layouts);
                           saveToFile ({suffix = "an-core-ml"}, No, ancoreML,
                                      Layouts ANormalCoreML.Program.layouts))
                  else ()
               end
            fun $ (f,arg) = f arg
            infixr 5 $
            val speclang = specast
            val (ve,re,pre) = Control.pass 
              {
                display = Control.NoDisplay,
                name = "Spec Elab",
                stats = fn _ => Layout.empty,
                style = Control.ML,
                suffix = "elr",
                thunk = (fn () => ElaborateVarEnv.elaborate ancoreML speclang)
              }
            (* 
             * Hack : ML has ::, but not cons. So, ty(::) <- ty(cons) 
             * and remove cons from ve.
             *)
            val consvid = Var.fromString "cons"
            val consvid' = Var.fromString $ Con.toString Con.cons
            val consty = VE.find ve consvid
            val ve = VE.add (VE.remove (VE.remove ve consvid) consvid')
              (consvid',consty)
            (*
            val _ = print "Specification Ast:\n"
            val _ = Control.message (Control.Top, fn _ =>
              SpecLang.RelSpec.layout specast)
            *)
            val _ = print "Var Env:\n"
            val _ = Control.message (Control.Top, fn _ =>
              VE.layout ve)
            val _ = print "Rel Env:\n"
            val _ = Control.message (Control.Top, fn _ =>
              RE.layout re)
            val _ = print "Param Rel Env:\n"
            val _ = Control.message (Control.Top, fn _ =>
              PRE.layout pre)
            val _ = print "\n"
            val vcs = Control.pass 
              {
                display = Control.NoDisplay,
                name = "Spec Verify",
                stats = fn _ => Layout.empty,
                style = Control.ML,
                suffix = "specverify",
                thunk = (fn () =>SpecVerify.doIt (ve,PRE.empty,ancoreML))
              }
            fun layouts (vcs,output) = (
              output $ Layout.str "Elaborated VarEnv:\n";
              output $ VE.layout ve;
              VC.layouts (vcs,output))
            val _ = Control.saveToFile ({suffix = "vcs"}, No, vcs,
                                      Layouts layouts)
            val elabvcs = Control.pass 
              {
                display = Control.NoDisplay,
                name = "Elaborate VCs",
                stats = fn _ => Layout.empty,
                style = Control.ML,
                suffix = "elabvcs",
                thunk = (fn () =>Vector.map (vcs, fn vc =>
                    VC.elaborate (re,pre,vc)))
              }
            val _ = Control.saveToFile ({suffix = "evcs"}, No, elabvcs,
                                      Layouts VC.layouts)
            exception CantDischargeVC
            fun dischargeVC (i,vc) = case VCE.discharge vc of
                VCE.Success => print ("VC# "^(Int.toString i)^" discharged\n")
              | VCE.Undef => (print ("Solver timeout  while trying to \
                  \discharge VC #"^(Int.toString i)); 
                  z3_log_close ();
                  raise CantDischargeVC)
              | VCE.Failure => (print ("VC # " ^(Int.toString i)^
                " is invalid!"); 
                  z3_log_close ();
                  raise CantDischargeVC)
            val _ = Control.pass 
              {
                display = Control.NoDisplay,
                name = "Discharge VCs",
                stats = fn _ => Layout.empty,
                style = Control.ML,
                suffix = "discharge",
                thunk = (fn () => Vector.foreachi (elabvcs,dischargeVC))
              }
            val _ = z3_log_close ()
         in
            print $ (!Control.inputFile)^" is correct w.r.t given specification!\n"
         end
end

end

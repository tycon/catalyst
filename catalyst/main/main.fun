(* Copyright (C) 2010-2011,2013 Matthew Fluet.
 * Copyright (C) 1999-2009 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Main (S: MAIN_STRUCTS): MAIN =
struct

open S

structure Compile = Compile ()

structure Place =
   struct
      datatype t = MLB | SML
   end

structure OptPred =
   struct
      datatype t =
         Target of string
       | Yes
   end

val expert: bool ref = ref false
val keepGenerated = ref false
val output: string option ref = ref NONE

fun parseMlbPathVar (line: String.t) =
   case String.tokens (line, Char.isSpace) of
      [var, path] => SOME {var = var, path = path}
    | _ => NONE

fun readMlbPathMap (file: File.t) =
   if not (File.canRead file) then
      Error.bug (concat ["can't read MLB path map file: ", file])
   else
      List.keepAllMap
      (File.lines file, fn line =>
       if String.forall (line, Char.isSpace)
          then NONE
       else
          case parseMlbPathVar line of
             NONE => Error.bug (concat ["strange mlb path mapping: ",
                                        file, ":: ", line])
           | SOME v => SOME v)


fun makeOptions {usage} =
   let
      val usage = fn s => (ignore (usage s); raise Fail "unreachable")
      open Control Popt
   in
      List.map
      (
       [
       (Expert, "expert", " {false|true}", "enable expert status",
        boolRef expert),
       (Normal, "keep", " {g|o}", "save intermediate files",
        SpaceString (fn s =>
                     case s of
                        "core-ml" => keepCoreML := true
                      | _ => usage (concat ["invalid -keep flag: ", s]))),
       (Normal, "spec", " <file>", "specification file name",
        SpaceString (fn s => specFile := SOME s)),
       (Expert, "keep-elab-env", " {true|false}", "keep elaborated env",
        boolRef keepElaboratedEnv),
       (Expert, "keep-pass", " <pass>", "keep the results of pass",
        SpaceString
        (fn s => (case Regexp.fromString s of
                     SOME (re,_) => let val re = Regexp.compileDFA re
                                    in List.push (keepPasses, re)
                                    end
                   | NONE => usage (concat ["invalid -keep-pass flag: ", s])))),
       (Normal, "mlb-path-map", " <file>", "additional MLB path map",
        SpaceString (fn s => mlbPathVars := !mlbPathVars @ readMlbPathMap s)),
       (Normal, "mlb-path-var", " '<name> <value>'", "additional MLB path var",
        SpaceString
        (fn s => mlbPathVars := !mlbPathVars @
                                [case parseMlbPathVar s of
                                    NONE => Error.bug ("strange mlb path var: " ^ s)
                                  | SOME v => v])),
       (Expert, "show-types", " {true|false}", "show types in ILs",
        boolRef showTypes),
       (Expert, #1 trace, " name1,...", "trace typechecker internals", #2 trace),
       (Normal, "verbose", " {0|1|2|3}", "how verbose to be",
        SpaceString
        (fn s =>
         verbosity := (case s of
                          "0" => Silent
                        | "1" => Top
                        | "2" => Pass
                        | "3" => Detail
                        | _ => usage (concat ["invalid -verbose arg: ", s])))),
       (Expert, "warn-ann", " {true|false}",
        "unrecognized annotation warnings",
        boolRef warnAnn),
       (Expert, "warn-deprecated", " {true|false}",
        "deprecated feature warnings",
        boolRef warnDeprecated)
       ],
       fn (style, name, arg, desc, opt) =>
       {arg = arg, desc = desc, name = name, opt = opt, style = style})
   end

val mainUsage =
   "mlton [option ...] file.{c|mlb|o|sml} [file.{c|o|s|S} ...]"

val {parse, usage} =
   Popt.makeUsage {mainUsage = mainUsage,
                   makeOptions = makeOptions,
                   showExpert = fn () => !expert}

val usage = fn s => (usage s; raise Fail "unreachable")

fun commandLine (args: string list): unit =
   let
      open Control
      val args =
         case args of
            lib :: args =>
               (libDir := OS.Path.mkCanonical lib
                ; args)
          | _ => Error.bug "incorrect args from shell script"
      val result = parse args
      val targetStr = "self"
      (* GK - We need this for SML_LIB path var *)
      val targetsDir =
         OS.Path.mkAbsolute { path = "targets",
                              relativeTo = !libDir }
      val targetDir =
         OS.Path.mkAbsolute { path = targetStr,
                              relativeTo = targetsDir }
      val () = libTargetDir := targetDir
      (* GK - This should go. *)
      val () =
         let
            val sizeMap =
               List.map
               (File.lines (OS.Path.joinDirFile {dir = !Control.libTargetDir,
                                                 file = "sizes"}),
                fn line =>
                case String.tokens (line, Char.isSpace) of
                   [ty, "=", size] =>
                      (case Int.fromString size of
                          NONE => Error.bug (concat ["strange size: ", size])
                        | SOME size =>
                             (ty, Bytes.toBits (Bytes.fromInt size)))
                 | _ => Error.bug (concat ["strange size mapping: ", line]))
            fun lookup ty' =
               case List.peek (sizeMap, fn (ty, _) => String.equals (ty, ty')) of
                  NONE => Error.bug (concat ["missing size mapping: ", ty'])
                | SOME (_, size) => size
         in
            Control.Target.setSizes
            {cint = lookup "cint",
             cpointer = lookup "cpointer",
             cptrdiff = lookup "cptrdiff",
             csize = lookup "csize",
             header = lookup "header",
             mplimb = lookup "mplimb",
             objptr = lookup "objptr",
             seqIndex = lookup "seqIndex"}
         end

      fun printVersion (out: Out.t): unit =
         Out.output (out, concat [Version.banner, "\n"])
   in
      case result of
      Result.No msg => usage msg
    | Result.Yes [] => usage "No file arguments provided"
    | Result.Yes (input :: rest) =>
         let
            val _ = inputFile := File.base (File.fileOf input)
            val (start, base) =
               let
                  val rec loop =
                     fn [] => usage (concat ["invalid file suffix on ", input])
                      | (suf, start, hasNum) :: sufs =>
                           if String.hasSuffix (input, {suffix = suf})
                              then (start,
                                    let
                                       val f = File.base input
                                    in
                                       if hasNum
                                          then File.base f
                                       else f
                                    end)
                           else loop sufs
                  datatype z = datatype Place.t
               in
                  loop [(".mlb", MLB, false),
                        (".sml", SML, false)]
               end
            val _ =
               if !verbosity = Top
                  then printVersion Out.error
               else ()
          fun mkCompileSrc {listFiles, elaborate} input =
             let
                val outputs: File.t list ref = ref []
                val r = ref 0
                val _ =
                   case !verbosity of
                      Silent => ()
                    | Top => ()
                    | _ =>
                         outputHeader
                         (Control.No, fn l =>
                          let val out = Out.error
                          in Layout.output (l, out)
                             ; Out.newline out
                          end)
             in
                trace (Top, "Elaborate and Catalyze SML")
                elaborate
                {input = input}
             end
          val processSML =
             mkCompileSrc {listFiles = fn {input} => Vector.fromList input,
                           elaborate = (case !specFile of 
                                NONE => Compile.elaborateSML
                              | SOME f => Compile.elaborateSMLWithSpec {spec = f})}
          val processMLB =
             mkCompileSrc {listFiles = Compile.sourceFilesMLB,
                           elaborate = Compile.elaborateMLB}
          fun process () =
             case start of
                Place.SML => processSML [input]
              | Place.MLB => processMLB input
          val doit
            = trace (Top, "Catalyze")
              (fn () =>
               Exn.finally
               (process, fn () => () ))
       in
          messageStr(Top,"Catalyst version 0.1");
          doit ()
       end
   end

val commandLine = Process.makeCommandLine commandLine

val main = fn (_, args) => commandLine args

val mainWrapped = fn () => OS.Process.exit (commandLine (CommandLine.arguments ()))

end

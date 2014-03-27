functor SpecFrontend(S : SPEC_FRONTEND_STRUCTS) : SPEC_FRONTEND = 
struct
  open S

  exception ParseError of string * int option * int option

  structure SpecLrVals = SpecLrValsFun(structure Token = LrParser.Token
                                       structure Spec = Spec)
  structure SpecLex = SpecLexFun(structure Tokens = SpecLrVals.Tokens)
  structure SpecParser = Join (structure LrParser = LrParser
                                 structure ParserData = SpecLrVals.ParserData
                                 structure Lex = SpecLex)
  fun lexAndParse (ins : In.t): Spec.RelSpec.t = 
    let val grab : int -> string = fn n => In.inputN (ins, n)
        val handleParseError : string * int * int -> unit = fn
            (msg,line,col) => 
            raise ParseError(msg,SOME(line),SOME(col))
        (* Position annotated Token stream *)
        val lexer = SpecParser.makeLexer grab
        val (spec,_) = SpecParser.parse
                    (15, lexer, handleParseError,())
                    handle SpecParser.ParseError => raise ParseError 
                      ("Unknown Parse Error",NONE,NONE)
        val dummyEOF = SpecLrVals.Tokens.EOF(0,0)
    in
      SpecLex.UserDeclarations.line := 1;
      spec
    end

  fun lexAndParseSpecFile (f: File.t) =
     File.withIn
     (f, fn ins => lexAndParse ins)

  val lexAndParseFile =
      Trace.trace ("SpecFrontend.lexAndParseFile", File.layout, Spec.RelSpec.layout)
      lexAndParseSpecFile
end

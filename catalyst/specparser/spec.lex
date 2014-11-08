structure Tokens = Tokens
type pos = int
type ('a,'b) token = ('a,'b) Tokens.token
type svalue = Tokens.svalue
type lexresult = (svalue,pos) token
val line = ref 1
val debugFlag = ref false
val eof = fn () => Tokens.EOF(!line,!line)
val debug = fn s => if (!debugFlag) then print s else () 
(*
  Spec_TOKENS defined using term declaration in grm
*)
%%
%s COMMENT;
%header (functor SpecLexFun (structure Tokens : Spec_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
variable=({alpha}|"_")+{digit}*("'")*;
number={digit}+;
eol=("\n"|"\013\n"|"\013");
ws=[\ \t];
%%

<INITIAL>{eol} => (line := (!line)+1; lex());
<INITIAL>{ws}+ => (debug "whitespace"; lex());
<INITIAL>("relation") => (debug "relation"; Tokens.RELATION(!line,yypos));
<INITIAL>("true") => (debug "true"; Tokens.TRUE(!line,yypos));
<INITIAL>("assume") => (debug "assume"; Tokens.ASSUME(!line,yypos));
<INITIAL>("false") => (debug "false"; Tokens.FALSE(!line,yypos));
<INITIAL>("not") => (debug "not"; Tokens.NOT(!line,yypos));
<INITIAL>("+") => (debug "plus"; Tokens.PLUS(!line,yypos));
<INITIAL>("-") => (debug "minus"; Tokens.MINUS(!line,yypos));
<INITIAL>("U") => (debug "union"; Tokens.UNION(!line,yypos));
<INITIAL>("X") => (debug "crossprd"; Tokens.CROSSPRD(!line,yypos));
<INITIAL>("C=") => (debug "subseteq"; Tokens.SUBSETEQ(!line,yypos));
<INITIAL>("C") => (debug "subset"; Tokens.SUBSET(!line,yypos));
<INITIAL>("=") => (debug "equalop";Tokens.EQUALOP(!line,yypos));
<INITIAL>("=>") => (debug "implies";Tokens.IMP(!line,yypos));
<INITIAL>("??") => (debug "hole";Tokens.HOLE(!line,yypos));
<INITIAL>("<=>") => (debug "iff";Tokens.IFF(!line,yypos));
<INITIAL>("/\\") => (debug "conj";Tokens.CONJ(!line,yypos));
<INITIAL>("\\/") => (debug "disj";Tokens.DISJ(!line,yypos));
<INITIAL>(":") => (debug "colon\n";Tokens.COLON(!line,yypos));
<INITIAL>(";") => (debug "semicolon\n";Tokens.SEMICOLON(!line,yypos));
<INITIAL>(",") => (debug "comma\n";Tokens.COMMA(!line,yypos));
<INITIAL>("*") => (debug "star\n";Tokens.STAR(!line,yypos));
<INITIAL>("(") => (debug "lparen\n"; Tokens.LPAREN(!line,yypos));
<INITIAL>(")") => (debug "rparen\n"; Tokens.RPAREN(!line,yypos));
<INITIAL>("{") => (debug "lcurly\n"; Tokens.LCURLY(!line,yypos));
<INITIAL>("}") => (debug "rcurly\n"; Tokens.RCURLY(!line,yypos));
<INITIAL>("[") => (debug "lbrace\n"; Tokens.LBRACE(!line,yypos));
<INITIAL>("]") => (debug "rbrace\n"; Tokens.RBRACE(!line,yypos));
<INITIAL>("->") => (debug "arrow\n"; Tokens.ARROW(!line,yypos));
<INITIAL>("(*") => (debug "comment begin\n"; YYBEGIN COMMENT; continue ());
<COMMENT>. => (continue());
<COMMENT>{ws}+ => (continue());
<COMMENT>{eol} => (line := (!line)+1; continue());
<COMMENT>("*)") => (debug "comment end\n"; YYBEGIN INITIAL; continue ());
<INITIAL>("|") => (debug "pipe\n"; Tokens.PIPE(!line,yypos));
<INITIAL>{variable} => (debug ("var: "^yytext^"\n"); Tokens.ID(yytext,!line,yypos));
<INITIAL>{number} => (debug ("int: "^yytext^"\n"); 
                      case Int.fromString yytext of
                          SOME n => Tokens.INT(n,!line,yypos) 
                        | NONE => raise (Fail "Number couldn't be obtained")
                     );

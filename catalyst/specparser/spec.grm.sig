signature Spec_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val INT: (int) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val PIPE:  'a * 'a -> (svalue,'a) token
val ARROW:  'a * 'a -> (svalue,'a) token
val RBRACE:  'a * 'a -> (svalue,'a) token
val LBRACE:  'a * 'a -> (svalue,'a) token
val RCURLY:  'a * 'a -> (svalue,'a) token
val LCURLY:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val STAR:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val COLON:  'a * 'a -> (svalue,'a) token
val DISJ:  'a * 'a -> (svalue,'a) token
val CONJ:  'a * 'a -> (svalue,'a) token
val IFF:  'a * 'a -> (svalue,'a) token
val HOLE:  'a * 'a -> (svalue,'a) token
val IMP:  'a * 'a -> (svalue,'a) token
val EQUALOP:  'a * 'a -> (svalue,'a) token
val SUBSET:  'a * 'a -> (svalue,'a) token
val SUBSETEQ:  'a * 'a -> (svalue,'a) token
val CROSSPRD:  'a * 'a -> (svalue,'a) token
val UNION:  'a * 'a -> (svalue,'a) token
val MINUS:  'a * 'a -> (svalue,'a) token
val PLUS:  'a * 'a -> (svalue,'a) token
val NOT:  'a * 'a -> (svalue,'a) token
val ASSUME:  'a * 'a -> (svalue,'a) token
val FALSE:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val RELATION:  'a * 'a -> (svalue,'a) token
end
signature Spec_LRVALS=
sig
structure Tokens : Spec_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end

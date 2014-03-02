functor SpecLrValsFun (structure Token : TOKEN
                                structure Spec : SPEC_LANG) : Spec_LRVALS = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)(*  User declarations section for helper functions *)
open Spec
open RelLang
structure TypeSpec = RelSpec.TypeSpec
structure RefTy = RefinementType
val defaultCons = Con.default
val symbase = "sp_"
val count = ref 0
val genVar = fn _ => 
  let val id = symbase ^ (Int.toString (!count))
      val _ = count := !count + 1
  in
    Var.fromString id 
  end
fun $ (f,arg) = f arg
infixr 5 $

(*#line 25.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\002\000\073\000\005\000\072\000\021\000\071\000\023\000\044\000\
\\025\000\070\000\029\000\064\000\000\000\
\\001\000\002\000\088\000\003\000\087\000\022\000\086\000\029\000\085\000\
\\030\000\084\000\000\000\
\\001\000\002\000\088\000\003\000\087\000\029\000\085\000\030\000\084\000\000\000\
\\001\000\002\000\123\000\003\000\122\000\029\000\121\000\000\000\
\\001\000\010\000\097\000\011\000\096\000\012\000\095\000\000\000\
\\001\000\010\000\097\000\011\000\096\000\012\000\095\000\022\000\089\000\000\000\
\\001\000\012\000\029\000\021\000\028\000\029\000\027\000\000\000\
\\001\000\012\000\037\000\000\000\
\\001\000\012\000\075\000\000\000\
\\001\000\012\000\116\000\000\000\
\\001\000\017\000\012\000\000\000\
\\001\000\017\000\024\000\000\000\
\\001\000\018\000\163\000\027\000\165\000\000\000\
\\001\000\018\000\010\000\000\000\
\\001\000\018\000\011\000\000\000\
\\001\000\020\000\061\000\021\000\060\000\000\000\
\\001\000\021\000\023\000\023\000\022\000\029\000\021\000\000\000\
\\001\000\021\000\028\000\029\000\027\000\000\000\
\\001\000\021\000\045\000\023\000\044\000\029\000\043\000\000\000\
\\001\000\021\000\045\000\023\000\044\000\029\000\064\000\000\000\
\\001\000\021\000\060\000\000\000\
\\001\000\021\000\062\000\000\000\
\\001\000\022\000\049\000\000\000\
\\001\000\022\000\053\000\000\000\
\\001\000\022\000\089\000\000\000\
\\001\000\022\000\103\000\000\000\
\\001\000\022\000\105\000\000\000\
\\001\000\022\000\106\000\000\000\
\\001\000\022\000\117\000\000\000\
\\001\000\024\000\048\000\028\000\047\000\000\000\
\\001\000\024\000\094\000\000\000\
\\001\000\024\000\108\000\000\000\
\\001\000\024\000\119\000\000\000\
\\001\000\026\000\124\000\000\000\
\\001\000\026\000\125\000\000\000\
\\001\000\026\000\126\000\000\000\
\\001\000\027\000\030\000\000\000\
\\001\000\029\000\013\000\000\000\
\\001\000\029\000\014\000\000\000\
\\001\000\029\000\031\000\000\000\
\\001\000\029\000\039\000\000\000\
\\001\000\029\000\077\000\000\000\
\\001\000\029\000\081\000\000\000\
\\001\000\029\000\098\000\000\000\
\\001\000\031\000\000\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\001\000\009\000\004\000\008\000\029\000\007\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\028\000\036\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\021\000\056\000\029\000\055\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\019\000\104\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\\150\000\007\000\059\000\008\000\058\000\009\000\057\000\000\000\
\\151\000\000\000\
\\152\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\019\000\107\000\000\000\
\\156\000\000\000\
\\157\000\000\000\
\\158\000\000\000\
\\159\000\000\000\
\\160\000\000\000\
\\161\000\000\000\
\\162\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\019\000\050\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\013\000\093\000\014\000\092\000\015\000\091\000\016\000\090\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\"
val actionRowNumbers =
"\049\000\013\000\014\000\046\000\
\\045\000\010\000\037\000\038\000\
\\049\000\049\000\016\000\011\000\
\\006\000\048\000\047\000\084\000\
\\012\000\036\000\079\000\086\000\
\\039\000\016\000\016\000\053\000\
\\050\000\007\000\040\000\018\000\
\\016\000\029\000\081\000\022\000\
\\082\000\078\000\017\000\018\000\
\\023\000\056\000\067\000\063\000\
\\051\000\015\000\021\000\019\000\
\\080\000\000\000\087\000\085\000\
\\016\000\052\000\055\000\008\000\
\\057\000\058\000\041\000\019\000\
\\019\000\019\000\042\000\062\000\
\\001\000\024\000\020\000\089\000\
\\098\000\097\000\030\000\004\000\
\\043\000\000\000\000\000\094\000\
\\083\000\018\000\025\000\060\000\
\\064\000\065\000\066\000\026\000\
\\027\000\072\000\074\000\077\000\
\\031\000\076\000\075\000\071\000\
\\000\000\000\000\000\000\000\000\
\\088\000\019\000\019\000\019\000\
\\009\000\028\000\005\000\095\000\
\\054\000\059\000\041\000\070\000\
\\032\000\002\000\068\000\093\000\
\\092\000\091\000\090\000\102\000\
\\103\000\104\000\003\000\096\000\
\\061\000\069\000\073\000\033\000\
\\034\000\035\000\099\000\101\000\
\\100\000\044\000"
val gotoT =
"\
\\001\000\125\000\002\000\004\000\003\000\003\000\004\000\002\000\
\\015\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\013\000\004\000\002\000\015\000\001\000\000\000\
\\003\000\014\000\004\000\002\000\015\000\001\000\000\000\
\\016\000\018\000\018\000\017\000\020\000\016\000\021\000\015\000\000\000\
\\000\000\
\\005\000\024\000\006\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\018\000\032\000\019\000\031\000\020\000\030\000\021\000\015\000\000\000\
\\016\000\033\000\018\000\017\000\020\000\016\000\021\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\036\000\000\000\
\\010\000\040\000\011\000\039\000\012\000\038\000\000\000\
\\016\000\044\000\018\000\017\000\020\000\016\000\021\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\049\000\006\000\023\000\000\000\
\\010\000\050\000\011\000\039\000\012\000\038\000\000\000\
\\000\000\
\\008\000\052\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\061\000\012\000\038\000\000\000\
\\000\000\
\\011\000\067\000\012\000\038\000\022\000\066\000\024\000\065\000\
\\025\000\064\000\026\000\063\000\000\000\
\\000\000\
\\000\000\
\\018\000\032\000\019\000\072\000\020\000\030\000\021\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\074\000\000\000\
\\011\000\076\000\012\000\038\000\000\000\
\\011\000\077\000\012\000\038\000\000\000\
\\011\000\078\000\012\000\038\000\000\000\
\\000\000\
\\000\000\
\\013\000\081\000\014\000\080\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\098\000\012\000\038\000\022\000\097\000\024\000\065\000\
\\025\000\064\000\026\000\063\000\000\000\
\\011\000\067\000\012\000\038\000\024\000\065\000\025\000\064\000\
\\026\000\099\000\000\000\
\\000\000\
\\000\000\
\\010\000\100\000\011\000\039\000\012\000\038\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\067\000\012\000\038\000\022\000\107\000\024\000\065\000\
\\025\000\064\000\026\000\063\000\000\000\
\\011\000\067\000\012\000\038\000\022\000\108\000\024\000\065\000\
\\025\000\064\000\026\000\063\000\000\000\
\\011\000\067\000\012\000\038\000\022\000\109\000\024\000\065\000\
\\025\000\064\000\026\000\063\000\000\000\
\\011\000\067\000\012\000\038\000\022\000\110\000\024\000\065\000\
\\025\000\064\000\026\000\063\000\000\000\
\\000\000\
\\011\000\111\000\012\000\038\000\000\000\
\\011\000\112\000\012\000\038\000\000\000\
\\011\000\113\000\012\000\038\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\116\000\000\000\
\\000\000\
\\000\000\
\\013\000\081\000\014\000\118\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 126
val numrules = 61
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | INT of  (int) | ID of  (string) | patom of  (Predicate.t) | bpatom of  (Predicate.BasePredicate.t) | rpatom of  (Predicate.RelPredicate.t) | relpred of  (Predicate.RelPredicate.t) | pred of  (Predicate.t) | basety of  (RefinementType.t) | tyatom of  (RefinementType.t) | vartyatomseq of  ( ( Var.t * RefTy.t )  list) | vartyatom of  (Var.t*RefinementType.t) | reftyseq of  (RefinementType.t list) | refty of  (RefinementType.t) | typespec of  (TypeSpec.t) | elemseq of  (elem list) | elem of  (elem) | ratom of  (expr) | rexpr of  (expr) | rterm of  (term) | idseq of  (Var.t list) | conargs of  (Var.t vector) | conpat of  (Con.t*Var.t vector option) | patmatch of  (Con.t*Var.t vector option*term) | patmatchseq of  ( ( Con.t * Var.t vector option * term )  list) | reldec of  (StructuralRelation.t) | decsandtys of  (RelSpec.t) | spec of  (RelSpec.t) | start of  (RelSpec.t)
end
type svalue = MlyValue.svalue
type result = RelSpec.t
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 30) => true | _ => false
val showTerminal =
fn (T 0) => "RELATION"
  | (T 1) => "TRUE"
  | (T 2) => "FALSE"
  | (T 3) => "ASSUME"
  | (T 4) => "NOT"
  | (T 5) => "PLUS"
  | (T 6) => "MINUS"
  | (T 7) => "UNION"
  | (T 8) => "CROSSPRD"
  | (T 9) => "SUBSETEQ"
  | (T 10) => "SUBSET"
  | (T 11) => "EQUALOP"
  | (T 12) => "IMP"
  | (T 13) => "IFF"
  | (T 14) => "CONJ"
  | (T 15) => "DISJ"
  | (T 16) => "COLON"
  | (T 17) => "SEMICOLON"
  | (T 18) => "COMMA"
  | (T 19) => "STAR"
  | (T 20) => "LPAREN"
  | (T 21) => "RPAREN"
  | (T 22) => "LCURLY"
  | (T 23) => "RCURLY"
  | (T 24) => "LBRACE"
  | (T 25) => "RBRACE"
  | (T 26) => "ARROW"
  | (T 27) => "PIPE"
  | (T 28) => "ID"
  | (T 29) => "INT"
  | (T 30) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 30) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.spec spec, spec1left, spec1right)) :: rest671)) => let val  result = MlyValue.start ((*#line 96.15 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)spec(*#line 436.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, spec1left, spec1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.decsandtys decsandtys, decsandtys1left, decsandtys1right)) :: rest671)) => let val  result = MlyValue.spec ((*#line 98.21 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)decsandtys(*#line 440.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, decsandtys1left, decsandtys1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.decsandtys decsandtys, _, decsandtys1right)) :: _ :: ( _, ( MlyValue.reldec reldec, reldec1left, _)) :: rest671)) => let val  result = MlyValue.decsandtys ((*#line 101.18 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)case decsandtys of RelSpec.T ({reldecs,typespecs}) => 
                    RelSpec.T {reldecs = Vector.fromList (reldec ::
                      (Vector.toList reldecs)), typespecs = typespecs}(*#line 444.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, reldec1left, decsandtys1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.decsandtys decsandtys, _, decsandtys1right)) :: _ :: ( _, ( MlyValue.typespec typespec, typespec1left, _)) :: rest671)) => let val  result = MlyValue.decsandtys ((*#line 105.18 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)case decsandtys of RelSpec.T {reldecs,typespecs} => 
                    RelSpec.T {reldecs = reldecs, typespecs = 
                    Vector.fromList (typespec :: (Vector.toList typespecs))}(*#line 450.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, typespec1left, decsandtys1right), rest671)
end
|  ( 4, ( rest671)) => let val  result = MlyValue.decsandtys ((*#line 108.16 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)RelSpec.T {reldecs = Vector.fromList [],
                  typespecs = Vector.fromList []}(*#line 456.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 5, ( ( _, ( MlyValue.patmatchseq patmatchseq, _, patmatchseq1right)) :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, RELATION1left, _)) :: rest671)) => let val  result = MlyValue.reldec ((*#line 112.12 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)StructuralRelation.T {id=RelId.fromString ID,
                map = Vector.fromList patmatchseq}(*#line 461.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, RELATION1left, patmatchseq1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.rterm rterm, _, rterm1right)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, RELATION1left, _)) :: rest671)) => let val  result = MlyValue.reldec ((*#line 115.12 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)StructuralRelation.T{id=RelId.fromString ID,
                map = Vector.fromList [(defaultCons,NONE,rterm)]}(*#line 466.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, RELATION1left, rterm1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.patmatchseq patmatchseq, _, patmatchseq1right)) :: _ :: ( _, ( MlyValue.patmatch patmatch, patmatch1left, _)) :: rest671)) => let val  result = MlyValue.patmatchseq ((*#line 118.42 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)patmatch :: patmatchseq(*#line 471.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, patmatch1left, patmatchseq1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.patmatch patmatch, patmatch1left, patmatch1right)) :: rest671)) => let val  result = MlyValue.patmatchseq ((*#line 119.25 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)[patmatch](*#line 475.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, patmatch1left, patmatch1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.rterm rterm, _, rterm1right)) :: _ :: _ :: ( _, ( MlyValue.conpat conpat, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.patmatch ((*#line 122.16 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)case conpat of (c,vlop) => (c,vlop,rterm)(*#line 479.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, LPAREN1left, rterm1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.rterm rterm, _, rterm1right)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.patmatch ((*#line 123.30 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)(Con.fromString ID,NONE,rterm)(*#line 483.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, ID1left, rterm1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.conpat ((*#line 125.15 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Con.fromString ID, NONE(*#line 487.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, ID1left, ID1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.conargs conargs, _, conargs1right)) :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.conpat ((*#line 126.23 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Con.fromString ID, SOME conargs(*#line 491.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, ID1left, conargs1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.conargs ((*#line 128.15 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Vector.fromList [Var.fromString ID](*#line 495.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, ID1left, ID1right), rest671)
end
|  ( 14, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.idseq idseq, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.conargs ((*#line 129.32 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Vector.fromList idseq(*#line 499.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.idseq ((*#line 131.13 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)[Var.fromString ID](*#line 503.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, ID1left, ID1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.idseq idseq, _, idseq1right)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.idseq ((*#line 132.25 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)(Var.fromString ID)::idseq(*#line 507.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, ID1left, idseq1right), rest671)
end
|  ( 17, ( ( _, ( _, _, STAR1right)) :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.rterm ((*#line 134.18 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Star(RelId.fromString ID)(*#line 511.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, ID1left, STAR1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.rexpr rexpr, rexpr1left, rexpr1right)) :: rest671)) => let val  result = MlyValue.rterm ((*#line 135.15 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Expr(rexpr)(*#line 515.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, rexpr1left, rexpr1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.rexpr rexpr, _, rexpr1right)) :: _ :: ( _, ( MlyValue.ratom ratom, ratom1left, _)) :: rest671)) => let val  result = MlyValue.rexpr ((*#line 137.31 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)X(ratom,rexpr)(*#line 519.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, ratom1left, rexpr1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.rexpr rexpr, _, rexpr1right)) :: _ :: ( _, ( MlyValue.ratom ratom, ratom1left, _)) :: rest671)) => let val  result = MlyValue.rexpr ((*#line 138.28 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)U(ratom,rexpr)(*#line 523.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, ratom1left, rexpr1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.rexpr rexpr, _, rexpr1right)) :: _ :: ( _, ( MlyValue.ratom ratom, ratom1left, _)) :: rest671)) => let val  result = MlyValue.rexpr ((*#line 139.28 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)D(ratom,rexpr)(*#line 527.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, ratom1left, rexpr1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.ratom ratom, ratom1left, ratom1right)) :: rest671)) => let val  result = MlyValue.rexpr ((*#line 140.16 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)ratom(*#line 531.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, ratom1left, ratom1right), rest671)
end
|  ( 23, ( ( _, ( _, _, RCURLY1right)) :: _ :: _ :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 142.38 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)T(Vector.fromList [])(*#line 535.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 24, ( ( _, ( _, _, RCURLY1right)) :: _ :: ( _, ( MlyValue.elemseq elemseq, _, _)) :: _ :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 143.46 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)T(Vector.fromList elemseq)(*#line 539.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 25, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 144.30 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)R(RelId.fromString ID1, Var.fromString ID2)(*#line 543.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 26, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.rexpr rexpr, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 145.30 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)rexpr(*#line 547.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 11, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.elem elem, elem1left, elem1right)) :: rest671)) => let val  result = MlyValue.elemseq ((*#line 147.17 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)[elem](*#line 551.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, elem1left, elem1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.elemseq elemseq, _, elemseq1right)) :: _ :: ( _, ( MlyValue.elem elem, elem1left, _)) :: rest671)) => let val  result = MlyValue.elemseq ((*#line 148.31 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)elem::elemseq(*#line 555.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, elem1left, elemseq1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.INT INT, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 150.13 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Int(INT)(*#line 559.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, INT1left, INT1right), rest671)
end
|  ( 30, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 151.14 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Bool(true)(*#line 563.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 31, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 152.15 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Bool(false)(*#line 567.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 153.12 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Var(Var.fromString ID)(*#line 571.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, ID1left, ID1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.refty refty, _, refty1right)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, ASSUME1left, _)) :: rest671)) => let val  result = MlyValue.typespec ((*#line 155.35 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)TypeSpec.T (true,(Var.fromString ID),refty)(*#line 575.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, ASSUME1left, refty1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.refty refty, _, refty1right)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.typespec ((*#line 156.28 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)TypeSpec.T (false,(Var.fromString ID),refty)(*#line 579.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 14, ( result, ID1left, refty1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.tyatom tyatom, tyatom1left, tyatom1right)) :: rest671)) => let val  result = MlyValue.refty ((*#line 159.17 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)case tyatom of RefTy.Base _ => RefTy.alphaRename tyatom
                    | _ => tyatom(*#line 583.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, tyatom1left, tyatom1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.refty refty, _, refty1right)) :: _ :: ( _, ( MlyValue.vartyatom vartyatom, vartyatom1left, _)) :: rest671)) => let val  result = MlyValue.refty ((*#line 161.32 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)RefinementType.Arrow(vartyatom,refty)(*#line 588.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, vartyatom1left, refty1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.tyatom tyatom, tyatom1left, tyatom1right)) :: rest671)) => let val  result = MlyValue.vartyatom ((*#line 163.21 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)let open RefTy in case tyatom of 
                      Base (v,_,_) => (v,alphaRename tyatom)
                    | Tuple _ => (genVar (),tyatom)
                    | Arrow _ => (genVar (),tyatom)
                    end(*#line 592.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, tyatom1left, tyatom1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.vartyatom vartyatom, vartyatom1left, vartyatom1right)) :: rest671)) => let val  result = MlyValue.vartyatomseq ((*#line 169.27 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)[vartyatom](*#line 600.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, vartyatom1left, vartyatom1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.vartyatomseq vartyatomseq, _, vartyatomseq1right)) :: _ :: ( _, ( MlyValue.vartyatom vartyatom, vartyatom1left, _)) :: rest671)) => let val  result = MlyValue.vartyatomseq ((*#line 170.46 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)vartyatom ::
                  vartyatomseq(*#line 604.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, vartyatom1left, vartyatomseq1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.basety basety, basety1left, basety1right)) :: rest671)) => let val  result = MlyValue.tyatom ((*#line 173.18 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)basety(*#line 609.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, basety1left, basety1right), rest671)
end
|  ( 41, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.vartyatomseq vartyatomseq, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.tyatom ((*#line 174.38 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)case vartyatomseq of 
                  [(v,refty)] => refty
                | _ => RefTy.Tuple (Vector.fromList vartyatomseq)(*#line 613.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.basety ((*#line 179.14 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)RefinementType.Base ((Var.fromString ID), 
                TypeDesc.makeTunknown(), 
                Predicate.truee())(*#line 619.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 20, ( result, ID1left, ID1right), rest671)
end
|  ( 43, ( ( _, ( _, _, RCURLY1right)) :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.basety ((*#line 182.28 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)RefinementType.Base ((Var.fromString ID), 
                TypeDesc.makeTunknown(), 
                Predicate.truee())(*#line 625.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 20, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 44, ( ( _, ( _, _, RCURLY1right)) :: ( _, ( MlyValue.pred pred, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.basety ((*#line 185.38 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)RefinementType.Base ((Var.fromString ID), 
                TypeDesc.makeTunknown(), pred)(*#line 631.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 20, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.patom patom, patom1left, patom1right)) :: rest671)) => let val  result = MlyValue.pred ((*#line 191.16 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)patom(*#line 636.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 21, ( result, patom1left, patom1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.pred pred, _, pred1right)) :: _ :: ( _, ( MlyValue.patom patom, patom1left, _)) :: rest671)) => let val  result = MlyValue.pred ((*#line 192.25 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Predicate.If (patom,pred)(*#line 640.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 21, ( result, patom1left, pred1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.pred pred, _, pred1right)) :: _ :: ( _, ( MlyValue.patom patom, patom1left, _)) :: rest671)) => let val  result = MlyValue.pred ((*#line 193.25 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Predicate.Iff (patom,pred)(*#line 644.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 21, ( result, patom1left, pred1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.pred pred, _, pred1right)) :: _ :: ( _, ( MlyValue.patom patom, patom1left, _)) :: rest671)) => let val  result = MlyValue.pred ((*#line 194.26 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Predicate.Conj (patom,pred)(*#line 648.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 21, ( result, patom1left, pred1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.pred pred, _, pred1right)) :: _ :: ( _, ( MlyValue.patom patom, patom1left, _)) :: rest671)) => let val  result = MlyValue.pred ((*#line 195.26 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Predicate.Disj (patom,pred)(*#line 652.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 21, ( result, patom1left, pred1right), rest671)
end
|  ( 50, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  result = MlyValue.patom ((*#line 197.15 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Predicate.truee()(*#line 656.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 25, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.patom patom, _, patom1right)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = MlyValue.patom ((*#line 198.20 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Predicate.Not patom(*#line 660.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 25, ( result, NOT1left, patom1right), rest671)
end
|  ( 52, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.pred pred, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.patom ((*#line 199.29 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)pred(*#line 664.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 25, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.rpatom rpatom, rpatom1left, rpatom1right)) :: rest671)) => let val  result = MlyValue.patom ((*#line 200.17 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Predicate.Rel rpatom(*#line 668.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 25, ( result, rpatom1left, rpatom1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.bpatom bpatom, bpatom1left, bpatom1right)) :: rest671)) => let val  result = MlyValue.patom ((*#line 201.17 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Predicate.Base bpatom(*#line 672.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 25, ( result, bpatom1left, bpatom1right), rest671)
end
|  ( 55, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.bpatom ((*#line 203.39 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Predicate.BasePredicate.varEq 
                      (Var.fromString ID1, Var.fromString ID2)(*#line 676.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 24, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 56, ( ( _, ( _, _, RBRACE1right)) :: _ :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.bpatom ((*#line 205.41 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Predicate.BasePredicate.varBoolEq 
                      (Var.fromString ID, true)(*#line 681.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 24, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 57, ( ( _, ( _, _, RBRACE1right)) :: _ :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.bpatom ((*#line 207.42 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Predicate.BasePredicate.varBoolEq 
                      (Var.fromString ID, false)(*#line 686.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 24, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _, ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  result = MlyValue.rpatom ((*#line 210.31 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Predicate.RelPredicate.Eq(rexpr1,rexpr2)(*#line 691.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 23, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _, ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  result = MlyValue.rpatom ((*#line 211.30 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Predicate.RelPredicate.Sub(rexpr1,rexpr2)(*#line 695.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 23, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 60, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _, ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  result = MlyValue.rpatom ((*#line 212.32 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm"*)Predicate.RelPredicate.SubEq(rexpr1,rexpr2)(*#line 699.1 "/Users/gowtham/git/catalyst-fo/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 23, ( result, rexpr1left, rexpr2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Spec_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun RELATION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun ASSUME (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun UNION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun CROSSPRD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun SUBSETEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun SUBSET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun EQUALOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun IMP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun IFF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun CONJ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun DISJ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun STAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun LCURLY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun RCURLY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.VOID,p1,p2))
fun PIPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.ID i,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.INT i,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID,p1,p2))
end
end

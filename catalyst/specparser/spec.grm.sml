functor SpecLrValsFun (structure Token : TOKEN
                                structure Spec : SPEC_LANG) : Spec_LRVALS = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)(*  User declarations section for helper functions *)
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
val empty = fn _ => Vector.new0 ()
infixr 5 $

(*#line 26.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\002\000\082\000\005\000\081\000\021\000\080\000\023\000\063\000\
\\025\000\079\000\029\000\050\000\000\000\
\\001\000\002\000\119\000\003\000\118\000\022\000\117\000\029\000\116\000\
\\030\000\115\000\000\000\
\\001\000\002\000\119\000\003\000\118\000\029\000\116\000\030\000\115\000\000\000\
\\001\000\002\000\144\000\003\000\143\000\029\000\142\000\000\000\
\\001\000\010\000\104\000\011\000\103\000\012\000\102\000\000\000\
\\001\000\010\000\104\000\011\000\103\000\012\000\102\000\022\000\120\000\000\000\
\\001\000\012\000\035\000\021\000\034\000\029\000\033\000\000\000\
\\001\000\012\000\046\000\000\000\
\\001\000\012\000\091\000\000\000\
\\001\000\012\000\096\000\021\000\034\000\029\000\033\000\000\000\
\\001\000\012\000\133\000\000\000\
\\001\000\017\000\013\000\000\000\
\\001\000\017\000\030\000\000\000\
\\001\000\017\000\058\000\000\000\
\\001\000\018\000\197\000\019\000\197\000\022\000\197\000\027\000\199\000\000\000\
\\001\000\018\000\198\000\019\000\198\000\022\000\198\000\027\000\200\000\000\000\
\\001\000\018\000\011\000\000\000\
\\001\000\018\000\012\000\000\000\
\\001\000\020\000\069\000\000\000\
\\001\000\020\000\141\000\000\000\
\\001\000\021\000\018\000\029\000\017\000\000\000\
\\001\000\021\000\027\000\023\000\026\000\029\000\025\000\000\000\
\\001\000\021\000\034\000\029\000\033\000\000\000\
\\001\000\021\000\064\000\023\000\063\000\029\000\050\000\000\000\
\\001\000\021\000\088\000\000\000\
\\001\000\021\000\089\000\000\000\
\\001\000\022\000\028\000\000\000\
\\001\000\022\000\057\000\000\000\
\\001\000\022\000\065\000\000\000\
\\001\000\022\000\072\000\000\000\
\\001\000\022\000\120\000\000\000\
\\001\000\022\000\122\000\000\000\
\\001\000\022\000\134\000\000\000\
\\001\000\022\000\135\000\000\000\
\\001\000\022\000\136\000\000\000\
\\001\000\024\000\055\000\028\000\054\000\000\000\
\\001\000\024\000\101\000\000\000\
\\001\000\024\000\138\000\000\000\
\\001\000\024\000\145\000\000\000\
\\001\000\026\000\124\000\000\000\
\\001\000\026\000\147\000\000\000\
\\001\000\026\000\148\000\000\000\
\\001\000\026\000\149\000\000\000\
\\001\000\027\000\037\000\000\000\
\\001\000\029\000\015\000\000\000\
\\001\000\029\000\016\000\000\000\
\\001\000\029\000\036\000\000\000\
\\001\000\029\000\038\000\000\000\
\\001\000\029\000\042\000\000\000\
\\001\000\029\000\048\000\000\000\
\\001\000\029\000\050\000\000\000\
\\001\000\029\000\052\000\000\000\
\\001\000\029\000\093\000\000\000\
\\001\000\029\000\105\000\000\000\
\\001\000\029\000\112\000\000\000\
\\001\000\032\000\000\000\000\000\
\\151\000\000\000\
\\152\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\001\000\010\000\004\000\009\000\021\000\008\000\029\000\007\000\000\000\
\\156\000\000\000\
\\157\000\000\000\
\\158\000\000\000\
\\159\000\000\000\
\\160\000\029\000\052\000\000\000\
\\161\000\000\000\
\\162\000\019\000\029\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\028\000\045\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\021\000\068\000\029\000\067\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\019\000\123\000\000\000\
\\173\000\000\000\
\\174\000\025\000\071\000\000\000\
\\175\000\000\000\
\\176\000\025\000\071\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\000\000\
\\181\000\007\000\087\000\008\000\086\000\009\000\085\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\019\000\137\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\000\000\
\\190\000\000\000\
\\191\000\000\000\
\\192\000\000\000\
\\193\000\000\000\
\\194\000\000\000\
\\195\000\000\000\
\\196\000\000\000\
\\201\000\019\000\056\000\000\000\
\\202\000\000\000\
\\203\000\000\000\
\\204\000\000\000\
\\205\000\000\000\
\\206\000\000\000\
\\207\000\013\000\100\000\014\000\099\000\015\000\098\000\016\000\097\000\000\000\
\\208\000\000\000\
\\209\000\000\000\
\\210\000\000\000\
\\211\000\000\000\
\\212\000\000\000\
\\213\000\000\000\
\\214\000\000\000\
\\215\000\000\000\
\\216\000\000\000\
\\217\000\000\000\
\\218\000\000\000\
\\219\000\000\000\
\\220\000\000\000\
\\221\000\000\000\
\\222\000\000\000\
\"
val actionRowNumbers =
"\060\000\016\000\017\000\057\000\
\\056\000\011\000\044\000\045\000\
\\020\000\060\000\060\000\021\000\
\\026\000\067\000\012\000\006\000\
\\046\000\059\000\058\000\014\000\
\\043\000\100\000\098\000\105\000\
\\047\000\021\000\048\000\044\000\
\\021\000\070\000\061\000\007\000\
\\049\000\050\000\051\000\021\000\
\\035\000\102\000\027\000\104\000\
\\013\000\068\000\097\000\022\000\
\\023\000\028\000\073\000\018\000\
\\079\000\029\000\065\000\101\000\
\\000\000\106\000\021\000\015\000\
\\021\000\069\000\086\000\072\000\
\\024\000\025\000\023\000\008\000\
\\074\000\075\000\052\000\063\000\
\\080\000\050\000\009\000\066\000\
\\108\000\117\000\116\000\036\000\
\\004\000\053\000\000\000\000\000\
\\113\000\103\000\099\000\023\000\
\\023\000\023\000\054\000\001\000\
\\030\000\023\000\031\000\077\000\
\\039\000\062\000\050\000\000\000\
\\000\000\000\000\000\000\107\000\
\\023\000\023\000\023\000\010\000\
\\032\000\005\000\114\000\083\000\
\\084\000\085\000\033\000\034\000\
\\091\000\093\000\096\000\037\000\
\\095\000\094\000\090\000\071\000\
\\076\000\052\000\081\000\019\000\
\\112\000\111\000\110\000\109\000\
\\121\000\122\000\123\000\003\000\
\\115\000\089\000\038\000\002\000\
\\087\000\078\000\082\000\064\000\
\\040\000\041\000\042\000\088\000\
\\092\000\118\000\120\000\119\000\
\\055\000"
val gotoT =
"\
\\001\000\148\000\002\000\004\000\003\000\003\000\004\000\002\000\
\\020\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\012\000\000\000\
\\000\000\
\\000\000\
\\003\000\017\000\004\000\002\000\020\000\001\000\000\000\
\\003\000\018\000\004\000\002\000\020\000\001\000\000\000\
\\021\000\022\000\022\000\021\000\023\000\020\000\026\000\019\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\030\000\008\000\029\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\021\000\039\000\022\000\021\000\023\000\020\000\024\000\038\000\
\\025\000\037\000\026\000\019\000\000\000\
\\000\000\
\\006\000\041\000\000\000\
\\021\000\042\000\022\000\021\000\023\000\020\000\026\000\019\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\045\000\000\000\
\\013\000\047\000\000\000\
\\005\000\049\000\000\000\
\\021\000\051\000\022\000\021\000\023\000\020\000\026\000\019\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\057\000\008\000\029\000\000\000\
\\013\000\060\000\016\000\059\000\017\000\058\000\000\000\
\\000\000\
\\010\000\064\000\000\000\
\\000\000\
\\014\000\068\000\000\000\
\\000\000\
\\005\000\071\000\000\000\
\\000\000\
\\013\000\060\000\016\000\076\000\017\000\058\000\027\000\075\000\
\\029\000\074\000\030\000\073\000\031\000\072\000\000\000\
\\000\000\
\\021\000\039\000\022\000\021\000\023\000\020\000\024\000\081\000\
\\025\000\037\000\026\000\019\000\000\000\
\\000\000\
\\021\000\082\000\022\000\021\000\023\000\020\000\026\000\019\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\060\000\016\000\088\000\017\000\058\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\090\000\000\000\
\\000\000\
\\000\000\
\\013\000\092\000\000\000\
\\007\000\093\000\008\000\029\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\060\000\016\000\105\000\017\000\058\000\027\000\104\000\
\\029\000\074\000\030\000\073\000\031\000\072\000\000\000\
\\013\000\060\000\016\000\076\000\017\000\058\000\029\000\074\000\
\\030\000\073\000\031\000\106\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\060\000\016\000\107\000\017\000\058\000\000\000\
\\013\000\060\000\016\000\108\000\017\000\058\000\000\000\
\\013\000\060\000\016\000\109\000\017\000\058\000\000\000\
\\000\000\
\\018\000\112\000\019\000\111\000\000\000\
\\000\000\
\\013\000\060\000\016\000\119\000\017\000\058\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\123\000\000\000\
\\013\000\060\000\016\000\076\000\017\000\058\000\027\000\124\000\
\\029\000\074\000\030\000\073\000\031\000\072\000\000\000\
\\013\000\060\000\016\000\076\000\017\000\058\000\027\000\125\000\
\\029\000\074\000\030\000\073\000\031\000\072\000\000\000\
\\013\000\060\000\016\000\076\000\017\000\058\000\027\000\126\000\
\\029\000\074\000\030\000\073\000\031\000\072\000\000\000\
\\013\000\060\000\016\000\076\000\017\000\058\000\027\000\127\000\
\\029\000\074\000\030\000\073\000\031\000\072\000\000\000\
\\000\000\
\\013\000\060\000\016\000\128\000\017\000\058\000\000\000\
\\013\000\060\000\016\000\129\000\017\000\058\000\000\000\
\\013\000\060\000\016\000\130\000\017\000\058\000\000\000\
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
\\011\000\137\000\000\000\
\\014\000\138\000\000\000\
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
\\018\000\112\000\019\000\144\000\000\000\
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
val numstates = 149
val numrules = 72
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
datatype svalue = VOID | ntVOID of unit | INT of  (int) | ID of  (string) | patom of  (Predicate.t) | bpatom of  (Predicate.BasePredicate.t) | rpatom of  (Predicate.RelPredicate.t) | relpred of  (Predicate.RelPredicate.t) | pred of  (Predicate.t) | basety of  (RefinementType.t) | varty of  (Var.t*RefinementType.t) | vartyseq of  ( ( Var.t * RefinementType.t )  list) | vartyatom of  (Var.t*RefinementType.t) | reftyatom of  (RefinementType.t) | refty of  (RefinementType.t) | typespec of  (TypeSpec.t) | elemseq of  (elem list) | elem of  (elem) | ratom of  (expr) | rexpr of  (expr) | varid of  (Var.t) | instexprs of  (instexpr list) | instexpr of  (instexpr) | rterm of  (term) | idseq of  (Var.t list) | conargs of  (Var.t vector) | conpat of  (Con.t*Var.t vector option) | patmatch of  (Con.t*Var.t vector option*term) | patmatchseq of  ( ( Con.t * Var.t vector option * term )  list) | paramseq of  (RelId.t list) | params of  (RelId.t list) | reldec of  (StructuralRelation.t) | decsandtys of  (RelSpec.t) | spec of  (RelSpec.t) | start of  (RelSpec.t)
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
fn (T 31) => true | _ => false
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
  | (T 30) => "UINST"
  | (T 31) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 31) $$ (T 30) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.spec spec, spec1left, spec1right)) :: rest671)) => let val  result = MlyValue.start ((*#line 104.15 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)spec(*#line 488.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 0, ( result, spec1left, spec1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.decsandtys decsandtys, decsandtys1left, decsandtys1right)) :: rest671)) => let val  result = MlyValue.spec ((*#line 106.21 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)decsandtys(*#line 492.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 1, ( result, decsandtys1left, decsandtys1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.decsandtys decsandtys, _, decsandtys1right)) :: _ :: ( _, ( MlyValue.reldec reldec, reldec1left, _)) :: rest671)) => let val  result = MlyValue.decsandtys ((*#line 109.18 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)case decsandtys of RelSpec.T ({reldecs,typespecs}) => 
                    RelSpec.T {reldecs = Vector.fromList (reldec ::
                      (Vector.toList reldecs)), typespecs = typespecs}(*#line 496.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, reldec1left, decsandtys1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.decsandtys decsandtys, _, decsandtys1right)) :: _ :: ( _, ( MlyValue.typespec typespec, typespec1left, _)) :: rest671)) => let val  result = MlyValue.decsandtys ((*#line 113.18 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)case decsandtys of RelSpec.T {reldecs,typespecs} => 
                    RelSpec.T {reldecs = reldecs, typespecs = 
                    Vector.fromList (typespec :: (Vector.toList typespecs))}(*#line 502.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, typespec1left, decsandtys1right), rest671)
end
|  ( 4, ( rest671)) => let val  result = MlyValue.decsandtys ((*#line 116.16 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)RelSpec.T {reldecs = Vector.fromList [],
                  typespecs = Vector.fromList []}(*#line 508.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 5, ( ( _, ( MlyValue.patmatchseq patmatchseq, _, patmatchseq1right)) :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, RELATION1left, _)) :: rest671)) => let val  result = MlyValue.reldec ((*#line 120.12 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)StructuralRelation.T {id=RelId.fromString ID,
                params = empty (),
                map = Vector.fromList patmatchseq}(*#line 513.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, RELATION1left, patmatchseq1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.patmatchseq patmatchseq, _, patmatchseq1right)) :: _ :: ( _, ( MlyValue.params params, _, _)) :: ( _, ( MlyValue.ID ID, _, _)) :: _ :: ( _, ( _, RELATION1left, _)) :: rest671)) => let val  result = MlyValue.reldec ((*#line 124.12 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)StructuralRelation.T {id=RelId.fromString ID,
                params = Vector.fromList params,
                map = Vector.fromList patmatchseq}(*#line 519.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, RELATION1left, patmatchseq1right), rest671)
end
|  ( 7, ( ( _, ( _, _, STAR1right)) :: ( _, ( MlyValue.instexpr instexpr, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, RELATION1left, _)) :: rest671)) => let val  result = MlyValue.reldec ((*#line 128.12 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)StructuralRelation.T{id=RelId.fromString ID,
                params = empty (),
                map = Vector.fromList [(defaultCons,NONE,
                  Star instexpr)]}(*#line 525.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, RELATION1left, STAR1right), rest671)
end
|  ( 8, ( ( _, ( _, _, STAR1right)) :: ( _, ( MlyValue.instexpr instexpr, _, _)) :: _ :: _ :: ( _, ( MlyValue.params params, _, _)) :: ( _, ( MlyValue.ID ID, _, _)) :: _ :: ( _, ( _, RELATION1left, _)) :: rest671)) => let val  result = MlyValue.reldec ((*#line 133.12 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)StructuralRelation.T{id=RelId.fromString ID,
                params = Vector.fromList params,
                map = Vector.fromList [(defaultCons,NONE,
                  Star instexpr)]}(*#line 532.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 3, ( result, RELATION1left, STAR1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.params ((*#line 138.14 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)[RelId.fromString ID](*#line 539.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.params params, _, params1right)) :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.params ((*#line 139.21 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)(RelId.fromString ID)::params(*#line 543.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 4, ( result, ID1left, params1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.paramseq ((*#line 141.16 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)[RelId.fromString ID](*#line 547.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, ID1left, ID1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.paramseq paramseq, _, paramseq1right)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.paramseq ((*#line 142.29 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)(RelId.fromString ID)::paramseq(*#line 551.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 5, ( result, ID1left, paramseq1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.patmatchseq patmatchseq, _, patmatchseq1right)) :: _ :: ( _, ( MlyValue.patmatch patmatch, patmatch1left, _)) :: rest671)) => let val  result = MlyValue.patmatchseq ((*#line 144.42 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)patmatch :: patmatchseq(*#line 555.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, patmatch1left, patmatchseq1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.patmatch patmatch, patmatch1left, patmatch1right)) :: rest671)) => let val  result = MlyValue.patmatchseq ((*#line 145.25 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)[patmatch](*#line 559.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 6, ( result, patmatch1left, patmatch1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.rexpr rexpr, _, rexpr1right)) :: _ :: _ :: ( _, ( MlyValue.conpat conpat, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.patmatch ((*#line 148.16 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)case conpat of (c,vlop) => (c, vlop, Expr rexpr)(*#line 563.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, LPAREN1left, rexpr1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.rexpr rexpr, _, rexpr1right)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.patmatch ((*#line 149.30 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)(Con.fromString ID, NONE, Expr rexpr)(*#line 567.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 7, ( result, ID1left, rexpr1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.conpat ((*#line 151.15 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Con.fromString ID, NONE(*#line 571.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, ID1left, ID1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.conargs conargs, _, conargs1right)) :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.conpat ((*#line 152.23 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Con.fromString ID, SOME conargs(*#line 575.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 8, ( result, ID1left, conargs1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.conargs ((*#line 154.15 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Vector.fromList [Var.fromString ID](*#line 579.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, ID1left, ID1right), rest671)
end
|  ( 20, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.idseq idseq, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.conargs ((*#line 155.32 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Vector.fromList idseq(*#line 583.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 9, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.idseq ((*#line 157.13 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)[Var.fromString ID](*#line 587.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, ID1left, ID1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.idseq idseq, _, idseq1right)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.idseq ((*#line 158.25 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)(Var.fromString ID)::idseq(*#line 591.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 10, ( result, ID1left, idseq1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.instexpr ((*#line 160.16 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)RInst { sargs = empty (), 
                targs = empty(), args = empty (), 
                rel = RelId.fromString ID}(*#line 595.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, ID1left, ID1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.instexprs instexprs, _, instexprs1right)) :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.instexpr ((*#line 163.26 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)RInst {
                sargs = empty (), targs = empty(),
                args = Vector.fromList instexprs, 
                rel = RelId.fromString ID}(*#line 601.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 12, ( result, ID1left, instexprs1right), rest671)
end
|  ( 25, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.instexpr instexpr, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.instexprs ((*#line 168.37 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)[instexpr](*#line 608.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.instexprs instexprs, _, instexprs1right)) :: _ :: ( _, ( MlyValue.instexpr instexpr, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.instexprs ((*#line 169.47 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)instexpr :: instexprs(*#line 612.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 13, ( result, LBRACE1left, instexprs1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.rexpr rexpr, _, rexpr1right)) :: _ :: ( _, ( MlyValue.ratom ratom, ratom1left, _)) :: rest671)) => let val  result = MlyValue.rexpr ((*#line 171.31 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)X(ratom,rexpr)(*#line 616.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, ratom1left, rexpr1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.rexpr rexpr, _, rexpr1right)) :: _ :: ( _, ( MlyValue.ratom ratom, ratom1left, _)) :: rest671)) => let val  result = MlyValue.rexpr ((*#line 172.28 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)U(ratom,rexpr)(*#line 620.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, ratom1left, rexpr1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.rexpr rexpr, _, rexpr1right)) :: _ :: ( _, ( MlyValue.ratom ratom, ratom1left, _)) :: rest671)) => let val  result = MlyValue.rexpr ((*#line 173.28 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)D(ratom,rexpr)(*#line 624.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, ratom1left, rexpr1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.ratom ratom, ratom1left, ratom1right)) :: rest671)) => let val  result = MlyValue.rexpr ((*#line 174.16 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)ratom(*#line 628.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 15, ( result, ratom1left, ratom1right), rest671)
end
|  ( 31, ( ( _, ( _, _, RCURLY1right)) :: _ :: _ :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 176.38 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)T(Vector.fromList [])(*#line 632.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 16, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 32, ( ( _, ( _, _, RCURLY1right)) :: _ :: ( _, ( MlyValue.elemseq elemseq, _, _)) :: _ :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 177.46 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)T(Vector.fromList elemseq)(*#line 636.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 16, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 33, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ID ID, _, _)) :: _ :: ( _, ( MlyValue.instexpr instexpr, instexpr1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 178.36 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)R (instexpr, Var.fromString ID)(*#line 640.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 16, ( result, instexpr1left, RPAREN1right), rest671)
end
|  ( 34, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.rexpr rexpr, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.ratom ((*#line 179.30 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)rexpr(*#line 644.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 16, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.elem elem, elem1left, elem1right)) :: rest671)) => let val  result = MlyValue.elemseq ((*#line 181.17 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)[elem](*#line 648.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, elem1left, elem1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.elemseq elemseq, _, elemseq1right)) :: _ :: ( _, ( MlyValue.elem elem, elem1left, _)) :: rest671)) => let val  result = MlyValue.elemseq ((*#line 182.31 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)elem::elemseq(*#line 652.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 18, ( result, elem1left, elemseq1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.INT INT, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 184.13 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Int(INT)(*#line 656.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, INT1left, INT1right), rest671)
end
|  ( 38, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 185.14 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Bool(true)(*#line 660.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 39, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 186.15 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Bool(false)(*#line 664.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.elem ((*#line 187.12 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Var(Var.fromString ID)(*#line 668.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 17, ( result, ID1left, ID1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.refty refty, _, refty1right)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, ASSUME1left, _)) :: rest671)) => let val  result = MlyValue.typespec ((*#line 189.35 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)TypeSpec.T {isAssume = true,
                                              name = (Var.fromString ID),
                                              params = empty (),
                                              refty = refty}(*#line 672.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, ASSUME1left, refty1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.refty refty, _, refty1right)) :: _ :: ( _, ( MlyValue.ID ID, ID1left, _)) :: rest671)) => let val  result = MlyValue.typespec ((*#line 193.28 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)TypeSpec.T {isAssume = false,
                                       name = (Var.fromString ID),
                                       params = empty (),
                                       refty = refty}(*#line 679.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, ID1left, refty1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.refty refty, _, refty1right)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: _ :: ( _, ( MlyValue.paramseq paramseq, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.typespec ((*#line 197.51 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)
                    TypeSpec.T {isAssume = false,
                                name = Var.fromString ID,
                                params = Vector.fromList paramseq, 
                                refty = refty}(*#line 686.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 19, ( result, LPAREN1left, refty1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.reftyatom reftyatom, reftyatom1left, reftyatom1right)) :: rest671)) => let val  result = MlyValue.refty ((*#line 203.20 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)reftyatom(*#line 694.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 20, ( result, reftyatom1left, reftyatom1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.refty refty, _, refty1right)) :: _ :: ( _, ( MlyValue.vartyatom vartyatom, vartyatom1left, _)) :: rest671)) => let val  result = MlyValue.refty ((*#line 204.32 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)RefTy.Arrow (vartyatom, refty)(*#line 698.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 20, ( result, vartyatom1left, refty1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.basety basety, basety1left, basety1right)) :: rest671)) => let val  result = MlyValue.reftyatom ((*#line 206.21 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)basety(*#line 702.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 21, ( result, basety1left, basety1right), rest671)
end
|  ( 47, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.vartyseq vartyseq, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.reftyatom ((*#line 207.38 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)case vartyseq of
                          [(v, refty as RefTy.Base _)] => 
                              RefTy.alphaRenameToVar refty v
                        | [(v,refty)] => refty
                        | _ => RefTy.Tuple (Vector.fromList vartyseq)(*#line 706.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 21, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.basety basety, basety1left, basety1right)) :: rest671)) => let val  result = MlyValue.vartyatom ((*#line 213.21 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)case basety of 
                      RefTy.Base (v,_,_) => (v,RefTy.alphaRename basety)
                    | _ => Error.bug "Impossible case of basety"(*#line 714.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 22, ( result, basety1left, basety1right), rest671)
end
|  ( 49, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.vartyseq vartyseq, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.vartyatom ((*#line 216.38 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)case vartyseq of
                          [x] => x 
                        | _ => (genVar (), RefTy.Tuple 
                            (Vector.fromList vartyseq))(*#line 720.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 22, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.varty varty, varty1left, varty1right)) :: rest671)) => let val  result = MlyValue.vartyseq ((*#line 221.19 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)[varty](*#line 727.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 23, ( result, varty1left, varty1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.vartyseq vartyseq, _, vartyseq1right)) :: _ :: ( _, ( MlyValue.varty varty, varty1left, _)) :: rest671)) => let val  result = MlyValue.vartyseq ((*#line 222.34 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)varty :: vartyseq(*#line 731.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 23, ( result, varty1left, vartyseq1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.refty refty, refty1left, refty1right)) :: rest671)) => let val  result = MlyValue.varty ((*#line 224.16 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)let open RefTy in case refty of 
                          Base (v,_,_) => (v,alphaRename refty)
                        | Tuple _ => (genVar (),refty)
                        | Arrow _ => (genVar (),refty)
                        end(*#line 735.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 24, ( result, refty1left, refty1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.ID ID, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.basety ((*#line 233.14 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)RefinementType.Base ((Var.fromString ID), 
                TypeDesc.makeTunknown(), 
                Predicate.truee())(*#line 743.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 25, ( result, ID1left, ID1right), rest671)
end
|  ( 54, ( ( _, ( _, _, RCURLY1right)) :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.basety ((*#line 236.28 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)RefinementType.Base ((Var.fromString ID), 
                TypeDesc.makeTunknown(), 
                Predicate.truee())(*#line 749.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 25, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 55, ( ( _, ( _, _, RCURLY1right)) :: ( _, ( MlyValue.pred pred, _, _)) :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, LCURLY1left, _)) :: rest671)) => let val  result = MlyValue.basety ((*#line 239.38 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)RefinementType.Base ((Var.fromString ID), 
                TypeDesc.makeTunknown(), pred)(*#line 755.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 25, ( result, LCURLY1left, RCURLY1right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.patom patom, patom1left, patom1right)) :: rest671)) => let val  result = MlyValue.pred ((*#line 244.16 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)patom(*#line 760.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 26, ( result, patom1left, patom1right), rest671)
end
|  ( 57, ( ( _, ( MlyValue.pred pred, _, pred1right)) :: _ :: ( _, ( MlyValue.patom patom, patom1left, _)) :: rest671)) => let val  result = MlyValue.pred ((*#line 245.25 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Predicate.If (patom,pred)(*#line 764.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 26, ( result, patom1left, pred1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.pred pred, _, pred1right)) :: _ :: ( _, ( MlyValue.patom patom, patom1left, _)) :: rest671)) => let val  result = MlyValue.pred ((*#line 246.25 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Predicate.Iff (patom,pred)(*#line 768.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 26, ( result, patom1left, pred1right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.pred pred, _, pred1right)) :: _ :: ( _, ( MlyValue.patom patom, patom1left, _)) :: rest671)) => let val  result = MlyValue.pred ((*#line 247.26 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Predicate.Conj (patom,pred)(*#line 772.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 26, ( result, patom1left, pred1right), rest671)
end
|  ( 60, ( ( _, ( MlyValue.pred pred, _, pred1right)) :: _ :: ( _, ( MlyValue.patom patom, patom1left, _)) :: rest671)) => let val  result = MlyValue.pred ((*#line 248.26 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Predicate.Disj (patom,pred)(*#line 776.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 26, ( result, patom1left, pred1right), rest671)
end
|  ( 61, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  result = MlyValue.patom ((*#line 250.15 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Predicate.truee()(*#line 780.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 30, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.patom patom, _, patom1right)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = MlyValue.patom ((*#line 251.20 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Predicate.Not patom(*#line 784.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 30, ( result, NOT1left, patom1right), rest671)
end
|  ( 63, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.pred pred, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.patom ((*#line 252.29 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)pred(*#line 788.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 30, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 64, ( ( _, ( MlyValue.rpatom rpatom, rpatom1left, rpatom1right)) :: rest671)) => let val  result = MlyValue.patom ((*#line 253.17 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Predicate.Rel rpatom(*#line 792.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 30, ( result, rpatom1left, rpatom1right), rest671)
end
|  ( 65, ( ( _, ( MlyValue.bpatom bpatom, bpatom1left, bpatom1right)) :: rest671)) => let val  result = MlyValue.patom ((*#line 254.17 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Predicate.Base bpatom(*#line 796.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 30, ( result, bpatom1left, bpatom1right), rest671)
end
|  ( 66, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.bpatom ((*#line 256.39 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Predicate.BasePredicate.varEq 
                      (Var.fromString ID1, Var.fromString ID2)(*#line 800.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 29, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 67, ( ( _, ( _, _, RBRACE1right)) :: _ :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.bpatom ((*#line 258.41 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Predicate.BasePredicate.varBoolEq 
                      (Var.fromString ID, true)(*#line 805.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 29, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 68, ( ( _, ( _, _, RBRACE1right)) :: _ :: _ :: ( _, ( MlyValue.ID ID, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.bpatom ((*#line 260.42 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Predicate.BasePredicate.varBoolEq 
                      (Var.fromString ID, false)(*#line 810.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 29, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 69, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _, ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  result = MlyValue.rpatom ((*#line 263.31 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Predicate.RelPredicate.Eq(rexpr1,rexpr2)(*#line 815.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 28, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 70, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _, ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  result = MlyValue.rpatom ((*#line 264.30 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Predicate.RelPredicate.Sub(rexpr1,rexpr2)(*#line 819.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 28, ( result, rexpr1left, rexpr2right), rest671)
end
|  ( 71, ( ( _, ( MlyValue.rexpr rexpr2, _, rexpr2right)) :: _ :: ( _, ( MlyValue.rexpr rexpr1, rexpr1left, _)) :: rest671)) => let val  result = MlyValue.rpatom ((*#line 265.32 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm"*)Predicate.RelPredicate.SubEq(rexpr1,rexpr2)(*#line 823.1 "/Users/gowtham/git/tycon/catalyst/catalyst/specparser/spec.grm.sml"*)
)
 in ( LrTable.NT 28, ( result, rexpr1left, rexpr2right), rest671)
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
fun UINST (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.VOID,p1,p2))
end
end

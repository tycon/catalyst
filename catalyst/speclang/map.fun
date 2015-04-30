functor ApplicativeMap (S : APPLICATIVE_MAP_STRUCTS) : APPLICATIVE_MAP = 
struct
  open S
  structure Key = Key
  structure Val = Value
  structure L = Layout
  exception KeyNotFound of Key.t
  type t = (Key.t * Val.t) list
  val empty = []
  fun mem map k = List.exists (map, fn (k',v) => Key.equal (k,k'))
  fun find map k = 
    let
      val valop = List.peek (map, fn (k',v) => Key.equal (k,k'))
    in
      case valop of NONE => raise (KeyNotFound k)
        | SOME (k',v) => v
    end
  fun add map k v = (k,v)::map
  fun remove map k = List.remove (map, fn (k',_) => Key.equal(k,k'))
  fun toVector map = Vector.fromList map
  fun toList map = map
  fun layout map = L.align (List.map (map, fn (k,v) =>
    L.seq [Key.layout k, L.str " :-> ", Val.layout v]))
end

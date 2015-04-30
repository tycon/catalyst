signature KEY = 
sig
  type t
  val equal : t*t -> bool
  val layout: t -> Layout.t
end
signature VALUE =
sig
  type t
  val layout: t -> Layout.t
end
signature APPLICATIVE_MAP_STRUCTS =
sig
  structure Key : KEY
  structure Value : VALUE
end
signature APPLICATIVE_MAP = 
sig
  include APPLICATIVE_MAP_STRUCTS
  exception KeyNotFound of Key.t
  type t
  val empty : t
  val mem : t -> Key.t -> bool
  val find : t -> Key.t -> Value.t
  val add : t -> Key.t -> Value.t -> t
  val remove : t -> Key.t -> t
  val toVector : t -> (Key.t * Value.t) vector
  val toList : t -> (Key.t * Value.t) list
  val layout : t -> Layout.t
end

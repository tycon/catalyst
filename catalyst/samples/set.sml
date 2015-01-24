structure Set : 
sig
  type t

  val nullSet : unit -> t
  val singleton : int list -> t
  val union : t list -> t
  val crossprd : t*t -> t
  val subEq : t*t -> bool
end =
struct
  open List

  type t = int list list

  fun nullSet _ = [[]]

  fun singleton x = [x]

  val union = concat

  fun crossprd (l1,l2) = foldr (fn (x,acc) => 
    (map (fn y => x@y) l2)@acc) [] l1

  fun listEq ([],[]) = true
    | listEq (x::xs,y::ys) = (x=y) andalso (listEq (xs,ys))
    | listEq _ = false

  fun subEq (l1,l2) = foldr (fn (x,acc) => acc andalso 
    (exists (fn y => listEq (y,x)) l2)) true l1

end

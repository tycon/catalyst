exception Catalyst;

fun concat l1 l2 = case l1 of
    [] => l2
  | x::xs => x::(concat xs l2)

fun rev l = case l of
    [] => []
  | x::xs => concat (rev xs) [x] 

fun snoc n l = case l of
    [] => [n]
  | x::xs => x::(snoc n xs)

(*
 * ('R) filter : l -> (f: x -> {v | v=true <=> 'R(x) = {()}})
      -> {v | (Rmem RId)(v) = (Rmem 'R)(l)}
 *)
fun filter l f = case l of
    [] => []
  | x::xs => 
    let
      val rest = filter xs f
    in
      if f x then rest else x::rest
    end 
(* 'R can be inst with (RNeq k)*)

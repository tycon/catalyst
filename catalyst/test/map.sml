exception Catalyst;

fun map l f = case l of 
    [] => []
  | x::xs => (f x)::(map xs f)

fun ('a,'b) foldl (l:'a list) (b:'b) (f: 'a -> 'b -> 'b)  = 
  case l of
    [] => b
  | x::xs => foldl xs (f x b) f

(*
fun concat ll = case ll of
  []  => []
| l::ls => append l (concat ls)
*)

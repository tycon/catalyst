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

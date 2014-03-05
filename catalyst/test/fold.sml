exception Catalyst;

fun ('a,'b) foldl (l:'a list) (b:'b) (f: 'a -> 'b -> 'b)  = 
  case l of
    [] => b
  | x::xs => foldl xs (f x b) f

fun ('a,'b) foldr (l:'a list) (b:'b) (f: 'a -> 'b -> 'b)  = 
  case l of
    [] => b
  | x::xs => f x (foldr xs b f)


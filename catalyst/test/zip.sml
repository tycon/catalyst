exception Catalyst;

datatype ('a,'b) pair = Pair of 'a * 'b

fun zip l1 l2 = case (l1,l2) of
    ([],[]) => []
  | (x1::xs1,x2::xs2) => (Pair (x1,x2))::(zip xs1 xs2)
  | _ => raise Catalyst;

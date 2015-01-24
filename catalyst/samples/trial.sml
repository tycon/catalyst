exception CatalystMeta;

open Set

val log = TextIO.print

val tabulate = List.tabulate

fun map2 ([],[],f) = []
  | map2 (x::xs,y::ys,f) = (f(x,y))::(map2 (xs,ys,f))

val rand = Random.rand (1,9999)

fun randInt () = Random.randInt rand

type int32 = int

val rec catalystGenList =  (fn x_5: int32 => case x_5 of (n: int32) => case (<= (n, 0x0)) of true => [] | false => (::[int32] ((randInt (unit) ()), (catalystGenList (- (n, 0x1))))))


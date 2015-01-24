exception CatalystMeta;

open Set

val log = TextIO.print

val tabulate = List.tabulate

fun map2 ([],[],f) = []
  | map2 (x::xs,y::ys,f) = (f(x,y))::(map2 (xs,ys,f))

val rand = Random.rand (1,9999)

fun randInt () = Random.randInt rand

fun catalystGenList n = if (n<=0) then [] else
  (randInt())::(catalystGenList (n-1))

fun Rhd [] = nullSet ()
  | Rhd (x::xs) = singleton [x]

fun Rmem [] = nullSet ()
  | Rmem (x::xs) = union [singleton [x],Rmem(xs)]
 
fun Rob [] = nullSet ()
  | Rob (x::xs) = crossprd (singleton [x],Rmem(xs))

fun Robs [] = nullSet ()
  | Robs (x::xs) = union [crossprd(singleton [x],Rmem(xs)), Robs(xs)]

val rhdSels = ref (tabulate (4, fn _ => true))
val rhdCands = [fn (x,y) => Rhd(x), fn (x,y) => Rhd(y), 
                fn (x,y) => Rmem(x), fn (x,y) => Rmem(y)]
val rmemSels = ref (tabulate (4, fn _ => true))
val rmemCands = [fn (x,y) => Rhd(x), fn (x,y) => Rhd(y), 
                fn (x,y) => Rmem(x), fn (x,y) => Rmem(y)]
val robSels = ref (tabulate (20, fn _ => true))
val robsSels = ref (tabulate (20, fn _ => true))

fun instrConcat l1 l2 = 
  let
    val l = concat l1 l2
    val (rhdl, rmeml, robl, robsl) = 
      (Rhd l, Rmem l, Rob l, Robs l)
    val _ = rhdSels := map2 (!rhdSels, rhdCands, 
      fn (sel,cand) => sel andalso subEq (cand (l1,l2), rhdl))
    val _ = rmemSels := map2 (!rmemSels, rmemCands, 
      fn (sel,cand) => sel andalso subEq (cand (l1,l2), rmeml))
  in 
    l
  end
  and concat l1 l2 = case l1 of
      [] => l2
    | x::xs => x::(instrConcat xs l2)

fun main _ = 
  let
    val l1 = catalystGenList 2
    val l2 = catalystGenList 2
    val _ = instrConcat l1 l2
    val _ = List.map (fn sel => if sel then log "1" 
        else log "0") (!rhdSels)
    val _ = print "\n"
    val _ = List.map (fn sel => if sel then log "1" 
        else log "0") (!rmemSels)
    val _ = print "\n"
  in
    ()
  end

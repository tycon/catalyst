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

fun Rhd l = case l of
    x::xs => singleton [x]
  | [] => singleton []

fun Rmem l = case l of
    x::xs => union [singleton [x],Rmem(xs)]
  | [] => singleton []

fun Rob l = case l of
    x::xs => crossprd (singleton [x],Rmem(xs))
  | [] => singleton []

fun Robs l = case l of
    x::xs => union [crossprd (singleton [x],Rmem(xs)),Robs(xs)]
  | [] => singleton []

val rhdSels = ref (tabulate (4, fn _ => true))
val rmemSels = ref (tabulate (4, fn _ => true))
val robSels = ref (tabulate (20, fn _ => true))
val robsSels = ref (tabulate (20, fn _ => true))val rhdCand = [fn (l1,l2) => Rhd(l2),fn (l1,l2) => Rhd(l1),fn (l1,l2) => Rmem(l2),fn (l1,l2) => Rmem(l1)]
val rmemCand = [fn (l1,l2) => Rhd(l2),fn (l1,l2) => Rhd(l1),fn (l1,l2) => Rmem(l2),fn (l1,l2) => Rmem(l1)]
val robCand = [fn (l1,l2) => Rob(l2),fn (l1,l2) => Rob(l1),fn (l1,l2) => Robs(l2),fn (l1,l2) => Robs(l1),fn (l1,l2) => crossprd (Rhd(l2),Rhd(l2)),fn (l1,l2) => crossprd (Rhd(l2),Rhd(l1)),fn (l1,l2) => crossprd (Rhd(l1),Rhd(l2)),fn (l1,l2) => crossprd (Rhd(l1),Rhd(l1)),fn (l1,l2) => crossprd (Rhd(l2),Rmem(l2)),fn (l1,l2) => crossprd (Rhd(l2),Rmem(l1)),fn (l1,l2) => crossprd (Rhd(l1),Rmem(l2)),fn (l1,l2) => crossprd (Rhd(l1),Rmem(l1)),fn (l1,l2) => crossprd (Rmem(l2),Rhd(l2)),fn (l1,l2) => crossprd (Rmem(l2),Rhd(l1)),fn (l1,l2) => crossprd (Rmem(l1),Rhd(l2)),fn (l1,l2) => crossprd (Rmem(l1),Rhd(l1)),fn (l1,l2) => crossprd (Rmem(l2),Rmem(l2)),fn (l1,l2) => crossprd (Rmem(l2),Rmem(l1)),fn (l1,l2) => crossprd (Rmem(l1),Rmem(l2)),fn (l1,l2) => crossprd (Rmem(l1),Rmem(l1))]
val robsCand = [fn (l1,l2) => Rob(l2),fn (l1,l2) => Rob(l1),fn (l1,l2) => Robs(l2),fn (l1,l2) => Robs(l1),fn (l1,l2) => crossprd (Rhd(l2),Rhd(l2)),fn (l1,l2) => crossprd (Rhd(l2),Rhd(l1)),fn (l1,l2) => crossprd (Rhd(l1),Rhd(l2)),fn (l1,l2) => crossprd (Rhd(l1),Rhd(l1)),fn (l1,l2) => crossprd (Rhd(l2),Rmem(l2)),fn (l1,l2) => crossprd (Rhd(l2),Rmem(l1)),fn (l1,l2) => crossprd (Rhd(l1),Rmem(l2)),fn (l1,l2) => crossprd (Rhd(l1),Rmem(l1)),fn (l1,l2) => crossprd (Rmem(l2),Rhd(l2)),fn (l1,l2) => crossprd (Rmem(l2),Rhd(l1)),fn (l1,l2) => crossprd (Rmem(l1),Rhd(l2)),fn (l1,l2) => crossprd (Rmem(l1),Rhd(l1)),fn (l1,l2) => crossprd (Rmem(l2),Rmem(l2)),fn (l1,l2) => crossprd (Rmem(l2),Rmem(l1)),fn (l1,l2) => crossprd (Rmem(l1),Rmem(l2)),fn (l1,l2) => crossprd (Rmem(l1),Rmem(l1))]

fun instrConcat l1 l2 = 
  let
    val l_ = concat l1 l2
    val rhdl_ = Rhd(l_)
    val rmeml_ = Rmem(l_)
    val robl_ = Rob(l_)
    val robsl_ = Robs(l_)
    val _ = rhdSels := map2 (!rhdSels, rhdCand, fn (s,c) => s andalso subEq (c(l1,l2), rhdl_))
    val _ = rmemSels := map2 (!rmemSels, rmemCand, fn (s,c) => s andalso subEq (c(l1,l2), rmeml_))
    val _ = robSels := map2 (!robSels, robCand, fn (s,c) => s andalso subEq (c(l1,l2), robl_))
    val _ = robsSels := map2 (!robsSels, robsCand, fn (s,c) => s andalso subEq (c(l1,l2), robsl_))
  in
    l_
  end

and concat x_0 x_1 = 
case (x_0, x_1) of (l1, l2) => case l1 of [] => l2 | x::xs => x::((concat xs) l2)

fun main _ = 
  let
    val l1 = catalystGenList 2
    val l2 = catalystGenList 2
    val _ = instrConcat l1 l2
    val _ = List.map (fn sel => if sel then log "1" else log "0") (!rhdSels)
    val _ = print "\n"
    val _ = List.map (fn sel => if sel then log "1" else log "0") (!rmemSels)
    val _ = print "\n"
    val _ = List.map (fn sel => if sel then log "1" else log "0") (!robSels)
    val _ = print "\n"
    val _ = List.map (fn sel => if sel then log "1" else log "0") (!robsSels)
    val _ = print "\n"
  in
    ()
  end

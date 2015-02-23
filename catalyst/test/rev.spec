relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;
relation Roa (cons(x,xs)) = Rmem(xs) X {(x)} | nil = {()};
relation Roas = Roa*;
assume catalystGenList : n -> {l | true};
concat : l1 -> l2 -> { l | ??};
(*
assume concat : l1 -> l2 -> { l | Rmem(l) = Rmem(l1) U Rmem(l2) /\
                                  Robs(l) = Robs(l1) U Robs(l2) U (Rmem(l1) X Rmem(l2))};
rev : l -> {l' | ??};
*)

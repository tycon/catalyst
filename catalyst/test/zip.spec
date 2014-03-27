relation (Rhd R)(cons(x,xs)) = R(x) | nil = {()};
relation (Rmem R) = Rhd[R]*;
relation (Rob R)(cons(x,xs)) = R(x) X Rmem[R](xs) | nil = {()};
relation (Robs R) = Rob[R]*;
relation Rfst(Pair(x,y)) = {(x)};
relation Rsnd(Pair(x,y)) = {(y)};
primitive relation RId = \x. {(x)};

zip : l1 -> l2 -> {v | Rmem[Rfst](v) = Rmem[RId](l1)};

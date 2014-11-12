relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Roa (cons(x,xs)) = Rmem(xs) X {(x)} | nil = {()};
relation Robs = Rob*;
relation Roas = Roa*;

concat : l1 -> l2 -> { l | ??};
rev : l1 -> { l2 | ??};

snoc : x -> l -> {v | not (Rmem(v) = {()})};

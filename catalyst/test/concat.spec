relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Robs = Rob*;

concat : l1 -> l2 -> { l | Rmem(l) = Rmem(l1) U Rmem(l2)};

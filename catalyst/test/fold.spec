relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rob (cons(x,xs)) = {(x)} X Rmem(xs) | nil = {()};
relation Roa (cons(x,xs)) = Rmem(xs) X {(x)} | nil = {()};
relation Robs = Rob*;
relation Roas = Roa*;


(* 
 * fold spec. We seed our language with uninterpreted relations qRm
 * and qRo. Provision to declare relation variables before 
 * function name will be provided in next update.
 *)

foldl : l -> b -> (y -> acc 
        -> {v| qRm(v) = {(y)} U qRm(acc)
            /\ qRo(v) = ({(x)} X qRm(acc)) U qRo(acc) }) 
        -> {v | qRm(v) = Rmem(l) U qRm(b)
             /\ qRo(v) = Roas(l) U qRo(b) U (Rmem(l) X qRm(b))};

foldr : l -> b -> (y -> acc 
        -> {v| qRm(v) = {(y)} U qRm(acc)
            /\ qRo(v) = ({(x)} X qRm(acc)) U qRo(acc) }) 
        -> {v | qRm(v) = Rmem(l) U qRm(b)
             /\ qRo(v) = Robs(l) U qRo(b) U (Rmem(l) X qRm(b))};

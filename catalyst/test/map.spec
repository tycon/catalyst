relation (Rhd R)(cons(x,xs)) = R(x) | nil = {()};
relation (Rmem R) = Rhd[R]*;
relation (Rob R)(cons(x,xs)) = R(x) X Rmem[R](xs) | nil = {()};
relation (Robs R) = Rob[R]*;
relation (Roa R)(cons(x,xs)) = Rmem[R](xs) X R(x) | nil = {()};
relation (Roas R) = Roa[R]*;

(R1,R2) map : l -> (x -> {v | R2(v) = R1(x)}) 
  -> {v | Rmem[R2](v) = Rmem[R1](l)};

(R,qRm,qRo) foldl : l -> b -> (y -> acc 
        -> {v| qRm(v) = R(y) U qRm(acc)
            /\ qRo(v) = (R(y) X qRm(acc)) U qRo(acc) }) 
        -> {v | qRm(v) = Rmem[R](l) U qRm(b)
             /\ qRo(v) = Roas[R](l) U qRo(b) U (Rmem[R](l) X qRm(b))};

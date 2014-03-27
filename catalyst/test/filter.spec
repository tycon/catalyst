relation (Rhd R)(cons(x,xs)) = R(x) | nil = {()};
relation (Rmem R) = Rhd[R]*;
primitive relation RId = \z.{(z)};

(R1) filter : l -> (x -> {v | ([v=true] => (R1(x) = {()}))
                           /\ ([v=false] => (R1(x) = {(x)})) }) 
  -> {v | Rmem[RId](v) = Rmem[R1](l)};

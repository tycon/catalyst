relation (Rhd R)(cons(x,xs)) = R(x) | nil = {()};
relation (Rmem R) = Rhd[R]*;

(R1) exists : l -> (x -> {v | [v=true] <=> (not (R1(x) = {()}))}) 
  -> {v | [v=true] <=> (not (Rmem[R1](l) = {()}))};

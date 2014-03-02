relation Rhd (cons(x,xs)) = {(x)} | nil = {()};
relation Rmem = Rhd*;
relation Rfv (Var x) = {(x)} 
      | (App (e1,e2)) = Rfv(e1) U Rfv(e2)
      | (Abs (id,e)) = Rfv(e) - {(id)};

assume idEq : (x,y) -> {v | [v=true] <=> ({(x)} = {(y)})};
assume listHas : l -> x -> {v | [v=true] <=> ({(x)} C= Rmem(l))};
assume freeVars : e -> {l | Rmem(l) = Rfv(e)};
assume createNewName : fvs -> id -> {v | (not ({(v)} = {(id)})) /\
                                  (Rmem(fvs) - {(v)} = Rmem(fvs))};
alphaConvert : e -> {ex | Rfv(ex) = Rfv(e)};
subst : e1 -> id -> e2 -> {ex | ( ({(id)} C= Rfv(e2)) /\ 
  (Rfv(ex) = (Rfv(e2) - {(id)}) U Rfv(e1)) ) \/ 
  ( (not ({(id)} C= Rfv(e2))) /\ (Rfv(ex) = Rfv(e2)))};

relation Rroot (T(c,l,n,r)) = {(n)} | E = {()};
relation Rmem = Rroot*;
relation Rto (T (c,l,n,r)) = (Rmem(l) X {(n)}) U ({(n)} X Rmem(r)) U
                              (Rmem(l) X Rmem(r)) | E = {()};
relation Rtos = Rto*;

balance : t -> {t' | Rmem(t') = Rmem(t) /\ Rtos(t') = Rtos(t)};

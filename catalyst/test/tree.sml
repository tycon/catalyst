exception Catalyst;

datatype colour = R | B
datatype 'a tree = E | T of colour * 'a tree * 'a * 'a tree
fun balance (t:'a tree) : 'a tree = case t of
    T (B,T (R,T (R,a,x,b),y,c),z,d) => T (R,T (B,a,x,b),y,T (B,c,z,d))
  | T (B,T (R,a,x,T (R,b,y,c)),z,d) => T (R,T (B,a,x,b),y,T (B,c,z,d))
  | T (B,a,x,T (R,T (R,b,y,c),z,d)) => T (R,T (B,a,x,b),y,T (B,c,z,d))
  | T (B,a,x,T (R,b,y,T (R,c,z,d))) => T (R,T (B,a,x,b),y,T (B,c,z,d))
  | _ => t


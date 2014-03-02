exception Catalyst;

datatype 'a tree = Leaf of 'a
                 | Node of 'a tree * 'a * 'a tree

fun concat l1 l2 = case l1 of
    [] => l2
  | x::xs => x::(concat xs l2)

fun preOrder t = case t of
    Leaf x => [x]
  | Node z => 
    let
      val (l,x,r) = z
    in
      concat (concat (preOrder l) [x]) (preOrder r)
    end 

fun postOrder t = case t of
    Leaf x => [x]
  | Node (l,x,r) => concat (concat (postOrder l) (postOrder r)) [x]


exception Catalyst

fun exists l f = case l of
    [] => false
  | x::xs =>
    let
      val v1 = exists xs f
      val v2 = f(x)
      val v3 = v1 orelse v2
    in
      v3 
    end

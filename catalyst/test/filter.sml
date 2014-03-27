exception Catalyst

fun filter l f = case l of
    [] => []
  | x::xs =>
    let
      val v1 = filter xs f
      val v2 = f(x)
    in
      if v2 then v1 else x::v1
    end

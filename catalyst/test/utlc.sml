exception Catalyst;

datatype exp =    Var of string
                | App of exp*exp
                | Abs of string*exp

fun idEq (x,y) = (x=y)

fun listHas l n = case l of
    [] => false
  | x::xs => (idEq(x,n)) orelse (listHas xs n)

fun freeVars e = case e of
      Var id => [id]
    | App (e1,e2) => List.concat [freeVars e1,freeVars e2]
    | Abs (id,e') => List.filter (fn fv => not (fv = id)) 
        (freeVars e')

fun createNewName fvs base = 
  let
    val name = base ^ "'"
  in
    if List.exists (fn fv => fv = name) fvs
    then createNewName fvs name
    else name
  end

fun alphaConvert e = case e of                                      
    Abs (id,e') => 
    let 
      val fv_e' = freeVars e'                                       
      val id' = createNewName fv_e' id
    in
      Abs(id',subst (Var id') id e')
    end
  | _ => raise Fail "No alpha-conversion for Unbound terms"

  (* Capture-avoiding substitution *)
and subst (e1:exp) (id:string) (e2:exp) :exp = case e2 of
    Var id' => if idEq (id,id')
      then e1 else e2
  | App(e21,e22) => App(subst e1 id e21, subst e1 id e22)
  | Abs(id',e2') => if idEq (id',id) then e2 else
    let
      val fv_e1 = freeVars e1 
    in
      if listHas fv_e1 id'
      then subst e1 id (alphaConvert e2)
      else Abs(id',subst e1 id e2')
    end (*Abs(id',subst e1 id e2')*)

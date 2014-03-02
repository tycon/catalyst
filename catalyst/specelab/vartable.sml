signature VAR_TABLE = 
sig
  exception VarNotFound
  
  include ATOMS
  include HASH_TABLE

  type 'b t = (Var.t,'b) hash_table

  val empty : unit -> 'b t

  val mem : 'b t -> Var.t -> bool

  val replace : 'b t -> Var.t -> 'b -> unit
end
structure VarTable : VAR_TABLE =
struct
  open Atoms
  open HashTable

  exception VarNotFound

  type 'b t = (Var.t,'b) hash_table

  fun empty () = mkTable ((HashString.hashString) o (Var.toString), 
        Var.equals) (37, VarNotFound)

  fun mem m k = inDomain m k

  fun replace m k v = insert m (k,v)
end

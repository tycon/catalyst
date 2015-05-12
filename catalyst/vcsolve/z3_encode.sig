signature Z3_ENCODE_STRUCTS = 
sig
  structure Z3_FFI : Z3_FFI_INTERFACE
  val z3_log : string -> unit
end
signature Z3_ENCODE = 
sig
  include Z3_ENCODE_STRUCTS
  type context
  type sort
  type set
  type ast
  type struc_rel
  type assertion
  datatype satisfiability = SAT | UNSAT | UNKNOWN
  datatype api = API of 
    {
      bool_sort : sort,
      int_sort : sort,
      const_false : ast,
      const_true : ast,
      falsee : assertion,
      truee : assertion,
      sortToString : sort -> string,
      constToString : ast -> string,
      strucRelToString : struc_rel -> string,
      mkUninterpretedSort :  unit -> sort,
      mkConst : (string * sort) -> ast,
      mkInt : int -> ast,
      mkStrucRel :  (string * sort vector) -> struc_rel,
      mkStrucRelApp : struc_rel * ast -> set,
      mkNullSet : unit -> set,
      mkSingletonSet : ast vector -> set,
      mkUnion :  (set * set) -> set,
      mkIntersection :  (set * set) -> set,
      mkCrossPrd :  (set * set) -> set ,
      mkDiff :  (set * set) -> set ,
      mkSetEqAssertion :  (set * set) -> assertion,
      mkSubSetAssertion : (set * set) -> assertion,
      mkConstEqAssertion :  (ast * ast) -> assertion,
      mkNot :  assertion -> assertion,
      mkIf : assertion * assertion -> assertion,
      mkIff : assertion * assertion -> assertion,
      mkAnd : assertion vector -> assertion,
      mkOr : assertion vector -> assertion,
      dischargeAssertion :  assertion -> unit,
      doPush : unit -> unit,
      doPop : unit -> unit,
      checkSAT : unit -> satisfiability
    }
  exception InvalidOperation
  val mkDefaultContext : unit -> context
  val generateAPI : context -> api
  val checkContext : context -> satisfiability
  val delContext : context -> unit
  val logComment : string -> unit
end

functor ANormalCoreML (S: A_NORMAL_CORE_ML_STRUCTS): A_NORMAL_CORE_ML = 
struct

open S

structure Field = Record.Field

fun maybeConstrain (x, t) =
   let
      open Layout
   in
      if !Control.showTypes
         then seq [x, str ": ", Type.layout t]
      else x
   end

fun layoutTargs (ts: Type.t vector) =
   let
      open Layout
   in
      if !Control.showTypes
         andalso 0 < Vector.length ts
         then list (Vector.toListMap (ts, Type.layout))
      else empty
   end

structure Pat =
   struct

      structure Val =
        struct
          datatype atom =
              Const of Const.t
            | Var of Var.t
            | Wild

          datatype t = 
              Atom of atom
            | Tuple of atom vector
            | Record of atom Record.t

          fun layout v = 
            let
              open Layout
            in
              case v of
                Atom (Const c) => Const.layout c
              | Atom (Var v) => Var.layout v
              | Atom (Wild) => str "_"
              | Tuple av => tuple (Vector.toListMap (av, fn v => layout (Atom v)))
              | Record r => record (Vector.toListMap (Record.toVector r, fn (f, p) =>
                             (Field.toString f, layout (Atom p))))
            end
        end

      datatype t = T of {node: node,
                         ty: Type.t}
      and node =
         Con of {arg: Val.t option,
                 con: Con.t,
                 targs: Type.t vector}
       | Value of Val.t

      local
         fun make f (T r) = f r
      in
         val dest = make (fn {node, ty} => (node, ty))
         val node = make #node
         val ty = make #ty
      end

      fun make (n, t) = T {node = n, ty = t}

      fun layout p =
         let
            val t = ty p
            open Layout
         in
            case node p of
               Con {arg, con, targs} =>
                  seq [Con.layout con,
                       layoutTargs targs,
                       case arg of
                          NONE => empty
                        | SOME p => Val.layout p]
             | Value v => Val.layout v
         end
  end

structure NoMatch =
   struct
      datatype t = Impossible | RaiseAgain | RaiseBind | RaiseMatch
   end

datatype noMatch = datatype NoMatch.t

datatype exp_val_atom = Const of Const.t
                      | Var of Var.t * Type.t vector

datatype exp_val_t = Atom of exp_val_atom
                  | Tuple of exp_val_atom vector
                  | Record of exp_val_atom Record.t

datatype valbind =
    ExpBind of Pat.Val.t * exp
  | PatBind of Pat.t * exp_val_t

and dec =
   Datatype of {cons: {arg: Type.t option,
                       con: Con.t} vector,
                tycon: Tycon.t,
                tyvars: Tyvar.t vector} vector
 | Exception of {arg: Type.t option,
                 con: Con.t}
 | Fun of {decs: {lambda: lambda,
                  var: Var.t} vector,
           tyvars: unit -> Tyvar.t vector}
 | Val of {rvbs: {lambda: lambda,
                  var: Var.t} vector,
           tyvars: unit -> Tyvar.t vector,
           vbs: {valbind : valbind,
                 lay: unit -> Layout.t,
                 nest: string list} vector}
and exp = Exp of {node: expNode,
                  ty: Type.t}
and expNode =
   App of exp_val_atom * exp_val_t
 | Case of {kind: string,
            lay: unit -> Layout.t,
            nest: string list,
            rules: {exp: exp,
                    lay: (unit -> Layout.t) option,
                    pat: Pat.t} vector,
            test: exp_val_t}
 | EnterLeave of exp_val_t * SourceInfo.t
 | Handle of {catch: Var.t * Type.t,
              handler: exp,
              try: exp}
 | Lambda of lambda
 | Let of dec vector * exp
 | PrimApp of {args: exp_val_t vector,
               prim: Type.t Prim.t,
               targs: Type.t vector}
 (*| Raise of exp_val_t*)
 | Nop 
 | Seq of exp vector
 | Value of exp_val_t

and lambda = Lam of {arg: Var.t,
                     argType: Type.t,
                     body: exp}

local
   open Layout
in
  fun exp_val_layt v = case v of
      Atom (Const c) => Const.layout c
    | Atom (Var (v,targs)) =>
        if Vector.isEmpty targs then Var.layout v
          else seq [Var.layout v, str " ", tuple (Vector.toListMap
            (targs, Type.layout))]
    | Tuple av => seq (Vector.toListMap (av, fn v => exp_val_layt (Atom v)))
    | Record r => Record.layout
            {extra = "",
             layoutElt = fn atm => exp_val_layt (Atom atm),
             layoutTuple = fn es => tuple (Vector.toListMap (es, 
                fn atm => exp_val_layt (Atom atm))),
             record = r,
             separator = " = "}

   fun layoutTyvars (ts: Tyvar.t vector) =
      case Vector.length ts of
         0 => empty
       | 1 => seq [str " ", Tyvar.layout (Vector.sub (ts, 0))]
       | _ => seq [str " ", tuple (Vector.toListMap (ts, Tyvar.layout))]

   fun layoutConArg {arg, con} =
      seq [Con.layout con,
           case arg of
              NONE => empty
            | SOME t => seq [str " of ", Type.layout t]]

   fun layoutDec d =
      case d of
         Datatype v =>
            seq [str "datatype",
                 align
                 (Vector.toListMap
                  (v, fn {cons, tycon, tyvars} =>
                   seq [layoutTyvars tyvars,
                        str " ", Tycon.layout tycon, str " = ",
                        align
                        (separateLeft (Vector.toListMap (cons, layoutConArg),
                                       "| "))]))]
       | Exception ca =>
            seq [str "exception ", layoutConArg ca]
       | Fun {decs, tyvars, ...} => layoutFuns (tyvars, decs)
       | Val {rvbs, tyvars, vbs, ...} =>
            align [layoutFuns (tyvars, rvbs),
                   align (Vector.toListMap
                          (vbs, fn {valbind, ...} => case valbind of
                              PatBind (pat,expval) => seq [str "val",
                                mayAlign [seq [layoutTyvars (tyvars ()),
                                               str " ", Pat.layout pat,
                                               str " .="],
                                          exp_val_layt expval]]
                            | ExpBind (patval,exp) => seq [str "val",
                                mayAlign [seq [layoutTyvars (tyvars ()),
                                               str " ", Pat.Val.layout patval,
                                               str " =."],
                                          layoutExp exp]]))]
   and layoutExp (Exp {node, ...}) =
      case node of
         App (f, arg) => paren (seq [exp_val_layt (Atom f), str " ", exp_val_layt arg])
       | Case {rules, test, ...} =>
            Pretty.casee {default = NONE,
                          rules = Vector.map (rules, fn {exp, pat, ...} =>
                                              (Pat.layout pat, layoutExp exp)),
                          test = exp_val_layt test}
       | EnterLeave (v, si) =>
            seq [str "EnterLeave ",
                 tuple [exp_val_layt v, SourceInfo.layout si]]
       | Handle {catch, handler, try} =>
            Pretty.handlee {catch = Var.layout (#1 catch),
                            handler = layoutExp handler,
                            try = layoutExp try}
       | Lambda l => layoutLambda l
       | Let (ds, e) =>
            Pretty.lett (align (Vector.toListMap (ds, layoutDec)),
                         layoutExp e)
       | PrimApp {args, prim, targs} =>
            Pretty.primApp {args = Vector.map (args, exp_val_layt),
                            prim = Prim.layout prim,
                            targs = Vector.map (targs, Type.layout)}
       (*| Raise v => Pretty.raisee (exp_val_layt v)*)
       | Nop => str "Nop"
       | Seq es => Pretty.seq (Vector.map (es, layoutExp))
        | Value v => exp_val_layt v

   and layoutFuns (tyvars, decs)  =
      if 0 = Vector.length decs
         then empty
      else
         align [seq [str "val rec", layoutTyvars (tyvars ())],
                indent (align (Vector.toListMap
                               (decs, fn {lambda as Lam {argType, body = Exp {ty = bodyType, ...}, ...}, var} =>
                                align [seq [maybeConstrain (Var.layout var, Type.arrow (argType, bodyType)), str " = "],
                                       indent (layoutLambda lambda, 3)])),
                        3)]
   and layoutLambda (Lam {arg, argType, body, ...}) =
      paren (align [seq [str "fn ", 
                         maybeConstrain (Var.layout arg, argType),
                         str " =>"],
                    layoutExp body])

   fun layoutExpWithType (exp as Exp {ty, ...}) =
      let
         val node = layoutExp exp
      in
         if !Control.showTypes
            then seq [node, str " : ", Type.layout ty]
         else node
      end
end

structure Lambda =
   struct
      datatype t = datatype lambda

      val make = Lam

      fun dest (Lam r) = r

      val bogus = make {arg = Var.newNoname (),
                        argType = Type.unit,
                        body = Exp {node = Seq (Vector.new0 ()),
                                    ty = Type.unit} }
   end

structure Exp =
   struct

    structure Val =
      struct
        datatype atom = datatype exp_val_atom
        datatype t = datatype exp_val_t
        val layout = exp_val_layt
      end

      type dec = dec
      type lambda = lambda
      datatype t = datatype exp
      datatype node = datatype expNode

      datatype noMatch = datatype noMatch

      val layout = layoutExp
      val layoutWithType = layoutExpWithType

      local
         fun make f (Exp r) = f r
      in
         val dest = make (fn {node, ty} => (node, ty))
         val node = make #node
         val ty = make #ty
      end

      fun make (n, t) = Exp {node = n,
                             ty = t}
   end

structure Dec =
   struct
      datatype valbind = datatype valbind

      datatype t = datatype dec

      val layout = layoutDec
   end

structure Program =
   struct
      datatype t = T of {decs: Dec.t vector}

      fun layouts (T {decs, ...}, output') =
         let
            open Layout
            (* Layout includes an output function, so we need to rebind output
             * to the one above.
             *)
            val output = output'
         in
            output (Layout.str "\n\nDecs:")
            ; Vector.foreach (decs, output o Dec.layout)
         end
   end

end

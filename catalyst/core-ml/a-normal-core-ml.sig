(*
 * Author : GK
 *)
signature A_NORMAL_CORE_ML_STRUCTS = 
   sig
      include ATOMS
      structure Type:
         sig
            type t

            val arrow: t * t -> t
            val bool: t
            val deConOpt: t -> (Tycon.t * t vector) option
            val deRecord: t -> (Record.Field.t * t) vector
            val isCharX: t -> bool
            val isInt: t -> bool
            val layout: t -> Layout.t
            val makeHom: {con: Tycon.t * 'a vector -> 'a,
                          var: Tyvar.t -> 'a} -> {destroy: unit -> unit,
                                                  hom: t -> 'a}
            val tuple: t vector -> t
            val unit: t
            val var : Tyvar.t -> t
            val toMyType : t -> TypeDesc.t
            val renameTyVars : (Tyvar.t * Tyvar.t) vector -> t -> t
         end
   end

signature A_NORMAL_CORE_ML = 
   sig
      include A_NORMAL_CORE_ML_STRUCTS

      structure Pat:
         sig

          structure Val:
            sig
              datatype atom =
                  Const of Const.t
                | Var of Var.t
                | Wild
              datatype t = 
                  Atom of atom
                | Tuple of atom vector
                | Record of atom Record.t
              val layout : t -> Layout.t
            end

            type t
            (*
             * Pat.List => Pat.Con {con = Con.cons, ...}
             * Pat.Layered (v,pat) => elab(val v=pat)
             *)
            datatype node =
               Con of {arg: Val.t option,
                       con: Con.t,
                       targs: Type.t vector}
             | Value of Val.t
            val dest: t -> node * Type.t
            val layout: t -> Layout.t
            val make: node * Type.t -> t
            val node: t -> node
            val ty : t -> Type.t
         end

      structure Exp:
         sig
            structure Val:
              sig
                datatype atom =
                    Const of Const.t
                  | Var of Var.t * Type.t vector
                datatype t = 
                    Atom of atom
                  | Tuple of atom vector
                  | Record of atom Record.t
                val layout : t -> Layout.t
              end
            type dec
            type lambda
            type t
            datatype node =
               App of Val.atom * 
                      Val.t (*GK*)
             | Case of {kind: string,
                        lay: unit -> Layout.t,
                        nest: string list,
                        rules: {exp: t,
                                lay: (unit -> Layout.t) option,
                                pat: Pat.t} vector,
                        test: Val.t} (*GK*)
             | EnterLeave of Val.t * SourceInfo.t
             | Handle of {catch: Var.t * Type.t,
                          handler: t,
                          try: t}
             | Lambda of lambda
             | Let of dec vector * t
             | PrimApp of {args: Val.t vector, (*GK*)
                           prim: Type.t Prim.t,
                           targs: Type.t vector}
             (*| Raise of Val.t *)
             | Nop
             | Seq of t vector 
             | Value of Val.t
            val dest: t -> node * Type.t
            val layout: t -> Layout.t
            val layoutWithType: t -> Layout.t
            val make: node * Type.t -> t
            val node: t -> node
            val ty: t -> Type.t
         end

      structure Lambda:
         sig
            type t

            val bogus: t
            val dest: t -> {arg: Var.t,
                            argType: Type.t,
                            body: Exp.t}
            val make: {arg: Var.t,
                       argType: Type.t,
                       body: Exp.t} -> t
         end
      sharing type Exp.lambda = Lambda.t

      structure Dec:
         sig
            datatype valbind =
                ExpBind of Pat.Val.t * Exp.t
              | PatBind of Pat.t * Exp.Val.t

            datatype t =
               Datatype of {cons: {arg: Type.t option,
                                   con: Con.t} vector,
                            tycon: Tycon.t,
                            tyvars: Tyvar.t vector} vector
             | Exception of {arg: Type.t option,
                             con: Con.t}
             | Fun of {decs: {lambda: Lambda.t,
                              var: Var.t} vector,
                       tyvars: unit -> Tyvar.t vector}
             | Val of {rvbs: {lambda: Lambda.t,
                              var: Var.t} vector,
                       tyvars: unit -> Tyvar.t vector,
                       vbs: {valbind: valbind,
                             lay: unit -> Layout.t,
                             nest: string list} vector}

            val layout: t -> Layout.t
         end
      sharing type Exp.dec = Dec.t

      structure Program:
         sig
            datatype t = T of {decs: Dec.t vector}

            val layouts: t * (Layout.t -> unit) -> unit
         end
   end

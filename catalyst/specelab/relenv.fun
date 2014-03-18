functor RelEnv (S : REL_ENV_STRUCTS) : REL_ENV = 
struct
  open S
  open SpecLang

  structure TyD = TypeDesc

  type reldesc = { ty : ProjTypeScheme.t,
                  map : (Con.t * Var.t vector option * RelLang.expr) 
                    vector}

  val relIdStrEq = fn (v,v') => 
    (RelId.toString v) = (RelId.toString v')

  structure Key:KEY = 
  struct
    type t = RelId.t
    val equal = relIdStrEq
    val layout = Layout.str o RelId.toString
  end
  structure Value:VALUE = 
  struct
    type t = reldesc
    val toString = fn ({ty,map}) =>
      let
        val tyDS = ProjTypeScheme.toString ty
        val conmap = "{" ^ (Vector.toString (fn (c,vlo,rexpr) =>
            let
              val cstr = Con.toString c
              val vseq = case vlo of NONE => ""
                | SOME vl => Vector.toString Var.toString vl
              val trmstr = RelLang.exprToString rexpr
            in
              cstr ^ vseq ^ " => " ^ trmstr
            end) map) ^ "}"
      in
        "{type = "^tyDS^", map = "^conmap^"}"
      end
    fun layout t = (Layout.str o toString) t
  end

  structure RelMap = ApplicativeMap (structure Key = Key
                                     structure Value = Value)

  exception RelNotFound of RelId.t

  type t = RelMap.t

  val empty = RelMap.empty

  val mem = RelMap.mem

  fun find env relId = RelMap.find env relId 
    handle (RelMap.KeyNotFound k) => raise (RelNotFound k)

  val add = fn env => fn (var,tys) => RelMap.add env var tys 

  val addUniterp = fn env => fn (r,pts) => RelMap.add env r {ty=pts, 
    map=Vector.new0 ()}

  val remove = RelMap.remove

  val toVector = RelMap.toVector

  val layout = RelMap.layout
end

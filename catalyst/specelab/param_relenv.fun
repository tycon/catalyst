functor ParamRelEnv (S : PARAM_REL_ENV_STRUCTS) : PARAM_REL_ENV = 
struct
  open S
  open SpecLang

  structure TyD = TypeDesc

  type reldesc = { ty : ProjTypeScheme.t,
                   def : Bind.def}

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
    val toString = fn ({ty,def}) =>
      let
        val tyDS = ProjTypeScheme.toString ty
        val defStr = Bind.defToString def
      in
        "{typescheme = "^tyDS^", def = "^defStr^"}"
      end
    fun layout t = (Layout.str o toString) t
  end

  structure RelMap = ApplicativeMap (structure Key = Key
                                     structure Value = Value)

  exception ParamRelNotFound of RelId.t

  type t = RelMap.t

  val empty = RelMap.empty

  val mem = RelMap.mem

  fun find env relId = RelMap.find env relId 
    handle (RelMap.KeyNotFound k) => raise (ParamRelNotFound k)

  val add = fn env => fn (var,tys) => RelMap.add env var tys 

  val addUniterp = fn env => fn (var,tys) => RelMap.add env var
  {ty=tys, def=Bind.BogusDef}

  val remove = RelMap.remove

  val toVector = RelMap.toVector

  val layout = RelMap.layout
end

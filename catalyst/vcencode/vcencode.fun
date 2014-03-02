functor VCEncode (S : VC_ENCODE_STRUCTS) : VC_ENCODE =
struct
  open S
  open VC
  structure TyD = TypeDesc
  structure RI = RelLang.RelId
  structure BP = Predicate.BasePredicate
  structure RP = Predicate.RelPredicate
  structure L = Layout
  datatype result = Success | Undef | Failure
  structure Z3_Encode = Z3_Encode (structure Z3_FFI = Z3_FFI
                                   val z3_log = z3_log)
  exception TyDNotFound
  exception ConstNotFound
  exception RelNotFound
      
 (* structure TyMap : APPLICATIVE_MAP where
    type Key.t = TypeDesc.t and type Value.t = Z3_Encode.sort =
  struct
    structure Key = 
    struct
      type t = TyD.t
      val layout = TyD.layout
      val equal = TyD.sameType
    end
    structure Value =
    struct
      type t = Z3_Encode.sort
      val layout = fn _ => L.empty
    end
    structure Map = ApplicativeMap (structure Key = Key
                                   structure Value = RelLang.RelId)
    open Map
  end*)

  fun $ (f,arg) = f arg
  infixr 5 $
  val assert = Control.assert
  val ignore = fn _ => ()
  
  fun discharge (VC.T (tydbinds,anteP,conseqP)) =
    let
      val ctx = Z3_Encode.mkDefaultContext ()
      (*
       * APIs for the current context.
       *)
      val api = Z3_Encode.generateAPI ctx
      val bool_sort = #bool_sort api
      val int_sort = #int_sort api
      val const_true = #const_true api
      val const_false = #const_false api
      val truee = #truee api
      val falsee = #falsee api
      val mkUninterpretedSort = #mkUninterpretedSort api
      val mkConst = #mkConst api
      val mkInt = #mkInt api
      val mkStrucRel = #mkStrucRel api
      val mkStrucRelApp = #mkStrucRelApp api
      val mkNullSet = #mkNullSet api
      val mkSingletonSet = #mkSingletonSet api
      val mkUnion = #mkUnion api
      val mkCrossPrd = #mkCrossPrd api
      val mkDiff = #mkDiff api
      val mkSetEqAssertion = #mkSetEqAssertion api
      val mkSubSetAssertion = #mkSubSetAssertion api
      val mkConstEqAssertion = #mkConstEqAssertion api
      val mkNot = #mkNot api
      val mkIf = #mkIf api
      val mkIff = #mkIff api
      val mkAnd = #mkAnd api
      val mkOr = #mkOr api
      val dischargeAssertion = #dischargeAssertion api
      (*
       * Maps to keep track of encoded values
       *)
      fun strEq (str1,str2) = (str1 = str2)
      val tyMap = HashTable.mkTable (MLton.hash, TyD.sameType) 
        (117, TyDNotFound)
      val intTyD = TyD.makeTconstr (Tycon.intInf,[])
      val boolTyD = TyD.makeTconstr (Tycon.bool,[])
      val _ = HashTable.insert tyMap (intTyD,int_sort)
      val _ = HashTable.insert tyMap (boolTyD,bool_sort)
      fun addTyD tyd = (fn sort => 
          (HashTable.insert tyMap (tyd,sort); sort))
        (mkUninterpretedSort ())
      val constMap = HashTable.mkTable (MLton.hash, strEq) 
        (117, ConstNotFound)
      val relMap = HashTable.mkTable (MLton.hash, strEq)
        (117, RelNotFound)
      fun getConstForVar v = (fn vstr => HashTable.lookup constMap vstr
        handle ConstNotFound => Error.bug ("Variable "^vstr^" undec\
          \lared despite processing tydbinds")) (Var.toString v)
      fun getStrucRelForRelId rid = (fn rstr => HashTable.lookup relMap
        rstr handle RelNotFound => Error.bug ("Rel "^rstr^" undec\
          \lared despite processing tydbinds")) (RI.toString rid)
      (*
       * Encoding functions
       * encodeConst and encodeStrucRel rely on uniqueness of 
       * bindings in tydbinds. In case of duplicate bindings,
       * duplication declarations show up in Z3 VC, but most
       * recent binding is used.
       *)
      fun encodeTyD tyD = case HashTable.find tyMap tyD of
          SOME sort => sort
        | NONE => (case tyD of TyD.Tvar _ =>  addTyD tyD
            | TyD.Tconstr _ => addTyD tyD
            | _ => Error.bug "Unexpected type")

      fun encodeConst (v:Var.t,tyd:TyD.t) = 
        let
          val vstr = Var.toString v
          val sort = encodeTyD tyd 
          val const = mkConst (vstr,sort)
          val _ = HashTable.insert constMap (vstr,const)
        in
          const
        end

      fun encodeStrucRel (rid : RI.t, TyD.Tarrow (t1,_)) =
        let
          val rstr = RI.toString rid
          val sorts = case t1 of 
              TyD.Trecord tydr => Vector.map (#2 $ Vector.unzip $ 
                Record.toVector tydr, encodeTyD)
            | _ => Vector.new1 $ encodeTyD t1
          val sr = mkStrucRel (rstr,sorts)
          val _ = HashTable.insert relMap (rstr,sr)
        in
          sr
        end

      fun processTyDBind (v:Var.t,tyd:TyD.t) = case tyd of 
        (*
         * Currently, the only values with function types
         * are structural relations encoded as functions from
         * a val or tuple of vals to bool.
         *)
          TyD.Tarrow (t1,TyD.Tconstr (tycon,_)) => 
            if Tycon.isBool tycon then ignore $ encodeStrucRel 
              (RI.fromString $ Var.toString v, tyd)
            else ignore $ encodeConst (v,tyd)
        | _ => ignore $ encodeConst (v,tyd)

      fun encodeBasePred (bp:BP.t) : Z3_Encode.assertion = 
        let
          open BP
          val encodeBaseExpr = fn (Int i) => mkInt i
            | Bool true => const_true | Bool false => const_false
            | Var v => getConstForVar v
        in
          case bp of Eq (e1,e2) => mkConstEqAssertion 
              (encodeBaseExpr e1, encodeBaseExpr e2)
            | Iff (bp1,bp2) => mkIff (encodeBasePred bp1,
                encodeBasePred bp2)
        end

      fun encodeRelPred (rp:RP.t) : Z3_Encode.assertion =
        let
          open RelLang
          val encodeRelElem = fn (Int i) => mkInt i 
            | Bool true => const_true | Bool false => const_false
            | Var v => getConstForVar v
          fun encodeRelExpr (e:expr) : Z3_Encode.set =
            case e of T els => (case Vector.length els of
                0 => mkNullSet ()
              | _ => mkSingletonSet $ Vector.map (els,
                  encodeRelElem))
            | X (e1,e2) => mkCrossPrd (encodeRelExpr e1, 
                encodeRelExpr e2)
            | U (e1,e2) => mkUnion (encodeRelExpr e1, 
                encodeRelExpr e2)
            | D (e1,e2) => mkDiff (encodeRelExpr e1, 
                encodeRelExpr e2)
            | R (rid,v) => mkStrucRelApp (getStrucRelForRelId rid,
                getConstForVar v)
          val f = encodeRelExpr
          open RP
        in
          case rp of Eq (e1,e2) => mkSetEqAssertion (f e1, f e2)
          | Sub (e1,e2) => mkSubSetAssertion (f e1, f e2)
          | SubEq (e1,e2) => (fn s => mkOr $ Vector.new2 (mkSetEqAssertion s,
              mkSubSetAssertion s)) (f e1, f e2)
        end

      fun encodeSimplePred (sp : VC.simple_pred) : Z3_Encode.assertion =
        case sp of 
          (Base bp) => encodeBasePred bp
        | (Rel rp) => encodeRelPred rp

      val assertSimplePred  = dischargeAssertion o encodeSimplePred

      fun encodeVCPred vcp = case vcp of 
          VC.Simple sp => encodeSimplePred sp
        | VC.Conj vcps => mkAnd $ Vector.map (vcps, encodeVCPred)
        | VC.Disj vcps => mkOr $ Vector.map (vcps, encodeVCPred)
        | VC.Not vcp => mkNot $ encodeVCPred vcp
        | VC.If (vcp1,vcp2) => mkIf (encodeVCPred vcp1, 
              encodeVCPred vcp2)
        | VC.Iff (vcp1,vcp2) => mkIff (encodeVCPred vcp1, 
              encodeVCPred vcp2)

      fun assertVCPred vcp = case vcp of 
          VC.Simple sp => assertSimplePred sp
        | VC.Conj spv => Vector.foreach (spv,assertVCPred)
        | _ => dischargeAssertion $ encodeVCPred vcp
     
      val _ = Vector.foreach (tydbinds, processTyDBind)
      val _ = assertVCPred anteP
      (*
       * We check the SAT of ¬conseqP
       *)
      val _ = dischargeAssertion $ mkNot $ encodeVCPred conseqP
      val res = Z3_Encode.checkContext ctx
      val _ = Z3_Encode.delContext ctx
    in
      case res of ~1 => Success | 0 => Undef | 1 => Failure
        | _ => Error.bug "Integer received when Z3_lbool expected"
    end
end

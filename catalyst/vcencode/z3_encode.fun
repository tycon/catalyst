functor Z3_Encode (S : Z3_ENCODE_STRUCTS) :> Z3_ENCODE =
struct
  open S
  open Z3_FFI
  structure L = Layout
  exception InvalidOperation
  datatype sort = Int of z3_sort
                | Bool of z3_sort
                | T of string * z3_sort
  datatype ast = AST of z3_ast * sort
  (*
   * Set Invariant : len(ty) = len(domain(pred)) 
   * Null constructor is motivated by two reasons:
   * 1. To get away with not knowing the type of Null
   * set until we use it in an operation, where its type
   * is inferred and, consequently, gets converted to an 
   * empty set.
   * 2. What if it gets used in an operation with another
   * null set? The result can be statically determined 
   * without even having to go to Z3 - second motivation.
   *)
  datatype set = Null
               | Set of {ty : sort vector,
                         pred : ast vector -> z3_ast}
  datatype struc_rel = SR of {ty : sort vector, 
                              rel : ast -> set}
  type assertion = z3_ast
  type context = z3_context
  val log = z3_log

  fun $ (f,arg) = f arg
  infixr 5 $
  val assert = Control.assert
  fun mkGenName (cnt,base) =
    let
      val count = ref cnt
    in
      fn () => 
        let 
          val name = base ^ (Int.toString (!count))
          val _ = count := !count + 1
        in
          name
        end
    end
  val genTypeName = mkGenName (0,"T")
  val genSetName = mkGenName (0,"set")

  fun mkDefaultContext () =
    let
      val cfg = Z3_mk_config ()
      val _   = Z3_global_param_set ("smt.macro-finder","true")
      val ctx = Z3_mk_context cfg
      val _   = Z3_del_config cfg
    in
      ctx
    end

  fun checkContext ctx = 
    let
      val _ = log "(check-sat)"
      val _ = log "\n\n"
    in
      Z3_check ctx
    end 
  val delContext = Z3_del_context

  (*
   * This function implements an object with encapsulated
   * state (ctx). 
   * Reference : http://mlton.org/ObjectOrientedProgramming
   *)
  fun generateAPI ctx = 
    let
      val mkSym = fn name => Z3_mk_string_symbol (ctx,name)

      (*
       * bool and int sorts declared here are not exposed.
       * Ones that are exposed are declared at the end.
       *)
      val bool_sort = Z3_mk_bool_sort ctx

      val int_sort = Z3_mk_int_sort ctx

      val falsee = Z3_mk_false ctx

      val truee = Z3_mk_true ctx

      fun mkUninterpretedSort () = 
        let
          val name = genTypeName ()
          val z3_sort = Z3_mk_uninterpreted_sort (ctx, mkSym name)
          val _ = log ("(declare-sort "^(Z3_sort_to_string 
            (ctx,z3_sort))^")")
          val _ = log "\n"
        in
          T (name, z3_sort)
        end

      fun sortToZ3Sort sort = case sort of Int t => t 
        | Bool t => t | T (name,t) => t

      fun sortToString sort = Z3_sort_to_string (ctx, sortToZ3Sort sort)

      fun typeCheckAst (AST (ast,sort),sort') = case (sort,sort') of
          (Int _,Int _) => true
        | (Bool _ , Bool _) => true
        | (T (name1,_), T (name2, _)) => name1 = name2
        | _ => (print ("Sort mismatch: "^(sortToString sort)
          ^" vs "^(sortToString sort')); false)

      fun astToZ3Ast (AST (z3_ast,sort)) = z3_ast

      fun astToString (AST (z3_ast,_)) =
        Z3_ast_to_string (ctx,z3_ast)

      fun sortOfAst (AST (_,sort)) = sort

      fun constToString ast = Z3_ast_to_string (ctx, astToZ3Ast ast)

      fun strucRelToString sr = raise (Fail "unimpl")

      fun mkEq (AST (x1,_),AST (x2,_)) = Z3_mk_eq (ctx,x1,x2)

      fun mkConst (name,sort) =
        let
          val z3_sort = sortToZ3Sort sort
          val const = Z3_mk_const (ctx, mkSym name, z3_sort)
          val _ = log $ "(declare-const "^(Z3_ast_to_string (ctx,const))
            ^" "^(Z3_sort_to_string (ctx,z3_sort))^")"
          val _ = log "\n"
        in
          AST (const,sort)
        end

      fun mkBoundVar ctx (index,sort) = 
        AST (Z3_mk_bound (ctx,index,sortToZ3Sort sort),sort)

      (*
       * Encoding Sets and Structural Relations. 
       * An n-arity set is eta-equivalent of an n-arity boolean 
       * Z3 function. Set s = \(x1,x2,..,xn).z3_f(x1,x2,...,xn)
       * Eg: s = s1 ∪ s2 is encoded as  
       * ∀x1,x2,..,xn, s(x1,x2,..,xn) = s1(x1,x2,..,xn) 
       *                              ∨ s2 (x1,x2,..,xn)
       * An n-arity structural relation is a curried version of
       * eta-equivalent of n-arity boolean Z3 function.
       * Relation Rmem = \x1.\(x2,...,xn).z3_f(x1,x2,...,xn)
       * Relation application will, therefore, produce a function
       * that accepts n-1 arguments and produces an n-arity set.
       * Eg: Rmem(l) = Rmem(l1) U Rmem(l2) is encoded as 
       * ∀x1,..,xn-1, Rmem(l,x1,..,xn-1) = Rmem(l1,x1,..,xn-1) 
       *                                 ∨ Rmem(l2,x1,..,xn-1)
       *)
      fun mkSet (name,sorts) =
        let
          val nargs = Vector.length sorts
          val z3_sorts = Vector.map (sorts, sortToZ3Sort)
          val func = Z3_mk_func_decl (ctx, mkSym name, nargs,
            z3_sorts, bool_sort)
          val _ = log $ Z3_func_decl_to_string (ctx,func)
          val _ = log "\n"
          val pred = fn asts => 
            let
              (* Following results in Size exception. Reason unknown. *)
              (*val astStr = fn _ =>(L.toString $ L.vector $ Vector.map 
                (asts, fn ast => L.str $ astToString ast))*)
              (*val sortStrs = List.map (Vector.toList sorts, fn s => 
                (sortToString s))
              val sortStr = fn _ => (List.fold (sortStrs, "(", fn (s,acc) =>
                acc^","^s))^")"*)
              val errMsg = (fn _ => "Type Mismatch. Set: "^name^".\n")
              val _ = assert (Vector.length asts = Vector.length sorts,
                errMsg ())
              val _ = assert (Vector.forall2 (asts,sorts,typeCheckAst),
                errMsg ())
              val z3_asts = Vector.map (asts,astToZ3Ast)
            in
              Z3_mk_app (ctx, func, nargs, z3_asts)
            end
        in
          Set {ty = sorts, pred = pred}
        end

      fun mkStrucRel (name,sorts) =
        let
          val nargs = Vector.length sorts
          val domainTy = Vector.sub (sorts,0)
          val Set {ty,pred} = mkSet (name,sorts)
          val rel = fn ast => 
            let
              val _ = assert (typeCheckAst (ast,domainTy),
                "Type error at app of relation "^name)
              (*
               * Constructing (n-1)-arity set from an n-arity
               * boolean function. 
               * n >= 2 invariant follows from structural relations.
               *)
              val ty' = Vector.dropPrefix (sorts,1)
              val pred' = fn asts => pred $ Vector.concat 
                [Vector.new1 ast, asts]
            in
              Set {ty = ty', pred = pred'}
            end
        in
          SR {ty=sorts, rel = rel}
        end

      fun mkStrucRelApp (SR {rel, ...}, ast) = rel ast

      fun mkSetUProp (indx, sorts : sort vector, propfn : ast vector
          -> (z3_pattern vector * z3_ast)) =
        let
          val numbvs = Vector.length sorts
          val (bvs,indx') = Vector.mapAndFold (sorts, indx, 
            fn (sort,i) => (mkBoundVar ctx (i,sort), i+1))
          (* 
           * De-brujin. Therefore: bv_n,bv_n-1,...,bv_0 
           * Z3 applies the convention that the last element in the
           * bvnames and bvtys array refers to the variable with
           * index 0, the second to last element of bvnames and
           * bvtys refers to the variable with index 1
           *)
          val bvnames = Vector.tabulate (numbvs, fn i => mkSym 
            ("bv"^(Int.toString (indx+numbvs-i-1))))
          val bvtys = Vector.rev $ Vector.map (sorts,sortToZ3Sort)
          val (patterns,prop) = propfn bvs
          val forall = Z3_mk_forall (ctx, 0, 
                        Vector.length patterns, 
                        patterns,
                        numbvs,
                        bvtys, 
                        bvnames, 
                        prop)
        in
          forall
        end

      fun mkSetProp (sorts : sort vector, propfn : ast vector -> 
        (z3_pattern vector * z3_ast)) =
          mkSetUProp (0, sorts, propfn)

      fun mkSetExProp (indx, sorts : sort vector, propfn : ast vector 
          -> (z3_pattern vector * z3_ast)) =
        let
          val numbvs = Vector.length sorts
          val (bvs,indx') = Vector.mapAndFold (sorts, indx, 
            fn (sort,i) => (mkBoundVar ctx (i,sort), i+1))
          (* 
           * De-brujin. Therefore: bv_n,bv_n-1,...,bv_0 
           *)
          val bvnames = Vector.tabulate (numbvs, fn i => mkSym 
            ("ev"^(Int.toString (numbvs-i-1))))
          val bvtys = Vector.rev $ Vector.map (sorts,sortToZ3Sort)
          val (patterns,prop) = propfn bvs
          val exists = Z3_mk_exists (ctx, 0, 
                        Vector.length patterns, 
                        patterns,
                        numbvs,
                        bvtys, 
                        bvnames, 
                        prop)
        in
          exists
        end

      fun dischargeAssertion asr = 
        let
          val _ = log $ "(assert "^(Z3_ast_to_string (ctx,asr))^")"
          val _ = log "\n"
        in
           Z3_assert_cnstr (ctx,asr)
        end

      fun assertSetProp (sorts,prop) =
        dischargeAssertion $ mkSetProp (sorts,prop)

      val mkNullSet = fn () => Null

      fun mkEmptySet sorts = 
        let
          val set as Set {ty,pred} = mkSet (genSetName (),sorts)
          val _ = assertSetProp (sorts, fn bvAsts =>
            let
              val fnapp = pred bvAsts
              val prop = Z3_mk_eq (ctx,fnapp,falsee)
              val pattern = Z3_mk_pattern (ctx,1,Vector.new1 fnapp)
            in
              (Vector.new1 pattern, prop)
            end)
        in
          set 
        end

      fun mkSingletonSet asts = 
        let
          val sorts = Vector.map (asts,sortOfAst)
          val set as Set {ty,pred} = mkSet (genSetName (),sorts)
          val _ = assertSetProp (sorts, fn bvAsts =>
            let
              val fnapp = pred bvAsts
              val eqs = Vector.map2 (bvAsts,asts,mkEq)
              val conj = Z3_mk_and (ctx,Vector.length eqs,eqs)
              val iff = Z3_mk_iff (ctx,fnapp,conj)
              val pattern = Z3_mk_pattern (ctx,1,Vector.new1 fnapp)
            in
              (Vector.new1 pattern, iff)
            end)
        in
          set
        end

      fun mkMultiPatterns multipatlist = Vector.fromList $ List.map 
        (multipatlist, fn terms => Z3_mk_pattern (ctx, List.length terms,
          Vector.fromList terms))

      fun mkSimplePatterns patlist = Vector.fromList $ List.map (patlist,
        fn pat => Z3_mk_pattern (ctx, 1, Vector.fromList [pat]))

      fun mkSetEqAssertion (s1,s2) = case (s1,s2) of 
          (Null,Null) => truee
        | (Null,Set {ty,...}) => mkSetEqAssertion (mkEmptySet ty, s2)
        | (Set {ty,...},Null) => mkSetEqAssertion (s1, mkEmptySet ty)
        | (Set {ty=sorts1,pred=pred1}, Set {ty=sorts2,pred=pred2}) => 
          (*
           * Pre-condition of sorts1 = sorts2 is automatically
           * checked when pred1 and pred2 are applied
           *)
          mkSetProp (sorts1, fn bvAsts => 
            let
              val fnapp1 = pred1 bvAsts
              val fnapp2 = pred2 bvAsts
              val iff = Z3_mk_iff (ctx,fnapp1,fnapp2)
              val pats = mkSimplePatterns [fnapp1,fnapp2]
            in
              (pats,iff)
            end)
       
      fun mkSubSetAssertion (s1,s2) = case (s1,s2) of 
          (Null,Null) => falsee
        | (Null,Set {ty,...}) => truee
        | (Set {ty,...},Null) => falsee
        | (Set {ty=sorts1,pred=pred1}, Set {ty=sorts2,pred=pred2}) => 
          (*
           * Pre-condition of sorts1 = sorts2 is automatically
           * checked when pred1 and pred2 are applied
           *)
          mkSetProp (sorts1, fn bvAsts => 
            let
              val fnapp1 = pred1 bvAsts
              val fnapp2 = pred2 bvAsts
              val implies = Z3_mk_implies (ctx,fnapp1,fnapp2)
              val pats = mkSimplePatterns [fnapp1,fnapp2]
            in
              (pats,implies)
            end)
       
      val mkUnion = fn (Null,s2) => s2 | (s1,Null) => s1 
        | (Set {ty=sorts1,pred=pred1}, Set {ty=sorts2,pred=pred2}) =>
          let
            val s as Set {ty,pred} = mkSet (genSetName (), sorts1)
            val _ = assertSetProp (ty, fn bvAsts =>
              let
                val fnapp = pred bvAsts
                val fnapp1 = pred1 bvAsts
                val fnapp2 = pred2 bvAsts
                val disj = Z3_mk_or (ctx,2,Vector.fromList 
                  [fnapp1,fnapp2])
                val iff = Z3_mk_iff (ctx,fnapp,disj)
                val pats = mkSimplePatterns [fnapp, fnapp1, fnapp2]
              in
                (pats, iff)
              end)
          in
            s
          end
       
      val mkCrossPrd = fn (Null,_) => Null | (_,Null) => Null 
        | (Set {ty=sorts1,pred=pred1}, Set {ty=sorts2,pred=pred2}) =>
          let
            val sorts = Vector.concat [sorts1,sorts2]
            val s as Set {ty,pred} = mkSet (genSetName (), sorts)
            val _ = assertSetProp (ty, fn bvAsts =>
              let
                val bvAsts1 = Vector.prefix (bvAsts,Vector.length 
                  sorts1)
                val bvAsts2 = Vector.dropPrefix (bvAsts, 
                  Vector.length sorts1)
                val fnapp = pred bvAsts
                val fnapp1 = pred1 bvAsts1
                val fnapp2 = pred2 bvAsts2
                val conj = Z3_mk_and (ctx,2,Vector.fromList 
                  [fnapp1,fnapp2])
                val iff = Z3_mk_iff (ctx,fnapp,conj)
                val pats = mkMultiPatterns [[fnapp], [fnapp1,fnapp2]]
              in
                (pats, iff)
              end)
          in
            s
          end

      val mkDiff = fn (Null,s2) => Null | (s1,Null) => s1 
        | (Set {ty=sorts1,pred=pred1}, Set {ty=sorts2,pred=pred2}) =>
          let
            val s as Set {ty,pred} = mkSet (genSetName (), sorts1)
            val _ = assertSetProp (ty, fn bvAsts =>
              let
                val fnapp = pred bvAsts
                val fnapp1 = pred1 bvAsts
                val fnapp2 = pred2 bvAsts
                val nfnapp2 = Z3_mk_not (ctx,fnapp2)
                val conj = Z3_mk_and (ctx,2,Vector.fromList 
                  [fnapp1,nfnapp2])
                val iff = Z3_mk_iff (ctx,fnapp,conj)
                val pats = mkSimplePatterns [fnapp, fnapp1, fnapp2]
              in
                (pats, iff)
              end)
          in
            s
          end

      val mkQCrossPrd = fn (sets) =>
          let
            val length = Vector.length
            val h = fn (x,y) => (x, Vector.map (y,length), 
              Vector.concatV y)
            val (dsorts,lens,rsorts) = h $ Vector.unzip $ Vector.map 
              (sets, fn (Set {ty,...}) => (Vector.sub (ty,0),
                Vector.dropPrefix (ty,1)))
            val sorts = Vector.concat [dsorts,rsorts]
            val s as Set {ty,pred} = mkSet (genSetName (), sorts)
            val _ = assertSetProp (ty, fn bvAsts =>
              let
                val dbvs = Vector.prefix (bvAsts, length dsorts)
                val rbvs = Vector.dropPrefix (bvAsts, length dsorts)
                val (bvss, _) = Vector.map2AndFold (dbvs,lens,rbvs,
                  fn (dbv,len,rest) => (Vector.concat [Vector.new1 dbv,
                      Vector.prefix (rest,len)], 
                    Vector.dropPrefix (rest, len)))
                val _ = assert (length sets = length bvss,
                  "mkQCrossPrd invariant failed")
                val fnapps = Vector.map2 (sets, bvss, 
                  fn (Set {pred,...},bvs) => pred bvs)
                val conj = Z3_mk_and (ctx,length fnapps, fnapps)
                val fnapp = pred bvAsts
                val iff = Z3_mk_iff (ctx,fnapp,conj)
                val pats = mkMultiPatterns [[fnapp], (Vector.toList
                  fnapps)]
              in
                (pats, iff)
              end)
          in
            s
          end

      fun mkQStrucRelApp (SR {ty,rel}) = Set {ty=ty, pred=
        fn bvs =>
          let
            val bv0 = Vector.sub (bvs,0)
            val bvs'= Vector.dropPrefix (bvs,1)
            val Set {pred, ...} = rel bv0
          in
            pred bvs'
          end}
      
      fun mkBind (Set {ty=ty1, pred=pred1},
          Set {ty=ty2, pred=pred2}) =
        let
          val length = Vector.length
          val srDomain = Vector.sub (ty1,0)
          val sorts = Vector.concat [Vector.new1 srDomain,
            ty2]
          val s as Set {ty,pred} = mkSet (genSetName (), sorts)
          val _ = assertSetProp (ty, fn bvAsts =>
            let
              val bvs1 = Vector.prefix (bvAsts,length ty1)
              val bvs2 = Vector.dropPrefix (bvAsts,1)
              val fnapp1 = pred1 bvs1
              val fnapp2 = pred2 bvs2
              val conj = Z3_mk_and (ctx,2,Vector.fromList 
                [fnapp1,fnapp2])
              val fnapp = pred bvAsts
              val iff = Z3_mk_iff (ctx,fnapp,conj)
              val pats = mkMultiPatterns [[fnapp], [fnapp1,
                fnapp2]]
            in
              (pats, iff)
            end)
        in
          s
        end
        | mkBind (Null,_) = Null | mkBind (_,Null) = Null

      fun assertBindIf (Set {ty=ty1, pred=pred1},
          Set {ty=ty2, pred=pred2}) =
        let
          val length = Vector.length
          val suffix = fn (l,n) => Vector.dropPrefix 
            (l, (length l)-n)
          val sorts = ty2
          val prop = mkSetProp (sorts, fn bvAsts =>
            let
              val rLen1 = (length ty1) - 1
              val bvs10 = Vector.sub (bvAsts,0)
              val bvs11 = suffix (bvAsts,rLen1)
              val bvs1 = Vector.concat [Vector.new1 bvs10, bvs11]
              val bvs2 = bvAsts
              val fnapp1 = pred1 bvs1
              val fnapp2 = pred2 bvs2
              val imp = Z3_mk_implies (ctx,fnapp2,fnapp1)
              val pats = mkMultiPatterns [[fnapp2, fnapp1]]
            in
              (pats, imp)
            end)
        in
          dischargeAssertion prop
        end
        | assertBindIf _ = raise (Fail "Bind eqn with null set")

      fun assertBindOnlyIf (Set {ty=ty1, pred=pred1},
          Set {ty=ty2, pred=pred2}) =
        let
          val length = Vector.length
          val usorts = ty1
          val dLen1 = 1
          val rLen1 = (length ty1) - dLen1
          val exsorts = Vector.dropSuffix(Vector.dropPrefix 
            (ty2,dLen1),rLen1)
          val exi = 0
          val ui = length exsorts
          fun expropfn (ubvs : ast vector, exbvs:ast vector) =
            let
              val fnapp1 = pred1 ubvs
              val allbvs = Vector.concat [
                (Vector.new1 (Vector.sub (ubvs,0))),
                exbvs,
                Vector.dropPrefix (ubvs,1)]
              val fnapp2 = pred2 allbvs
              val imp = Z3_mk_implies (ctx,fnapp1,fnapp2)
              val pats = mkMultiPatterns [[fnapp1, fnapp2]]
            in
              (pats,imp)
            end
          val uprop = mkSetUProp (ui,usorts, fn ubvs =>
            let
              val fnapp1 = pred1 ubvs
              val exprop = mkSetExProp (exi, exsorts, 
                fn exbvs =>
                  let
                    (*
                    val _ = print "Ex bvs are: "
                    val _ = print $ Vector.toString astToString exbvs
                    val _ = print "\n fnapp1 is: "
                    val _ = print $ Z3_ast_to_string (ctx,fnapp1)
                    val _ = print "\n"
                    *)
                  in
                    expropfn (ubvs,exbvs)
                  end)
              val pats = mkSimplePatterns []
            in
              (pats, exprop)
            end)
        in
          dischargeAssertion uprop
        end
        | assertBindOnlyIf _ = raise (Fail "Bind eqn with null set")

      fun assertBindEq x = (assertBindIf x ; assertBindOnlyIf x)

      fun mkNot asr = Z3_mk_not (ctx, asr) 

      fun mkIf (asr1,asr2) = Z3_mk_implies (ctx, asr1, asr2) 

      fun mkIff (asr1,asr2) = Z3_mk_iff (ctx, asr1, asr2) 

      fun mkAnd asrv = Z3_mk_and (ctx, Vector.length asrv, asrv) 

      fun mkOr asrv = Z3_mk_or (ctx, Vector.length asrv, asrv) 


      fun mkConstEqAssertion (ast1 as AST (x1,s1), AST (x2,s2)) = 
        (typeCheckAst (ast1,s2); Z3_mk_eq (ctx,x1,x2))

      fun mkInt i = AST (Z3_mk_int (ctx, i, int_sort), Int int_sort)
    in
      {
        bool_sort = Bool bool_sort,
        int_sort = Int int_sort,
        const_false = AST (falsee, Bool bool_sort),
        const_true = AST (truee, Bool bool_sort),
        truee = truee,
        falsee = falsee,
        sortToString = sortToString,
        constToString = constToString,
        strucRelToString = strucRelToString,
        mkUninterpretedSort = mkUninterpretedSort,
        mkConst = mkConst,
        mkInt = mkInt,
        mkStrucRel = mkStrucRel,
        mkStrucRelApp = mkStrucRelApp,
        mkNullSet = mkNullSet,
        mkSingletonSet = mkSingletonSet,
        mkUnion = mkUnion,
        mkCrossPrd = mkCrossPrd,
        mkDiff = mkDiff,
        mkQStrucRelApp = mkQStrucRelApp,
        mkQCrossPrd = mkQCrossPrd,
        mkBind = mkBind,
        assertBindEq = assertBindEq,
        mkSetEqAssertion = mkSetEqAssertion,
        mkSubSetAssertion = mkSubSetAssertion,
        mkConstEqAssertion = mkConstEqAssertion,
        mkNot = mkNot,
        mkIf = mkIf,
        mkIff = mkIff,
        mkAnd = mkAnd,
        mkOr = mkOr,
        dischargeAssertion = dischargeAssertion
       }
    end
end

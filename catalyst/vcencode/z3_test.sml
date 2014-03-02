open Z3_FFI;

fun $ (f,arg) = f arg
infixr 5 $

fun simple_example () = 
  let
    val cfg = Z3_mk_config ()
    val ctx = Z3_mk_context cfg
    val str = Z3_context_to_string ctx
  in
    print str;
    print "done"
  end;

fun demorgan () =
  let
    val cfg                = Z3_mk_config ()
    val ctx                = Z3_mk_context cfg
    val _                  = Z3_del_config cfg
    val bool_sort          = Z3_mk_bool_sort ctx
    val symbol_x           = Z3_mk_int_symbol (ctx, 0)
    val symbol_y           = Z3_mk_int_symbol (ctx, 1)
    val x                  = Z3_mk_const(ctx, symbol_x, bool_sort)
    val y                  = Z3_mk_const(ctx, symbol_y, bool_sort)
    (* De Morgan - with a negation around *)
    (* !(!(x && y) <-> (!x || !y)) *)
    val not_x              = Z3_mk_not(ctx, x)
    val not_y              = Z3_mk_not(ctx, y)
    val args               = Array.fromList [x,y]
    val x_and_y            = Z3_mk_and(ctx, 2, args)
    val ls                 = Z3_mk_not(ctx, x_and_y)
    val args               = Array.fromList [not_x,not_y]
    val rs                 = Z3_mk_or(ctx, 2, args)
    val conjecture         = Z3_mk_iff(ctx, ls, rs)
    val negated_conjecture = Z3_mk_not(ctx, conjecture)
    val _ = Z3_assert_cnstr(ctx, negated_conjecture)
    val res = Z3_check ctx
    val _ = Z3_del_context ctx
  in
    case res of
      ~1 => print "DeMorgan is valid\n"
    | 0 => print "Undef\n"
    | 1 => print "Demorgan is invalid"
    | _ => raise (Fail "z3_lbool expected. Got something else.")
  end;

fun declareSet ctx (name,elem_sort) =
  let
    val sym = Z3_mk_string_symbol (ctx,name)
    val bool_sort = Z3_mk_bool_sort ctx
  in
      Z3_mk_func_decl (ctx, sym, 1,
        Array.fromList [elem_sort], bool_sort)
  end

fun declareSets ctx (names,ty) = 
  List.map (names, fn (name) => declareSet ctx (name,ty))

fun mkBoundVar ctx (index,ty) =
  Z3_mk_bound (ctx,index,ty)

fun assertSetProp (ctx, numbvs, elty, propfn) =
  let
    val bvs = Array.tabulate (numbvs, fn i => mkBoundVar ctx (i,elty))
    (* 
     * De-brujin. Therefore: bv_n,bv_n-1,...,bv_0 
     *)
    val bvnames = Array.tabulate (numbvs, fn i => Z3_mk_string_symbol 
      (ctx,"bv"^(Int.toString (numbvs-i-1))))
    val bvtys = Array.tabulate (numbvs, fn i => elty)
    val (patterns,prop) = propfn (ctx,bvs)
    val forall = Z3_mk_forall (ctx, 0, 
                  Array.length patterns, 
                  patterns,
                  numbvs,
                  bvtys, 
                  bvnames, 
                  prop)
    (*val _ = print $ "(assert "^(Z3_ast_to_string (ctx,forall))^")"
    val _ = print "\n"*)
  in
    Z3_assert_cnstr (ctx, forall)
  end

fun assertNegSetProp (ctx, numbvs, elty, propfn) =
  let
    val bvs = Array.tabulate (numbvs, fn i => mkBoundVar ctx (i,elty))
    (* 
     * De-brujin. Therefore: bv_n,bv_n-1,...,bv_0 
     *)
    val bvnames = Array.tabulate (numbvs, fn i => Z3_mk_string_symbol 
      (ctx,"bv"^(Int.toString (numbvs-i-1))))
    val bvtys = Array.tabulate (numbvs, fn i => elty)
    val (patterns,prop) = propfn (ctx,bvs)
    val forall = Z3_mk_forall (ctx, 0, 
                  Array.length patterns, 
                  patterns,
                  numbvs,
                  bvtys, 
                  bvnames, 
                  prop)
    val neg = Z3_mk_not (ctx,forall)
    (*val _ = print $ "(assert "^(Z3_ast_to_string (ctx,neg))^")"
    val _ = print "\n"*)
  in
    Z3_assert_cnstr (ctx, neg)
  end

fun assertEmpty (ctx, set, elty) =
  assertSetProp (ctx, 1, elty, fn (ctx,bvs) =>
    let
      val bv = Array.sub (bvs,0)
      val fnapp = Z3_mk_app (ctx, set, 1, bvs)
      val flse = Z3_mk_false ctx
      val prop = Z3_mk_eq (ctx,fnapp,flse)
      val pattern = Z3_mk_pattern (ctx,1,Array.fromList [fnapp])
    in
      (Array.fromList [pattern], prop)
    end)

fun assertSingleton (ctx, set, el : z3_ast, elty) =
  assertSetProp (ctx, 1, elty, fn (ctx,bvs) =>
    let
      val bv = Array.sub (bvs,0)
      val fnapp = Z3_mk_app (ctx, set, 1, bvs)
      val eq = Z3_mk_eq (ctx,bv,el)
      val iff = Z3_mk_iff (ctx,eq,fnapp)
      val pattern = Z3_mk_pattern (ctx,1,Array.fromList [fnapp])
    in
      (Array.fromList [pattern], iff)
    end)

fun assertUnion (ctx, set, set1, set2, elty) = 
  assertSetProp (ctx, 1, elty, fn (ctx,bvs) =>
    let
      val fnapp = Z3_mk_app (ctx, set, 1, bvs)
      val fnapp1 = Z3_mk_app (ctx, set1, 1, bvs)
      val fnapp2 = Z3_mk_app (ctx, set2, 1, bvs)
      val disj = Z3_mk_or (ctx,2,Array.fromList [fnapp1,fnapp2])
      val iff = Z3_mk_iff (ctx,fnapp,disj)
      val mk_pat = fn ast => Z3_mk_pattern (ctx,1,Array.fromList [ast])
      val pats = Array.fromList $ List.map ([fnapp,fnapp1,fnapp2],mk_pat)
    in
      (pats, iff)
    end)

fun assertCrossPrd (ctx, set, set1, set2, tuplety, pairfn, elty) =
  assertSetProp (ctx, 2, elty, fn (ctx,bvs) =>
    let
      val bvpair = Z3_mk_app (ctx,pairfn,2,bvs)
      val bv1 = Array.sub(bvs,0)
      val bv2 = Array.sub(bvs,1)
      val fnapp = Z3_mk_app (ctx, set, 1, Array.fromList [bvpair])
      val fnapp1 = Z3_mk_app (ctx, set1, 1, Array.fromList [bv1])
      val fnapp2 = Z3_mk_app (ctx, set2, 1, Array.fromList [bv2])
      val conj = Z3_mk_and(ctx,2,Array.fromList [fnapp1,fnapp2])
      val iff = Z3_mk_iff (ctx,fnapp,conj)
      val mk_pat = fn ast => Z3_mk_pattern (ctx,1,Array.fromList [ast])
      val pats = Array.fromList $ List.map ([fnapp],mk_pat)
    in
      (pats, iff)
    end)

fun assertSetEq (ctx, set1, set2, elty) = 
  assertSetProp (ctx, 1, elty, fn (ctx,bvs) =>
    let
      val fnapp1 = Z3_mk_app (ctx, set1, 1, bvs)
      val fnapp2 = Z3_mk_app (ctx, set2, 1, bvs)
      val iff = Z3_mk_iff (ctx,fnapp1,fnapp2)
      val mk_pat = fn ast => Z3_mk_pattern (ctx,1,Array.fromList [ast])
      val pats = Array.fromList $ List.map ([fnapp1,fnapp2],mk_pat)
    in
      (pats, iff)
    end)

fun assertSetNotEq (ctx, set1, set2, elty) = 
  assertNegSetProp (ctx, 1, elty, fn (ctx,bvs) =>
    let
      val fnapp1 = Z3_mk_app (ctx, set1, 1, bvs)
      val fnapp2 = Z3_mk_app (ctx, set2, 1, bvs)
      val iff = Z3_mk_iff (ctx,fnapp1,fnapp2)
      val mk_pat = fn ast => Z3_mk_pattern (ctx,1,Array.fromList [ast])
      val pats = Array.fromList $ List.map ([fnapp1,fnapp2],mk_pat)
    in
      (pats, iff)
    end)

fun declareTupleSort ctx elty = 
  let
    val bool_sort          = Z3_mk_bool_sort ctx
    val mkSym              = fn str => Z3_mk_string_symbol (ctx,str)
    val pair_sym           = mkSym "Pair"
    val recog_sym          = mkSym "is_pair"
    val first_sym          = mkSym "first"
    val second_sym         = mkSym "second"
    val fld_names          = Array.fromList [first_sym,second_sym]
    val mkpair_sym         = mkSym "mkpair"
    val sorts              = Array.fromList [elty,elty]
    val sort_refs          = Array.fromList [0,0]
    val pair_cons          = Z3_mk_constructor (ctx, mkpair_sym,
                              recog_sym,2,fld_names,sorts,sort_refs)
    val tuple_ty           = Z3_mk_datatype (ctx, pair_sym, 1, 
                              Array.fromList [pair_cons])
    val pair_fn            = ref $ Z3_mk_func_decl (ctx, mkSym "mk-pair",
                              2, sorts, tuple_ty)
    val is_pair_fn         = ref $ Z3_mk_func_decl (ctx, mkSym "is_pair_decl",
                              1, Array.fromList [tuple_ty], bool_sort)
    val first_fn           = Z3_mk_func_decl (ctx, mkSym "first_decl",
                              1, Array.fromList [tuple_ty], elty)
    val second_fn          = Z3_mk_func_decl (ctx, mkSym "second_decl",
                              1, Array.fromList [tuple_ty], elty)
    val proj_decls         = Array.fromList [first_fn, second_fn]
    val _                  = Z3_query_constructor (ctx, pair_cons, 2, 
                              pair_fn, is_pair_fn, proj_decls)
    val first_fn           = Array.sub (proj_decls,0)
    val second_fn          = Array.sub (proj_decls,1)
  in
    (tuple_ty, !pair_fn)
  end

fun reverse_proof () =
  let
    val cfg                = Z3_mk_config ()
    val ctx                = Z3_mk_context cfg
    val _                  = Z3_del_config cfg
    (* functions in this context *)
    val declareSet         = declareSet ctx
    val declareSets        = declareSets ctx
    val t_sym              = Z3_mk_string_symbol (ctx,"T")
    val t_sort             = Z3_mk_uninterpreted_sort (ctx,t_sym)
    val [rmemxs, rmeml,
        rmemxs1, rmemv,
        rmemx1]            = declareSets (["Rmemxs","Rmeml",
                              "Rmemxs1", "Rmemv","Rmemx1"],t_sort)
    (* Declare constants *)
    val x_sym              = Z3_mk_string_symbol (ctx,"x")
    val x                  = Z3_mk_const (ctx,x_sym,t_sort)
    (* Declare tuple type *) 
    val (tuple_sort,pair_cons) = declareTupleSort ctx t_sort
    val [robsxs, robsl,
         roasxs1, roasx1,
         roasv]            = declareSets (["Robsxs", "Robsl", "Roasxs1", 
                                "Roasx1", "Roasv"],tuple_sort)
    val setx               = declareSet ("setx",t_sort)
    val setc               = declareSet ("setc",tuple_sort)
    val _                  = assertSingleton (ctx,setx,x,t_sort)
    val _                  = assertUnion (ctx,rmeml,setx,rmemxs,t_sort)
    val _                  = assertCrossPrd (ctx,setc,setx,rmemxs,
                              tuple_sort, pair_cons, t_sort)
    val _                  = assertUnion (ctx,robsl,setc,robsxs,tuple_sort)
    val _                  = assertSetEq (ctx,rmemxs,rmemxs1,t_sort)
    val _                  = assertSetEq (ctx,robsxs,roasxs1,tuple_sort)
    val _                  = assertSingleton (ctx,rmemx1,x,t_sort)
    val _                  = assertEmpty (ctx,roasx1,tuple_sort)
    val _                  = assertUnion (ctx,rmemv,rmemxs1,rmemx1,t_sort)
    val seta               = declareSet ("seta",tuple_sort)
    val _                  = assertUnion (ctx,seta,roasxs1,roasx1,tuple_sort)
    val setb               = declareSet ("setb",tuple_sort)
    val _                  = assertCrossPrd (ctx,setb,rmemx1,rmemxs1,
                              tuple_sort, pair_cons, t_sort)
    val _                  = assertUnion (ctx,roasv,seta,setb,tuple_sort)
    (*val _                  = assertSetNotEq (ctx,rmemv,rmeml,t_sort)*)
    val _                  = assertSetNotEq (ctx,roasv,robsl,tuple_sort)
    val res                = Z3_check ctx
    val _                  = Z3_del_context ctx
  in
    case res of
      ~1 => print "Reverse is typechecked\n"
    | 0 => print "Undef\n"
    | 1 => print "Reverse typechecking failed\n"
    | _ => raise (Fail "z3_lbool expected. Got something else.")
  end;

fun main () = 
  let 
    val _ = Z3_open_log ("z3.log")
    (*val _ = simple_example ()*)
    val _ = demorgan ()
    val _ = reverse_proof ()
    val _ = Z3_close_log ()
  in
    ()
  end;

main ();

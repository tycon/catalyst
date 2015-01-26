functor Runner (S : RUNNER_STRUCTS) : RUNNER = 
struct
  open S
  structure SpecLang = VE.SpecLang
  open SpecLang
  structure RI = RelLang.RelId
  structure TyD = TypeDesc
  structure RefTy = RefinementType
  structure RefTyS = RefinementTypeScheme
  structure P = Predicate
  structure BP = Predicate.BasePredicate
  structure RP = Predicate.RelPredicate
  structure L = Layout
  open CoreML

  val assert = Control.assert
  fun $ (f,arg) = f arg
  infixr 5 $

  val inputFile = ref "bogus"
  fun setInputFile str = (inputFile := str)
  val generatorPrefix = "catalystGen"
  val sampleFilePath = "samples/sample.sml"
  fun writeSample str = File.withOut (sampleFilePath, 
    fn outS => Out.output (outS,str))

  fun isGeneratorName s = 
    let
      val itIs = String.hasPrefix (s,{prefix=generatorPrefix})
      (*
      val _ = print $ s^" is "^(if itIs then "" else "NOT")
        ^" a generator\n"
      *)
    in
       itIs
    end

  fun isGeneratorVar v = isGeneratorName $ Var.toString v

  fun fromJust (SOME x) = x
    | fromJust (NONE) = Error.bug ("Did not expect NONE\n")

  fun isGeneratorDec (Dec.Fun {decs,...}) = Vector.exists (decs,
      fn ({var,...}) => isGeneratorVar var)
    | isGeneratorDec _ = false

  fun argTyDOfLam lam = Type.toMyType $ #argType $ Lambda.dest lam

  structure GM = ApplicativeMap (structure Key = 
                                 struct
                                  open TyD
                                  val equal = unifiable
                                 end
                                 structure Value = Var)

  fun readPrelude () =
    let
      val lines = File.withIn ("runner/prelude.sml.txt", fn inS =>
        In.inputAll inS)
    in
      lines
    end

  fun vecToCommaStr f vec = String.concatWith 
    (Vector.toListMap (vec,f),",")

  fun vecToNewLineStr f vec = String.concatWith 
    (Vector.toListMap (vec,f),"\n")

  fun listToNewLineStr f vec = String.concatWith 
    (List.map (vec,f),"\n")

  fun listToCommaStr f lst = String.concatWith 
    (List.map (lst,f),",")

  fun listToSpcStr f lst = String.concatWith 
    (List.map (lst,f)," ")

  fun monoToPolyRIStr relIdStr = String.keepAll (relIdStr,
    Char.isAlpha)

  fun relIdToVarName relId = String.toLower $ 
      monoToPolyRIStr $ RI.toString relId

  fun firstCharCaps str = 
    let
      val x::xs = String.explode str
      val x' = Char.toUpper x
    in
      String.implode $ x'::xs
    end

  fun indentBy n str = (String.tabulate (n, fn _ => Char.space))^str

  fun relExpToSMLString re = let open RelLang in case re of
      T elvec => "singleton ["^(vecToCommaStr elemToString elvec)^"]"
    | X (re1,re2) => "crossprd ("^(listToCommaStr 
          relExpToSMLString [re1, re2])^")"
    | U (re1,re2) => "union ["^(listToCommaStr 
          relExpToSMLString [re1, re2])^"]"
    | D (re1,re2) => "minus ("^(listToCommaStr 
          relExpToSMLString [re1, re2])^")"
    | R (relId,v) => (monoToPolyRIStr $ RI.toString relId)
          ^"("^(Var.toString v)^")"
  end

  fun relMapToPMSeq map = Vector.map (map, 
    fn (cons,argOp,re) =>
      let
        val conStr = Con.toString cons
        val argStr = case argOp of NONE => ""
          | SOME args => "("^String.concatWith 
            (Vector.toListMap (args, Var.toString), ",")^")"
        val conAppStr = case (conStr,argOp) of 
            ("cons",SOME args) => 
            let
              val _ = assert (Vector.length args = 2, "Impossible\
                \ case\n")
              val [xStr,xsStr] = Vector.toListMap (args,Var.toString)
            in
              xStr^"::"^xsStr
            end
          | ("nil",None) => "[]"
          | _ => conStr^argStr
        val reStr = relExpToSMLString re
      in
        conAppStr^" => "^reStr
      end)
  
  exception DummyRelRet
  fun strucRelToSMLFn (relId,{ty,map} ) = 
    let
      val relName = RI.toString relId
      val _ = if relName = "qRm" orelse relName = "qRo" 
        then raise DummyRelRet else ()
      val funStr = "fun "^relName^" l = case l of\n"
      val patMatchSeq = relMapToPMSeq map
      val firstPM::restPMs = Vector.toList patMatchSeq
      val pmStr = (List.fold (restPMs,"    "^firstPM^"\n",
        fn (pm,acc) => acc^"  | "^pm^"\n"))
    in
      funStr^pmStr
    end handle DummyRelRet => ""

  (*
   * Simple representation of a relpred of form R(x) = e1 U .. U en.
   * Note that this hole_rp is different from hole_rp in vcencode.
   *)
  datatype hole_rp = RPEq of {lhsRel: RI.t, 
                              lhsVar: Var.t,
                              rhsExprs: RelLang.expr list}
  fun toHoleRP (RP.Eq (RelLang.R (relId,v), rhsExpr)) =
    let
      fun foldUnion (RelLang.U (e1,e2)) = List.concat 
        [foldUnion e1, foldUnion e2]
        | foldUnion e =  [e]
      val rhsExprs = foldUnion rhsExpr
    in
      RPEq {lhsRel = relId, lhsVar = v, rhsExprs=rhsExprs}
    end
    | toHoleRP _ = raise (Fail "Hole RP structure is not as \
        \ expected.")

  fun fromHoleRP (RPEq {lhsRel, lhsVar, rhsExprs}) = 
    let
      open RelLang
      val lhsre = app (lhsRel,lhsVar)
      val rhsre = List.fold (rhsExprs, emptyexpr (), union)
    in
      RP.Eq (lhsre,rhsre)
    end

  fun unwrapLambda lam = 
    let 
      val {arg,body,...} = Lambda.dest lam
      val bodyNode = #1 $ Exp.dest body
    in
      case bodyNode of 
        Exp.Lambda lam' => (fn (args',exp) => (arg::args',exp))
          (unwrapLambda lam')
      | _ => ([arg],body)
    end

  fun refineHole userDecs ve re (holeId,relpreds) = 
    let
      (*
       * relpreds -> holeRPs
       *)
      val holeRPs = Vector.map (relpreds, toHoleRP)
      (*
       * First, extract all generators from userDecs
       *)
      val generatorDecs = Vector.keepAll (userDecs, isGeneratorDec)
      (*
      val generatorLyts = Vector.map (generatorDecs, Dec.layout)
      val generatorStr = Vector.fold (generatorLyts, "", 
        fn (genLyt, acc) => (L.toString genLyt)^"\n"^acc )
      *)
      (*
       * Make a map of tyd to corresponding generator function.
       *)
      val genMap = Vector.fold (generatorDecs, GM.empty, 
        fn (Dec.Fun {decs, ...}, gm) => Vector.fold (decs, gm,
          fn ({var,lambda},gm) => if isGeneratorVar var
            then GM.add gm (argTyDOfLam lambda) var else gm))
      (*
       * Second, convert all structural rels in RE to SML strings.
       *)
      val relDecStrs = Vector.map (RE.toVector re, 
        strucRelToSMLFn)
      val relDecsStr = Vector.foldr (relDecStrs, "", 
        fn (relDecStr, acc) => acc^"\n"^relDecStr)
      (*
       * For each disjunct in the RHS of relpred, we need a boolean
       * selector to describe if the disjunct is present in the
       * invariant or not, based on actual observed IO values.
       *)
      fun selDecStr selName n = "val "^selName^" = \
        \ref (tabulate ("^(Int.toString n)^", fn _ => true))"
      val (selNames, selDecStrs) = Vector.unzip $ Vector.map (holeRPs, 
        fn (RPEq {lhsRel, rhsExprs, ...}) => 
          let
            val selName = (relIdToVarName lhsRel)^"Sels"
            val selDecStr = selDecStr selName
              (List.length rhsExprs)
          in
            (selName,selDecStr)
          end)
      val selDecsStr = Vector.fold (selDecStrs, "", 
        fn (selDecStr,acc) => acc^"\n"^selDecStr)
      (*
       * fvar is the function whose unknown invariant (post-condition)
       * is represented using a hole with id = holeId.
       *)
      val (fvar,refTyS) = fromJust $ Vector.peek 
        (VE.toVector ve, fn (v,refTyS) => 
          RefTyS.invIsHoleWithId refTyS holeId)
      val flam = fromJust $ Vector.peekMap (userDecs, 
        fn (Dec.Fun {decs, ...}) => Vector.peekMap (decs,
          fn ({lambda,var}) => if varStrEq (var,fvar) 
            then SOME lambda else NONE)
          | _ => NONE)
      val fRefTy = RefTyS.specialize refTyS
      val argVarTyDs = RefTy.getArgVarTyDs fRefTy
      val argVars = List.map (argVarTyDs, fn (v,_) => v)
      val argsTupStr = "("^ (listToCommaStr Var.toString 
        argVars)^")"
      val argsSpcStr = listToSpcStr Var.toString argVars
      (*
       * Each disjunct in RHS is a relational abstraction of one of
       * the inputs of f. We need a function to calculate the value
       * of a disjunct, given all the inputs.
       *)
      fun disjLamStr disjre = 
        let
          val bodyStr = relExpToSMLString disjre
        in
          "fn "^argsTupStr^" => "^bodyStr
        end
      fun disjLamsListStr relExps = "["^(listToCommaStr disjLamStr 
        relExps)^"]"
      fun candDecStr candName relExps = "val "^candName^" = "
        ^(disjLamsListStr relExps)
      val (candNames, candDecStrs) = Vector.unzip $ Vector.map (holeRPs, 
        fn (RPEq {lhsRel, rhsExprs, ...}) => 
          let
            val candName = (relIdToVarName lhsRel)^"Cand"
          in
            (candName, candDecStr candName rhsExprs)
          end)
      val candDecsStr = vecToNewLineStr (fn x => x) candDecStrs
      (*
       * Create an instrumented version of f (called instrf). instrf
       * takes argVars, calls f with argVars to obtain retVar.
       *)
      val fname = Var.toString fvar
      val instrfName = "instr"^(firstCharCaps fname)
      val instrfStr =
        let
          val instrfHdr = "fun "^instrfName^" "^argsSpcStr^" = "
          val retVarStr = "l_"
          val fCallDecStr = "    val "^retVarStr^" = "^fname^" "
            ^argsSpcStr
          fun relAppDecStr relId = 
            let
              val relStr = monoToPolyRIStr $ RI.toString relId
              val lhsVarStr = (String.toLower relStr)^retVarStr
              val rhsExpr = relStr^"("^retVarStr^")"
            in
              (lhsVarStr, "val "^lhsVarStr^" = "^rhsExpr)
            end  
          val (relAppVarNames, relAppDecStrs) = Vector.unzip $ 
            Vector.map (holeRPs, 
              fn (RPEq {lhsRel, ...}) => relAppDecStr lhsRel)
          val relAppDecsStr = vecToNewLineStr (indentBy 4)
            relAppDecStrs
          fun selUpdDecStr (selName, candName, relAppVarName) = 
            let
              val mapfStr = "fn (s,c) => s andalso subEq (c"
                ^argsTupStr^", "^relAppVarName^")"
              val map2Str = "map2 (!"^selName^", "^candName^", "
                ^mapfStr^")"
              val updStr = selName^" := "^map2Str
            in
              "val _ = "^updStr
            end
          val selUpdDecStrs = Vector.map3 (selNames, candNames,
            relAppVarNames, selUpdDecStr)
          val selUpdDecsStr = vecToNewLineStr (indentBy 4)
            selUpdDecStrs
          val instrfBody = listToNewLineStr (fn x => x) 
            ["  let",
             fCallDecStr, 
             relAppDecsStr, 
             selUpdDecsStr,
             "  in", 
             "    "^retVarStr, 
             "  end"]
        in
          instrfHdr^"\n"^instrfBody^"\n"
        end
      (*
       * The actual definition of f. It now needs to be mutually
       * recursive with instrf.
       *)
      val fStr = 
        let
          val (fArgVars,fBody) = unwrapLambda flam
          val fArgsSpcStr = listToSpcStr (Var.toString) fArgVars
          val fHdrStr = "and "^fname^" "^fArgsSpcStr^" = "
          val fBodyStr = Layout.toString $ Exp.layoutSML fBody
        in
          fHdrStr^"\n"^fBodyStr^"\n"
        end
      (*
       * Create a main function that calls the instrf with appropriate
       * arguments.
       *)
      val mainfStr = 
        let
          val mainfHdr = "fun main _ = "
          (*
           * Foreach (v,tyd) in argVarTyds, if generate dec:
           * "val v = catalystGenTyd randInt()"
           *)
          fun inputDecStr (v,tyd) = "    val "^(Var.toString v)^" = "
            ^(Var.toString $ GM.find genMap tyd)^" 2"
          val inputDecsStr = listToNewLineStr inputDecStr argVarTyDs
          val instrfCallDec = "    val _ = "^instrfName^" "^argsSpcStr
          fun selLogStr selName = listToNewLineStr (indentBy 4) 
            [
              "val _ = List.map (fn sel => if sel then log \"1\" \
                \else log \"0\") (!" ^selName^")", 
              "val _ = print \"\\n\""
            ]
          val selLogsStr = vecToNewLineStr selLogStr selNames
          val mainfBody = listToNewLineStr (fn x => x)
            [
              "  let",
              inputDecsStr,
              instrfCallDec,
              selLogsStr,
              "  in",
              "    ()",
              "  end"
            ]
        in
          mainfHdr^"\n"^mainfBody
        end
      val preStr = readPrelude ()
      (*val _ = print "\nSML:\n"*)
      val _ = writeSample $ preStr ^ relDecsStr ^ selDecsStr 
                          ^ candDecsStr ^ "\n\n" ^ instrfStr 
                          ^ "\n" ^fStr ^ "\n" ^ mainfStr ^ "\n"
      (*
       * Compile the sample.
       *)
      val _ = Process.call' ("mlton", ["samples/sample.mlb"])
      (*
       * Run the sample
       *)
      val sampleOut = Process.callWithIn ("./samples/sample", [],
        fn inS => In.inputAll inS)
      (*
      val _ = print "Here is the sample output:\n"
      val _ = print sampleOut
      val _ = print "\n"
      *)
      val llen = List.length
      val vlen = Vector.length
      val lines = List.keepAll (String.split (sampleOut,#"\n"),
        fn line => not (line = ""))
      val (nlines, npreds) = (llen lines, vlen holeRPs)
      val _ = assert (nlines = npreds, "Sample run output\
        \ failed sanity check. nlines="^(Int.toString nlines)
        ^". npreds="^(Int.toString npreds)^".\n")
      val holeRPs' = Vector.map2 (holeRPs, Vector.fromList lines, 
        fn (RPEq {lhsRel, lhsVar, rhsExprs}, line) => 
          let
            val _ = print $ "Processing line: "^line^".\n"
            val flags = List.map (String.explode $ 
              String.deleteSurroundingWhitespace line,
              fn c => fromJust $ Char.digitToInt c)
            val (nflags, ndisjuncts) = (llen flags, llen rhsExprs)
            val _ = assert (nflags = ndisjuncts, "Sample run output\
              \ failed sanity check. nflags = "^(Int.toString nflags)
              ^". ndisjuncts = "^(Int.toString ndisjuncts)^".\n")
            val (_,rhsExprs') = List.unzip $ List.keepAll (
              List.zip (flags,rhsExprs), fn (flag,_) => flag=1)
            val _ = print $ (Int.toString $ llen rhsExprs')^
                  " RHSExprs remain for "^(RI.toString lhsRel)^"\n"
          in
            RPEq {lhsRel=lhsRel, lhsVar=lhsVar, rhsExprs=rhsExprs'}
          end)
      val relpreds' = Vector.map (holeRPs', fromHoleRP)
    in
      (holeId,relpreds')
    end

  fun refineHM userDecs ve re hm = HM.fromVector $ 
    Vector.map (HM.toVector hm, refineHole userDecs ve re)
end

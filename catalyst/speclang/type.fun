functor TypeDesc (S: TYPE_DESC_STRUCTS): TYPE_DESC = 
	struct
	
		open S
	
		datatype t =
			  	Tunknown
			  | Tvar of Tyvar.t
			  | Tarrow of t * t
			  | Ttuple of t list
			  | Tconstr of Tycon.t * t list
			  | Tfield of string * t
			  
	 	fun makeTarrow (tdesc1, tdesc2) = Tarrow (tdesc1, tdesc2)
	 	fun makeTconstr (cons, tdlist) = Tconstr (cons, tdlist)
	 	fun makeTvar tvar = Tvar tvar
	 	fun makeTfield (str, tdesc1) = Tfield (str, tdesc1)
	 	fun makeTtuple tdlist = Ttuple tdlist
	 	fun makeTunknown () = Tunknown
    fun visitType t = case t of
        Tunknown => "Tunknown"
      | Tvar v  => "Tvar ("^(Tyvar.toString v)^")"
      | Tarrow (t1,t2)=> "Tarrow ("^(visitType t1)^","^(visitType t2)^")"
      | Ttuple tdl => "Ttuple (["^(List.fold (tdl,"",(fn(t,s)=>(s^","^(visitType t)))))^"])"
      | Tconstr (tc,tdl) => "Tconstr("^(Tycon.toString tc)^","^"["^
                            (List.fold (tdl,"",(fn(t,s)=>(s^","^(visitType t)))))^"])"
      | Tfield (s,td) => "Tfield ("^s^","^(visitType td)^")"
    fun sametype (t1,t2) = 
      let 
        val _ = print ("Asked if "^(visitType t1)^" and "^(visitType t2)^" are sametypes\n")
        fun sametypes (tl1,tl2) = (List.length tl1 = List.length tl2) andalso 
          List.fold2 (tl1,tl2,true, fn(t1,t2,flag) => (flag andalso sametype (t1,t2)))
      in
      case (t1,t2) of
          (Tunknown,Tunknown) => true
        | (Tvar v1, Tvar v2 ) => Tyvar.equals (v1,v2)
        | (Tarrow (tda1,tdr1), Tarrow (tda2,tdr2)) => 
            (sametype (tda1,tda2)) andalso
            (sametype (tdr1,tdr2))
        | (Ttuple td1, Ttuple td2) => sametypes (td1,td2)
        | (Tconstr (tycon1,td1), Tconstr (tycon2,td2)) => 
            Tycon.equals (tycon1,tycon2) andalso
            sametypes (td1,td2)
        | (Tfield (s1,td1),Tfield (s2,td2)) => (s1 = s2) andalso sametype (td1,td2)
        | (_,_) => false
      end
	
	end
	

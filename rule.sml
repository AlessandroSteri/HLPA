signature RULE = 
  sig
  	type state
  	type tactic = state -> state

  	val ruleBasic  : int -> tactic
  	val ruleNormal : int -> tactic  
  	val ruleAssign : int -> tactic
  	val ruleSkip   : int -> tactic
  	val ruleWeak   : int -> tactic
  	val ruleStr    : int -> tactic
  	val ruleComp   : int -> tactic
  	val ruleIf     : int -> tactic
  	val ruleWhile  : int -> tactic

  end

structure Rule :> RULE = 
  struct
  	datatype state = State of Hoare.form list * int
  	type tactic = state -> state

  	fun replPre x e (Hoare.Num n)      = (Hoare.Num n)
	  | replPre x e (Hoare.Var y)      = if (Utils.compare (x, y)) then e else (Hoare.Var y)
	  | replPre x e (Hoare.Plus (m,n)) = Hoare.Plus (replPre x e m, replPre x e n)

	fun replPost x e (Hoare.Num n)      = (Hoare.Num n)
	  | replPost x e (Hoare.Var y)      = if (Utils.compare (x, y)) then e else (Hoare.Var y)
	  | replPost x e (Hoare.Plus (m,n)) = if (Hoare.Plus(m, n) = e) then Hoare.Var x else Hoare.Plus (if m = e then Hoare.Var x else replPost x e m, 
	  	                                                                                              if n = e then x else replPost x e n)

	fun subst x e (Hoare.Bool b) replace       = Hoare.Bool b
	  | subst x e (Hoare.Not b) replace        = Hoare.Not (subst x e b replace)
	  | subst x e (Hoare.And (a, b)) replace   = Hoare.And (subst x e a replace, subst x e b replace)
	  | subst x e (Hoare.Or (a, b)) replace    = Hoare.Or (subst x e a replace, subst x e b replace)
	  | subst x e (Hoare.Impl (a, b)) replace  = Hoare.Impl (subst x e a replace, subst x e b replace)
	  | subst x e (Hoare.Minor (m, n)) replace = Hoare.Minor (replace x e m, replace x e n)
	  | subst x e (Hoare.Equal (m, n)) replace = Hoare.Equal (replace x e m, replace x e n)
	  | subst x e _ replace                    = raise Match

	val ruleBasic = ruleBase (fn (Hoare.Triple (b1, Hoare.Skip, b2))           => if b1 = b2 then [] else raise Match
	                           | (Hoare.Triple (b1, Hoare.Assign (x, m), b2))) => if b1 = subst(x m b2 replPre)) orelse b2 = subst(x m b1 replPost) then [] else raise Match
							   | (Hoare.Prop (Hore.Impl(b1, b2))               => [])
							   | _                                             => raise Match

	fun ruleNormal i (State (fs, n))  =
	    let val Triple(MetaVal(a,b), _, _)
	    	  | Triple(_,_,MetaVal(a,b))   = List.nth(fs, i-1) 
	    	  

	val ruleAssign = ruleBase (fn (Hoare.Triple (Hoare.Meta a, Hoare.Assign (x, m), b)) => [Hoare.Triple (Hoare.MetaVal (a, subst x m b replPre), Hoare.Assign (x, m), b)]
		                        | (Hoare.Triple (b, Hoare.Assign (x, m), Hoare.Meta a)) => [Hoare.Triple (b, Hoare.Assign (x, m), Hoare.MetaVal (a, subst x m b replPost))]
		                        | _                                                     => raise Match)

	val ruleSkip = ruleBase (fn (Hoare.Triple (Hoare.Meta a, Hoare.Skip, b)) => [Hoare.Triple (Hoare.MetaVal (a, b), Hoare.Skip, b)]
		                      | (Hoare.Triple (Hoare.Meta a, Hoare.Skip, b)) => [Hoare.Triple (Hoare.MetaVal (a, b), Hoare.Skip, b)]
		                      | _                                            => raise Match)

	val ruleIf = ruleBase (fn (Hoare.Triple (b1, Hoare.If (b2, p1, p2), b3)) => [Hoare.Triple (Hoare.And (b1, b2), p1, b3), Hoare.Triple (Hoare.And (b1, Hoare.Not b2), p2, b3)]
							| _                                              => raise Match)

	val ruleWhile = ruleBase (fn (Hoare.Triple (Hoare.Meta a, Hoare.While (b2, p), b3)) => [Hoare.Triple (Hoare.MetaVal(a, invar(b2, b3)), Hoare.While(b2, p), b3)]
		                       | (Hoare.Triple (b1, Hoare.While (b2, p), Hoare.Meta a)) => [Hoare.Triple (b1, Hoare.While(b2, p), b1)]
		                       | (Hoare.Triple (b1, Hoare.While (b2, p), _))            => [Hoare.Triple (Hoare.And(b1, b2)) p, _)]
							   | _                                                      => raise Match)

	val ruleStr = ruleMeta (fn (Hoare.Triple (b1, p, b2), a) => [Hoare.Prop (Hoare.Impl (b1, Hoare.Meta a)), Hoare.Triple (Hoare.Meta a, p, b2)]
				             | _                             => raise Match)

	val ruleWeak = ruleMeta (fn (Hoare.Triple (b1, p, b2), a) => [Hoare.Triple (b1, p, Hoare.Meta a), Hoare.Prop (Hoare.Impl (Hoare.Meta a, b2))]
				              | _                             => raise Match)

	val ruleComp = ruleMeta (fn (Hoare.Triple (b1, Hoare.Comp (p1, p2), b2), a) => [Hoare.Triple (b1, p1, Hoare.Meta a), Hoare.Triple (Hoare.Meta a, p2, b2)]
		                      | _                                               => raise Match)

  end
signature RULE = 
  sig
    type state
    type tactic = state -> state
    exception TacFailed
    
    val tacAxiom  : int -> tactic
    (* val tacNorm   : int -> tactic *)
    val tacSkip   : int -> tactic
    val tacAssign : int -> tactic
    val tacWhile  : int -> tactic
    val tacIf     : int -> tactic
    val tacStr    : int -> tactic
    val tacWeak   : int -> tactic
    val tacComp   : int -> tactic
  end

structure Rule :> RULE = 
  struct
    structure H    = Hoare
    datatype state = State of H.form list * int
    type tactic    = state -> state
    exception TacFailed    

    fun letter n = String.substring ("abcdefghijklmnopqrstuvwxyz", n, 1)

    fun gensym n =
      if n < 26 then "_" ^ letter n
      else gensym (n div 26) ^ letter (n mod 26)

    (* dummy function *)
    fun invar b post = H.Bool true

    fun spliceForms forms newForms i =
      List.take (forms, i - 1) @ newForms @ List.drop (forms, i)

    (* Nel caso in cui la lista forms è vuota o l'indice i non è valido viene propagata un'eccezione *)
    fun tacBase f i (State (forms, n)) =
      let val newForms = spliceForms forms (f (List.nth (forms, i - 1))) i
      in  State (newForms, n)
      end

    fun tacMeta f i (State (forms, n)) =
      let val newForms = spliceForms forms (f (List.nth (forms, i - 1), gensym n)) i
      in  State (newForms, n + 1)
      end

    val tacAxiom =
      tacBase (fn H.Prop (H.Impl (b1, b2))              => []
                | H.Triple (pre, H.Skip, post)          => if pre = post then [] else raise TacFailed
                | H.Triple (pre, H.Assign (x, m), post) => if pre = H.subst H.replPre x m pre then [] else raise TacFailed
                | _                                     => raise Match)

    val tacSkip =
      tacBase (fn H.Triple (H.Meta a, H.Skip, post) => [H.Triple (H.MetaVal (a, post), H.Skip, post)]
                | H.Triple (pre, H.Skip, H.Meta a)  => [H.Triple (pre, H.Skip, H.MetaVal (a, pre))]
                | _                                 => raise Match)

    val tacAssign =
      tacBase (fn H.Triple (H.Meta a, H.Assign (x, m), post) => let val v = H.subst H.replPre x m post
                                                                in  [H.Triple (H.MetaVal (a, v), H.Assign (x, m), post)]
                                                                end
                | H.Triple (pre, H.Assign (x, m), H.Meta a)  => let val v = H.subst H.replPost x m pre
                                                                in  [H.Triple (pre, H.Assign (x, m), H.MetaVal (a, v))]
                                                                end
                | _                                          => raise Match)

    val tacWhile =
      tacBase (fn H.Triple (H.Meta a, H.While (b, p), post) => let val v = invar b post
                                                               in  [H.Triple (H.MetaVal (a, v), H.While (b, p), post)]
                                                               end
                | H.Triple (pre, H.While (b, p), H.Meta a)  => let val v = H.And (pre, H.Not b)
                                                               in  [H.Triple (pre, H.While (b, p), H.MetaVal (a, v))]
                                                               end
                | H.Triple (pre, H.While (b, p), post)      => if post = H.And (pre, H.Not b) then
                                                                 [H.Triple (H.And (pre, b), p, pre)]
                                                               else
                                                                 raise TacFailed
                | _                                         => raise Match)

    val tacIf =
      tacBase (fn H.Triple (pre, H.If (b, p1, p2), post) => [ H.Triple (H.And (pre, b), p1, post)
                                                            , H.Triple (H.And (pre, H.Not b), p2, post)
                                                            ]
                | _                                      => raise Match)

    val tacStr =
      tacMeta (fn (H.Triple (pre, p, post), a) => [H.Prop (H.Impl (pre, H.Meta a)), H.Triple (H.Meta a, p, post)]
                | _                            => raise Match)

    val tacWeak =
      tacMeta (fn (H.Triple (pre, p, post), a) => [H.Triple (pre, p, H.Meta a), H.Prop (H.Impl (H.Meta a, post))]
                | _                            => raise Match)

    val tacComp =
      tacMeta (fn (H.Triple (pre, H.Comp (p1, p2), post), a) => [H.Triple (pre, p1, H.Meta a), H.Triple (H.Meta a, p2, post)]
                | _                                          => raise Match)
  end
signature RULE = 
  sig
    exception InvalFormIndex
    exception InvarError
    exception TacFailed

    type state
    type tactic = state -> state
    
    val init      : Hoare.form -> state
    val isFinal   : state -> bool
    val getForms  : state -> Hoare.form list
    val tacNorm   : int -> tactic
    val tacAxiom  : int -> tactic
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
    structure H = Hoare
    exception InvalFormIndex
    exception InvarError
    exception TacFailed

    datatype state = State of H.form list * int
    type tactic    = state -> state

    fun letter n = String.substring ("abcdefghijklmnopqrstuvwxyz", n, 1)

    fun gensym n =
      if n < 26 then "_" ^ letter n
      else gensym (n div 26) ^ letter (n mod 26)

    fun invar g post =
      let val ng = H.Not g
          val H.And (b1, b2) = post
      in  if (ng = b2) then b1 else raise InvarError
      end

    fun spliceForms forms newForms i =
      List.take (forms, i - 1) @ newForms @ List.drop (forms, i)

    fun tacBase f i (State (forms, n)) =
      let val newForms = spliceForms forms (f (List.nth (forms, i - 1))) i
      in  State (newForms, n)
      end
      handle Subscript => raise InvalFormIndex

    fun tacMeta f i (State (forms, n)) =
      let val newForms = spliceForms forms (f (List.nth (forms, i - 1), gensym n)) i
      in  State (newForms, n + 1)
      end
      handle Subscript => raise InvalFormIndex

    fun init form = State ([form], 0)

    fun isFinal (State (forms, _)) = null forms

    fun getForms (State (forms, _)) = forms

    local
      fun substForms b1 b2 []                                 = []
        | substForms b1 b2 (H.Prop b :: forms)                = H.Prop (H.substBool b1 b2 b) :: substForms b1 b2 forms
        | substForms b1 b2 (H.Triple (pre, p, post) :: forms) = let val pre'  = H.substBool b1 b2 pre
                                                                    val post' = H.substBool b1 b2 post
                                                                in  H.Triple (pre', p, post') :: substForms b1 b2 forms
                                                                end
    in
      fun tacNorm i (State (forms, n)) =
        let val mv as H.MetaVal (a, b) = hd (H.getMetaVals (List.nth (forms, i - 1)))
        (* in  State (substForms mv b (substForms (H.Meta a, b, forms)), n) *)
        in  State (substForms mv b (substForms (H.Meta a) b forms), n)
        end
        handle Subscript => raise InvalFormIndex
    end

    val tacAxiom =
      tacBase (fn H.Prop (H.Impl (b1, b2))              => []
                | H.Triple (pre, H.Skip, post)          => if pre = post then [] else raise TacFailed
                | H.Triple (pre, H.Assign (x, m), post) => if pre = H.substNum (H.Var x) m pre then [] else raise TacFailed
                | _                                     => raise Match)

    val tacSkip =
      tacBase (fn H.Triple (H.Meta a, H.Skip, post) => [H.Triple (H.MetaVal (a, post), H.Skip, post)]
                | H.Triple (pre, H.Skip, H.Meta a)  => [H.Triple (pre, H.Skip, H.MetaVal (a, pre))]
                | _                                 => raise Match)

    val tacAssign =
      tacBase (fn H.Triple (H.Meta a, H.Assign (x, m), post) => let val v = H.substNum (H.Var x) m post
                                                                in  [H.Triple (H.MetaVal (a, v), H.Assign (x, m), post)]
                                                                end
                | H.Triple (pre, H.Assign (x, m), H.Meta a)  => let val v = H.substNum (H.Var x) m pre
                                                                in  [H.Triple (pre, H.Assign (x, m), H.MetaVal (a, v))]
                                                                end
                | _                                          => raise Match)

    val tacWhile =
      tacBase (fn H.Triple (H.Meta a, H.While (g, p), post) => let val v = invar g post
                                                               in  [H.Triple (H.MetaVal (a, v), H.While (g, p), post)]
                                                               end
                | H.Triple (pre, H.While (g, p), H.Meta a)  => let val v = H.And (pre, H.Not g)
                                                               in  [H.Triple (pre, H.While (g, p), H.MetaVal (a, v))]
                                                               end
                | H.Triple (pre, H.While (g, p), post)      => if post = H.And (pre, H.Not g) then
                                                                 [H.Triple (H.And (pre, g), p, pre)]
                                                               else
                                                                 raise TacFailed
                | _                                         => raise Match)

    val tacIf =
      tacBase (fn H.Triple (pre, H.If (g, p1, p2), post) => [ H.Triple (H.And (pre, g), p1, post)
                                                            , H.Triple (H.And (pre, H.Not g), p2, post)
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
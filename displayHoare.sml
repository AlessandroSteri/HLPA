signature DISPLAY_HOARE = 
  sig 
    val displayForm : int -> Hoare.form -> unit
  end

structure DisplayHoare :> DISPLAY_HOARE =
  struct 
    structure H = Hoare

    fun printNum (H.Num k)     = Int.toString k
      | printNum (H.Var x)     = x
      | printNum (H.Plus(m,n)) = printNum m ^ " + " ^ printNum n 
    (*| printNum (Neg m)    = "(-" ^ printNum m ^ ")"*)

    fun printBool (H.Or(b1,b2))       = printBool b1 ^ " | " ^ printBool b2
      | printBool (H.And(b1,b2))      = printBool b1 ^ " & " ^ printBool b2
      | printBool (H.Not(H.Minor(m,n))) = printNum m ^ " >= " ^ printNum n 
      | printBool (H.Not b)           = "~" ^ printBool b
      | printBool (H.Bool b)          = Bool.toString(b)
      | printBool (H.Impl(a, b))      = "(" ^ printBool a ^ " -> " ^ printBool b ^ ")"
      | printBool (H.Minor(m, n))     = printNum m ^ " < " ^ printNum n
      | printBool (H.Equal(m, n))     = printNum m ^ " = " ^ printNum n
      | printBool (H.Meta a)          = "?" ^ a
      | printBool (H.MetaVal(a,b))    = "?" ^ a ^ " : " ^ printBool b

    fun printProg H.Skip           = "skip"
      | printProg (H.Comp(p1,p2))  = printProg p1 ^ "; " ^ printProg p2
      | printProg (H.Assign(x,m))  = x ^ " := " ^ printNum m
      | printProg (H.If(b,p1,p2))  = "if " ^ printBool b ^ " then " ^ printProg p1 
                                                         ^ " else " ^ printProg p2
      | printProg (H.While(b,p))   = "while (" ^ printBool b ^ ") do (" ^ printProg p ^ ")" 

    local fun printForm (H.Prop b)          = printBool b
            | printForm (H.Triple(b1,p,b2))   = "{" ^ printBool b1 ^ "} " ^ printProg p ^ " {" ^ printBool b2 ^ "}"
    in
      fun displayForm n form = print (Int.toString n ^ ". " ^ printForm form ^ "\n")
    end
  end
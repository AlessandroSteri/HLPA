signature PRINTER =
  sig
    val prForm: int -> Hoare.form -> unit
  end

structure Printer :> PRINTER =
  struct
    structure H = Hoare

    fun prNExp (H.Num n)         = Int.toString n
      | prNExp (H.Var x)         = x
      | prNExp (H.Plus (m1, m2)) = prNExp m1 ^ " + " ^ prNExp m2

    fun prBExp (H.Bool b)                 = Bool.toString b
      | prBExp (H.Not (H.Minor (m1, m2))) = prNExp m1 ^ " >= " ^ prNExp m2
      | prBExp (H.Not b)                  = "~" ^ prBExp b
      | prBExp (H.Meta a)                 = a
      | prBExp (H.MetaVal (a, v))         = a ^ " : " ^ prBExp v
      | prBExp (H.And (b1, b2))           = prBExp b1 ^ " & " ^ prBExp b2
      | prBExp (H.Or (b1, b2))            = prBExp b1 ^ " | " ^ prBExp b2
      | prBExp (H.Impl (b1, b2))          = prBExp b1 ^ " -> " ^ prBExp b2
      | prBExp (H.Minor (m1, m2))         = prNExp m1 ^ " < " ^ prNExp m2
      | prBExp (H.Equal (m1, m2))         = prNExp m1 ^ " = " ^ prNExp m2

    fun prProg H.Skip             = "skip"
      | prProg (H.Comp (p1, p2))  = prProg p1 ^ "; " ^ prProg p2
      | prProg (H.Assign (x, m))  = x ^ " := " ^ prNExp m
      | prProg (H.While (g, p))   = "while (" ^ prBExp g ^ ") do " ^ prProg p ^ " end"
      | prProg (H.If (g, p1, p2)) = "if (" ^ prBExp g ^ ") then " ^ prProg p1 ^ " else " ^ prProg p2 ^ " end"

    fun prForm i form =
      let fun helper (H.Prop b)                = prBExp b
            | helper (H.Triple (pre, p, post)) = "{" ^ prBExp pre ^ "}" ^ prProg p ^ "{" ^ prBExp post ^ "}"
      in  print (Int.toString i ^ ". " ^ helper form ^ "\n")
  	  end
  end
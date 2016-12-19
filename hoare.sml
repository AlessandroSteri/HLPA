signature HOARE = 
  sig
    datatype numExp = Num  of int
                    | Var  of string
                    | Plus of numExp * numExp

    datatype boolExp = Bool    of bool
                     | Not     of boolExp
                     | Meta    of string
                     | MetaVal of string * boolExp
                     | And     of boolExp * boolExp
                     | Or      of boolExp * boolExp
                     | Impl    of boolExp * boolExp
                     | Minor   of numExp * numExp
                     | Equal   of numExp * numExp

    datatype prog = Skip
                  | Comp   of prog * prog
                  | Assign of string * numExp
                  | While  of boolExp * prog
                  | If     of boolExp * prog * prog

    datatype form = Prop   of boolExp
                  | Triple of boolExp * prog * boolExp

    type repl
    val replPre  : repl
    val replPost : repl
    val subst    : repl -> string -> numExp -> boolExp -> boolExp
  end

structure Hoare :> HOARE = 
  struct
    datatype numExp = Num  of int
                    | Var  of string
                    | Plus of numExp * numExp

    datatype boolExp = Bool    of bool
                     | Not     of boolExp
                     | Meta    of string
                     | MetaVal of string * boolExp
                     | And     of boolExp * boolExp
                     | Or      of boolExp * boolExp
                     | Impl    of boolExp * boolExp
                     | Minor   of numExp * numExp
                     | Equal   of numExp * numExp

    datatype prog = Skip
                  | Comp   of prog * prog
                  | Assign of string * numExp
                  | While  of boolExp * prog
                  | If     of boolExp * prog * prog

    datatype form = Prop   of boolExp
                  | Triple of boolExp * prog * boolExp

    type repl = string -> numExp -> numExp -> numExp

    fun replPre x e (Num n)      = Num n
      | replPre x e (Var y)      = if x = y then e else Var y
      | replPre x e (Plus (m,n)) = Plus (replPre x e m, replPre x e n)

    fun replPost x e (Num n)      = Num n
      | replPost x e (Var y)      = if Var y = e then Var x else Var y
      | replPost x e (Plus (m,n)) = if Plus (m, n) = e then
                                      Var x
                                    else
                                      let val m' = if m = e then Var x else replPost x e m
                                          val n' = if n = e then Var x else replPost x e n
                                      in  Plus (m', n')
                                      end

    fun subst f x e (Bool b)        = Bool b
      | subst f x e (Not b)         = Not (subst f x e b)
      | subst f x e (And (a, b))    = And (subst f x e a, subst f x e b)
      | subst f x e (Or  (a, b))    = Or (subst f x e a, subst f x e b)
      | subst f x e (Impl (a, b))   = Impl (subst f x e a, subst f x e b)
      | subst f x e (Minor (m, n))  = Minor (f x e m, f x e m)
      | subst f x e (Equal (m, n))  = Equal (f x e m, f x e m)
      | subst f x e _               = raise Match
  end
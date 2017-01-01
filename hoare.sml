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

    val substNum    : numExp -> numExp -> boolExp -> boolExp
    val substBool   : boolExp -> boolExp -> boolExp -> boolExp
    val getMetaVals : form -> boolExp list
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

    local
      fun replace m1 m2 (e as Plus (m, n)) = if e = m1 then m2 else Plus (replace m1 m2 m, replace m1 m2 n)
        | replace m1 m2 e                  = if e = m1 then m2 else e
    in
      fun substNum m1 m2 (Bool b)       = Bool b
        | substNum m1 m2 (Not b)        = Not (substNum m1 m2 b)
        | substNum m1 m2 (And (a, b))   = And (substNum m1 m2 a, substNum m1 m2 b)
        | substNum m1 m2 (Or (a, b))    = Or (substNum m1 m2 a, substNum m1 m2 b)
        | substNum m1 m2 (Impl (a, b))  = Impl (substNum m1 m2 a, substNum m1 m2 b)
        | substNum m1 m2 (Minor (m, n)) = Minor (replace m1 m2 m, replace m1 m2 n)
        | substNum m1 m2 (Equal (m, n)) = Equal (replace m1 m2 m, replace m1 m2 n)
        | substNum m1 m2 _              = raise Match
    end

    local
      fun metaEqual a (Meta a')         = a = a'
        | metaEqual a (MetaVal (a', _)) = a = a'
        | metaEqual a _                 = false
    in
      fun substBool b1 b2 (e as Not b)          = if e = b1 then b2 else Not (substBool b1 b2 b)
        | substBool b1 b2 (e as And (a, b))     = if e = b1 then b2 else And (substBool b1 b2 a, substBool b1 b2 b)
        | substBool b1 b2 (e as Or (a, b))      = if e = b1 then b2 else Or (substBool b1 b2 a, substBool b1 b2 b)
        | substBool b1 b2 (e as Impl (a, b))    = if e = b1 then b2 else Impl (substBool b1 b2 a, substBool b1 b2 b)
        | substBool b1 b2 (e as MetaVal (a, _)) = if metaEqual a b1 then b2 else e
        | substBool b1 b2 e                     = if e = b1 then b2 else e
    end

    local
      fun helper (And (a, b))          = (helper a) @ (helper b)
        | helper (mv as MetaVal (_,_)) = [mv]
        | helper _                     = []
    in
      fun getMetaVals (Prop b)                = helper b
        | getMetaVals (Triple (pre, p, post)) = (helper pre) @ (helper post)
    end
  end
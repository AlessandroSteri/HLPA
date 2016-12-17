signature HOARE =
  sig
  	datatype numExp = Num  of int
	                | Var  of string
	                | Plus of numExp * numExp

	datatype boolExp = Bool     of bool
	                 | Not      of boolExp
	                 | And      of boolExp * boolExp
	                 | Or       of boolExp * boolExp
	                 | Impl     of boolExp * boolExp
	                 | Minor    of numExp * numExp
	                 | Equal    of numExp * numExp
	                 | Meta     of string
	                 | MetaVal  of string * boolExp

	datatype prog = Skip
	              | Comp   of prog * prog
	              | Assign of string * numExp
	              | If     of boolExp * prog * prog
	              | While  of boolExp * prog

	datatype form = Prop   of boolExp
	              | Triple of boolExp * prog * boolExp
  end


structure Hoare :> HOARE =
  struct
	datatype numExp = Num  of int
	                | Var  of string
	                | Plus of numExp * numExp

	datatype boolExp = Bool  of bool
	                 | Not   of boolExp
	                 | And   of boolExp * boolExp
	                 | Or    of boolExp * boolExp
	                 | Impl  of boolExp * boolExp
	                 | Minor of numExp * numExp
	                 | Equal of numExp * numExp
	                 | Meta  of string
	              	 | MetaVal  of string * boolExp


	datatype prog = Skip
	              | Comp   of prog * prog
	              | Assign of string * numExp
	              | If     of boolExp * prog * prog
	              | While  of boolExp * prog

	datatype form = Prop   of boolExp
	              | Triple of boolExp * prog * boolExp
  end
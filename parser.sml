use "hoare.sml";

(*NOTA IMPORTANTE: Ho rimosso temporaneamente la segnatura, questo mi permette di esporre direttamente
					tutta la struttura del parser; una volta finito, possiamo ricoprire tutto*)
(*
signature PARSER = 
sig
	exception InvalidArgument of string

	val parse : string -> Hoare.form
end*)
print "loading";
structure Parser (*:> PARSER*) = 
struct
	exception InvalidArgument of string

	(*Token kinds are implemented as hierarchical datatypes, hence the constructor nestings :/ *)

	(*TODO Negative integers and subtraction are yet to be added *)
	datatype arithmToken = 
		LitInt of int | Variable of string | SymPlus | SymTimes |
		LParen | RParen |
		ASubExp of Hoare.numExp

	datatype arithmLogicToken = 
		ART of arithmToken |
	    LitBool of bool | SymEquals | SymLessThan | SymNot | SymImplies | SymAnd | SymOr |
	    BSubExp of Hoare.boolExp

	datatype programToken = 
		Skip | EndOfStmt | CmdAssign | CmdIf | CmdThen | CmdElse | CmdWhile | CmdDo | CmdEnd |
		SubProgram of Hoare.prog

	datatype token =
	    ALT of arithmLogicToken | PT of programToken | LBrace | RBrace

	(*=======================================================================================*)
	(* TOKENIZER SECTION                                                                     *)

	(*	Predicate telling if a char can be part of a variable name (i.e. not a reserved symbol)
		Letters, digits and underscores make valid identifiers,
			the first symbol is correclty detected by the tokenizer *)
	fun isValidVarSymbol (ch : char) : bool =
		Char.isDigit ch orelse Char.isAlpha ch orelse (ch = #"_");

	(*	Tokenizes a string, filtering keywords before creating an actual varible identifier *)
	val rec makeWordToken = fn
		"skip" 		=> PT Skip
		| "if" 		=> PT CmdIf
		| "then" 	=> PT CmdThen
		| "else" 	=> PT CmdElse
		| "while" 	=> PT CmdWhile
		| "do" 		=> PT CmdDo
		| "end" 	=> PT CmdEnd
		| "true" 	=> ALT (LitBool true)
		| "false" 	=> ALT (LitBool false)
		| s 		=> ALT (ART (Variable s));

	(*	--==The Little Jewel==--

		Signature: (glyphs, source, agglomerate, predicate) -> (token, remainder)
		Current use: Given
			- 'source': a list of characters,
			- 'glyphs': another list (that SHALL initially be empty),
			- 'agglomerate': a function that maps char-lists to tokens,
			- 'predicate': a predicate that decides if a char from 'source' is allowed in 'glyphs',
		this method:
			- starts reading 'source' and puts all accepted chars in 'glyphs', according to 'predicate',
			- when 'source' is empty, or the next char from it is rejected,
				'agglomerate's the 'glyphs' into a token
			- and finally returns the token, along with the remainder of 'source' 

		NOTA: Apparentemente questo metodo è profondamente polimorfo, ho il forte sospetto che
			possa essere riusato ad un livello superiore, partendo da liste di token
			e generando numExp/boolExp/prog, ma questo è tutto da vedere... le parentesi non perdonano *)
	val rec splicer : 'a list * 'a list * ('a list -> 'b) * ('a -> bool) -> 'b * 'a list = fn
		(nil, _, _, _) 									=> raise InvalidArgument "Empty symbol list"
		| (glyphs, nil, agglomerate, _) 				=> (agglomerate glyphs, nil)
		| (glyphs, g :: tail, agglomerate, predicate) 	=> if (predicate g)
			then splicer (glyphs @ [g], tail, agglomerate, predicate)
			else (agglomerate glyphs, g :: tail)

	(*the Tokenizer - repetitive, but does its job well*)
	val rec tokenize : char list -> token list = fn
		nil 					=> nil
		| #" " :: tail 			=> tokenize tail
		| #"{" :: tail 			=> LBrace :: tokenize tail
		| #"}" :: tail 			=> RBrace :: tokenize tail
		| #"(" :: tail 			=> ALT (ART LParen) :: tokenize tail
		| #")" :: tail 			=> ALT (ART RParen) :: tokenize tail
		| #"*" :: tail 			=> ALT (ART SymTimes) :: tokenize tail
		| #"+" :: tail 			=> ALT (ART SymPlus) :: tokenize tail
		| #"&" :: tail 			=> ALT SymAnd :: tokenize tail
		| #"|" :: tail 			=> ALT SymOr :: tokenize tail
		| #"<" :: tail 			=> ALT SymLessThan :: tokenize tail
		| #"!" :: tail 			=> ALT SymNot :: tokenize tail
		| #";" :: tail 			=> PT EndOfStmt :: tokenize tail
		| #":" :: #"=" :: tail 	=> PT CmdAssign :: tokenize tail
		(*Recognizing both implication and equality*)
		| #"=" :: tail 			=> if ((not (List.null tail)) andalso (List.hd tail = #">")) 
			then (ALT SymImplies :: tokenize (List.tl tail))
			else (ALT SymEquals :: tokenize tail)
		(*Recognizing keywords, numbers and identifiers*)
		| c :: tail 			=> if (Char.isDigit c)
			then let val (token, tail') = splicer ([c], tail, ALT o ART o LitInt o Option.valOf o Int.fromString o String.implode, Char.isDigit)
				in (token :: tokenize tail') end
			else if (isValidVarSymbol c)
				then let val (token, tail') = splicer ([c], tail, makeWordToken o String.implode, isValidVarSymbol)
					in (token :: tokenize tail') end
				else raise InvalidArgument ("Stray characher: " ^ (Char.toString c))

	(* for testing purposes*)
	val rec toString : arithmLogicToken list -> char list = fn
		nil 						=> nil
		| ART LParen :: tail		=> #"(" :: toString tail
		| ART RParen :: tail		=> #")" :: toString tail
		| ART SymTimes :: tail		=> #"*" :: toString tail
		| ART SymPlus :: tail		=> #"+" :: toString tail
		| SymAnd :: tail			=> #"&" :: toString tail
		| SymOr :: tail				=> #"|" :: toString tail
		| SymLessThan :: tail 		=> #"<" :: toString tail
		| SymNot :: tail 			=> #"!" :: toString tail
		(*Recognizing both implication and equality*)
		| SymImplies :: tail		=> #"=" :: #">" :: toString tail
		| SymEquals :: tail			=> #"=" :: toString tail
		| ART (Variable v) :: tail	=> (String.explode v) @ (toString tail)
		| ART (LitInt i) :: tail	=> (String.explode (Int.toString i)) @ (toString tail)
		(*Recognizing keywords, numbers and identifiers*)
		| LitBool true :: tail 		=> (String.explode "true") @ (toString tail)
		| LitBool false :: tail 	=> (String.explode "false") @ (toString tail)
		| _ :: tail 				=> #"#" :: toString tail

	(*=======================================================================================*)
	(* PARSER SECTION                                                                        *)

	(* BETTER Polymorph list splitter, mimicks String.tokens*)
	val rec spH2 : ('a -> bool) * 'a list list * 'a list * 'a list -> 'a list list = fn
		(_, chain, vec, nil) 						=> chain @ [vec]
		| (predicate, chain, vec, elem :: source) 	=> if (predicate elem)
			then spH2 (predicate, chain @ [vec], nil, source)
			else spH2 (predicate, chain, vec @ [elem], source)
	val split2 : ('a -> bool) -> 'a list -> 'a list list = fn
		predicate => fn source => spH2 (predicate, nil, nil, source)
		
	(* ARITHMETIC ---------------------------------------------------------------------------*)

	(* called on expressions composed solely of variables, literals, subexpressions and the product symbol;
		the generated tree nests on the right side *)
	val rec foldProduct : arithmLogicToken list -> Hoare.numExp = fn
		ART (LitInt e) :: nil 						=> Hoare.Num e
		| ART (Variable e) :: nil 					=> Hoare.Var e
		| ART (ASubExp e) :: nil 					=> e
		| ART (LitInt e) :: ART (SymTimes) :: l 	=> Hoare.Times (Hoare.Num e, foldProduct l)
		| ART (Variable e) :: ART (SymTimes) :: l 	=> Hoare.Times (Hoare.Var e, foldProduct l)
		| ART (ASubExp e) :: ART (SymTimes) :: l 	=> Hoare.Times (e, foldProduct l)
		(* the structure here is simple enough to be described by the rules above,
			anything out of place will interrupt parsing, nil case included*)
		| _ 							=> raise InvalidArgument "Error while building arithmetic expression"

	(* Empty expression should've been taken care of at product level *)
	val rec foldSum : Hoare.numExp list -> Hoare.numExp = fn
		elem :: nil => elem
		| ls 		=> Hoare.Plus(List.hd ls, foldSum(List.tl ls))

	(* Actual expression builder, uses foldSum and foldProduct in tandem, according to precedence rules *)
	val genNumExp : arithmLogicToken list -> Hoare.numExp = 
		foldSum o List.map foldProduct o split2 (fn c => c = ART (SymPlus))

	(* BOOLEAN ------------------------------------------------------------------------------*)

	val rec genArithmPredicate : arithmLogicToken list * arithmLogicToken list -> Hoare.boolExp = fn 
		(SymEquals :: tail, ex1) => Hoare.Equal (genNumExp ex1, genNumExp tail)
		| (SymLessThan :: tail, ex1) => Hoare.Minor (genNumExp ex1, genNumExp tail)
		| (t :: tail, ex1) => genArithmPredicate (tail, ex1 @ [t])
		| _ => raise InvalidArgument "error on equals"

	(*Bridges the gap between boolean and arithmetic, while recognizing negations and truth values too, which must appear as the first symbol of the simple expression*)
	val rec gsbeH : arithmLogicToken list -> Hoare.boolExp = fn 
		SymNot :: tokens => Hoare.Not (gsbeH (tokens))
		| LitBool b :: nil => Hoare.Bool b
		| BSubExp e :: nil => e
		(*do recon arithm predicates now, must be single!*)
		| tokens => genArithmPredicate (tokens, nil)


(*DO REVERSE IMPL TREE!!!*)
(*current state: a=>b=>c translates to a=>(b=>c) *)
	(* Empty expression should've been taken care of at product level *)
	val rec foldAnd : Hoare.boolExp list -> Hoare.boolExp = fn
		elem :: nil => elem
		| ls 		=> Hoare.And(List.hd ls, foldAnd(List.tl ls))
	(* Empty expression should've been taken care of at product level *)
	val rec foldOr : Hoare.boolExp list -> Hoare.boolExp = fn
		elem :: nil => elem
		| ls 		=> Hoare.Or(List.hd ls, foldOr(List.tl ls))
	(* Empty expression should've been taken care of at product level *)
	val rec foldImpl : Hoare.boolExp list -> Hoare.boolExp = fn
		elem :: nil => elem
		| ls 		=> Hoare.Impl(List.hd ls, foldImpl(List.tl ls))


	val genBoolExp : arithmLogicToken list -> Hoare.boolExp =
		foldImpl
		o List.map foldOr
		o List.map (List.map foldAnd)
		o List.map (List.map (List.map (gsbeH)))
		o List.map (List.map (split2 (fn oper => oper = SymAnd)))
		o List.map (split2 (fn oper => oper = SymOr))
		o split2 (fn c => c = SymImplies)




	val isArithmetic : arithmLogicToken -> bool = fn 
		ART a => true
		| _ => false
	val decideType : arithmLogicToken list -> arithmLogicToken = fn 
		tokens => if (List.all isArithmetic tokens)
			then ART (ASubExp (genNumExp tokens))
			else BSubExp (genBoolExp tokens)

	val rec bbeH : arithmLogicToken list * arithmLogicToken list list -> Hoare.boolExp = fn
		(ART (LParen) :: tail, outer) 	=> bbeH (tail, nil :: outer)
		| (ART (RParen) :: tail, outer) => bbeH (decideType (List.hd outer) :: tail, List.tl outer)
		| (token :: tail, outer) 		=> bbeH (tail, (List.hd outer @ [token]) :: (List.tl outer))
		(*On empty input, there must be only one level, else we have an unclosed parenthesis*)
		| (nil, form :: nil) 			=> genBoolExp form
		| (nil, _) 						=> raise InvalidArgument "Missing closing parenthesis"
	val buildBoolExp : arithmLogicToken list -> Hoare.boolExp = fn
		tokens => bbeH (tokens, [nil]);


	(* ONLY For testing purposes - nonlogic tokens WILL RAISE exceptions (i think...) *)
	val testBoolExp : string -> Hoare.boolExp = fn
		input => (print "hello\n"; buildBoolExp (List.map (fn ALT t => t) (tokenize (String.explode input))))
		handle InvalidArgument s => (print ("Parsing error: "^ s ^"\n") ; Hoare.Bool false)
			| Empty => (print ("No input given.\n") ; Hoare.Bool false)



	(* PROGRAM ------------------------------------------------------------------------------*)


	val buildProgram : token list -> Hoare.prog = fn
		tokens => Hoare.Skip;






	(* STARTING POINT -----------------------------------------------------------------------*)

	(*Identifies the pre/post-condition sections of a triple*)
	val rec findCondition : arithmLogicToken list * token list -> Hoare.boolExp * token list = fn
		(*No tokens, we don't have any input*)
		(nil, nil) 						=> raise InvalidArgument "Empty condition"
		(*Still reading valid tokens, check if they are of the boolExp/numExp kind*)
		| (altokens, ALT token :: tail) => findCondition (altokens @ [token], tail)
		(*Found end of precondition, build its boolean expression and return everything*)
		| (altokens, RBrace :: tail) 	=> (buildBoolExp (altokens), tail)
		| (_, _) 						=> raise InvalidArgument "error when reading condition"

	(*Identifies the program section of a triple*)
	val rec findProgram : token list * token list -> Hoare.prog * token list = fn
		(*No tokens, we don't have any input*)
		(nil, nil) 						=> raise InvalidArgument "Empty program"
		(*Found end of program, build its tree and return everything*)
		| (ptokens, LBrace :: tail) 	=> (buildProgram (ptokens), tail)
		(*Still reading other tokens*)
		| (ptokens, token :: tail) 		=> findProgram (ptokens @ [token], tail)
		| (_, _) 						=> raise InvalidArgument "error when reading program"

	(*Parsing begins here*)
	fun parse (input : string) : Hoare.form =
		let val tokens = tokenize (String.explode input)
		in if (List.hd tokens) = LBrace
			then let 
				val (precondition, tokens') 	= findCondition (nil, (List.tl tokens))
				val (program, tokens'') 		= findProgram (nil, tokens')
				val (postcondition, tokens''') 	= findCondition (nil, tokens'')
			in Hoare.Triple (precondition, program, postcondition) end
			else raise InvalidArgument "Triple must begin with a left brace" end
		handle InvalidArgument s => (print ("Parsing error: "^ s ^"\n") ; Hoare.Prop (Hoare.Bool false))
			| Empty => (print ("No input given.\n") ; Hoare.Prop (Hoare.Bool false))

end
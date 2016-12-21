use "hoare.sml";

(*NOTA IMPORTANTE: Ho rimosso temporaneamente la segnatura, questo mi permette di esporre direttamente
					tutta la struttura del parser; una volta finito, possiamo ricoprire tutto*)

(*signature PARSER = 
sig
	exception InvalidArgument of string

	val parse : string -> Hoare.form
end*)

structure Parser (*:> PARSER*) = 
struct
	exception InvalidArgument of string

	(*Token kinds are implemented as hierarchical datatypes, hence the constructor nestings :/ *)

	(*TODO Negative integers are yet to be added *)
	datatype arithmToken = 
		LitInt of int | Variable of string | SymPlus | SymTimes |
		LParen | RParen

	datatype arithmLogicToken = 
		ART of arithmToken |
	    LitBool of bool | SymEquals | SymLessThan | SymNot | SymImplies

	datatype programToken = 
		Skip | EndOfStmt | CmdAssign | CmdIf | CmdThen | CmdElse | CmdWhile | CmdDo | CmdEnd

	datatype token =
	    ALT of arithmLogicToken | PT of programToken | LBrace | RBrace

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
		| (glyphs, g :: tail, agglomerate, predicate) 	=> 
			if (predicate g)
				then splicer (glyphs @ [g], tail, agglomerate, predicate)
				else (agglomerate glyphs, g :: tail)

	(*the Tokenizer - rather tedious, but does its job well*)
	val rec tokenize : char list -> token list = fn
		nil 					=> nil
		| #" " :: tail 			=> tokenize tail
		| #"{" :: tail 			=> LBrace :: tokenize tail
		| #"}" :: tail 			=> RBrace :: tokenize tail
		| #"(" :: tail 			=> ALT (ART LParen) :: tokenize tail
		| #")" :: tail 			=> ALT (ART RParen) :: tokenize tail
		| #"*" :: tail 			=> ALT (ART SymTimes) :: tokenize tail
		| #"+" :: tail 			=> ALT (ART SymPlus) :: tokenize tail
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


	(*DUMMY FUNCTION*)
	val buildNumExp : arithmToken list -> Hoare.numExp = fn
		tokens => Hoare.Num 0;

	(*DUMMY FUNCTION*)
	val buildBoolExp : arithmLogicToken list -> Hoare.boolExp = fn
		tokens => Hoare.Bool true;

	(*DUMMY FUNCTION*)
	val buildProgram : token list -> Hoare.prog = fn
		tokens => Hoare.Skip;

	(*Identifies the pre/post-condition sections of a triple*)
	val rec findCondition : arithmLogicToken list * token list -> Hoare.boolExp * token list = fn
		(*No tokens, we don't have any input*)
		(nil, nil) => raise InvalidArgument "Empty condition"
		(*Still reading valid tokens, check if they are of the boolExp/numExp kind*)
		| (altokens, ALT token :: tail) => findCondition(altokens @ [token], tail)
		(*Found end of precondition, build its boolean expression and return everything*)
		| (altokens, RBrace :: tail) => (buildBoolExp(altokens), tail)
		| (_, _) => raise InvalidArgument "error when reading condition"

	(*Identifies the program section of a triple*)
	val rec findProgram : token list * token list -> Hoare.prog * token list = fn
		(*No tokens, we don't have any input*)
		(nil, nil) => raise InvalidArgument "Empty program"
		(*Found end of program, build its tree and return everything*)
		| (ptokens, LBrace :: tail) => (buildProgram(ptokens), tail)
		(*Still reading other tokens*)
		| (ptokens, token :: tail) => findProgram(ptokens @ [token], tail)
		| (_, _) => raise InvalidArgument "error when reading program"

	(*Parsing begins here*)
	fun parse (input : string) : Hoare.form =
		let val tokens = tokenize (String.explode input)
		in if (List.hd tokens) = LBrace
			then let 
				val (precondition, tokens') 	= findCondition (nil, (List.tl tokens))
				val (program, tokens'') 		= findProgram (nil, tokens')
				val (postcondition, tokens''') 	= findCondition (nil, tokens'')
			in Hoare.Triple(precondition, program, postcondition) end
			else raise InvalidArgument "Triple must begin with a left brace" end
		handle InvalidArgument s => (print ("Parsing error: "^ s ^"\n") ; Hoare.Prop (Hoare.Bool false))
			| Empty => (print ("No input given.\n") ; Hoare.Prop (Hoare.Bool false))

end
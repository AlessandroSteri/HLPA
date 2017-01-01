signature LEXER =
  sig
    datatype token = LBrace
                   | RBrace
                   | LParen
                   | RParen
                   | Plus
                   | Minor
                   | Equal
                   | Not
                   | And
                   | Or
                   | Impl
                   | Assign
                   | Comp
                   | Skip
                   | While
                   | Do
                   | If
                   | Then
                   | Else
                   | End
                   | Num  of int
                   | Var  of string
                   | Bool of bool

    val scan     : string -> token list
    val toString : token list -> string
  end

structure Lexer :> LEXER =
  struct
    datatype token = LBrace
                   | RBrace
                   | LParen
                   | RParen
                   | Plus
                   | Minor
                   | Equal
                   | Not
                   | And
                   | Or
                   | Impl
                   | Assign
                   | Comp
                   | Skip
                   | While
                   | Do
                   | If
                   | Then
                   | Else
                   | End
                   | Num  of int
                   | Var  of string
                   | Bool of bool

    fun split f cs =
      let val s = Substring.full (String.implode cs)
          val (ls, rs) = Substring.splitl f s
      in (Substring.string ls, Substring.explode rs)
      end

    fun numTok cs =
      let val (s, cs) = split Char.isDigit cs
          val n = valOf (Int.fromString s)
      in  (Num n, cs)
      end

    fun alphaTok cs =
      let val (s, cs) = split Char.isAlphaNum cs
          fun makeTok "skip"  = Skip
            | makeTok "while" = While
            | makeTok "do"    = Do
            | makeTok "if"    = If
            | makeTok "then"  = Then
            | makeTok "else"  = Else
            | makeTok "end"   = End
            | makeTok "true"  = Bool true
            | makeTok "false" = Bool false
            | makeTok id      = Var id
      in  (makeTok s, cs)
      end

    fun tokenize acc []                   = List.rev acc
      | tokenize acc (#"{" :: cs)         = tokenize (LBrace :: acc) cs
      | tokenize acc (#"}" :: cs)         = tokenize (RBrace :: acc) cs
      | tokenize acc (#"(" :: cs)         = tokenize (LParen :: acc) cs
      | tokenize acc (#")" :: cs)         = tokenize (RParen :: acc) cs
      | tokenize acc (#"+" :: cs)         = tokenize (Plus :: acc) cs
      | tokenize acc (#"<" :: cs)         = tokenize (Minor :: acc) cs
      | tokenize acc (#"=" :: cs)         = tokenize (Equal :: acc) cs
      | tokenize acc (#"~" :: cs)         = tokenize (Not :: acc) cs
      | tokenize acc (#"&" :: cs)         = tokenize (And :: acc) cs
      | tokenize acc (#"|" :: cs)         = tokenize (Or :: acc) cs
      | tokenize acc (#"-" :: #">" :: cs) = tokenize (Impl :: acc) cs
      | tokenize acc (#":" :: #"=" :: cs) = tokenize (Assign :: acc) cs
      | tokenize acc (#";" :: cs)         = tokenize (Comp :: acc) cs
      | tokenize acc (all as c :: cs)     =
          if Char.isDigit c then
            let val (tok, cs) = numTok all
            in  tokenize (tok :: acc) cs
            end
          else if Char.isAlpha c then
            let val (tok, cs) = alphaTok all
            in  tokenize (tok :: acc) cs
            end
          else
            tokenize acc cs

    fun scan input = tokenize [] (String.explode input)

    fun toString [] = ""
      | toString (LBrace :: toks) = "{" ^ toString toks
      | toString (RBrace :: toks) = "}" ^ toString toks
      | toString (LParen :: toks) = "(" ^ toString toks
      | toString (RParen :: toks) = ")" ^ toString toks
      | toString (Plus :: toks)   = " + " ^ toString toks
      | toString (Minor :: toks)  = " < " ^ toString toks
      | toString (Equal :: toks)  = " = " ^ toString toks
      | toString (Not :: toks)    = "~" ^ toString toks
      | toString (And :: toks)    = " & " ^ toString toks
      | toString (Or :: toks)     = " | " ^ toString toks
      | toString (Impl :: toks)   = " -> " ^ toString toks
      | toString (Assign :: toks) = " := " ^ toString toks
      | toString (Comp :: toks)   = "; " ^ toString toks
      | toString (Skip :: toks)   = "skip" ^ toString toks
      | toString (While :: toks)  = "while " ^ toString toks
      | toString (Do :: toks)     = " do " ^ toString toks
      | toString (If :: toks)     = "if " ^ toString toks
      | toString (Then :: toks)   = " then " ^ toString toks
      | toString (Else :: toks)   = " else " ^ toString toks
      | toString (End :: toks)    = " end" ^ toString toks
      | toString (Num n :: toks)  = Int.toString n ^ toString toks
      | toString (Var id :: toks) = id ^ toString toks
      | toString (Bool b :: toks) = Bool.toString b ^ toString toks
  end
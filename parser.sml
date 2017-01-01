signature PARSER = 
sig
  exception SyntaxError of string
  val read : string -> Hoare.form
end

structure Parser :> PARSER = 
struct
  structure L = Lexer
  structure H = Hoare
  exception SyntaxError of string

  fun parseNExp []                 = raise SyntaxError "Error parsing numeric expression: incomplete input"
    | parseNExp (L.Num n :: toks)  = parseArithOp (H.Num n, toks)
    | parseNExp (L.Var id :: toks) = parseArithOp (H.Var id, toks)
    | parseNExp (L.LParen :: toks) = let val (m, toks) = parseNParen toks
                                     in  parseArithOp (m, toks)
                                     end
    | parseNExp toks               = raise SyntaxError ("Numeric expression expected. " ^ L.toString toks)

  and parseArithOp (m, L.Plus :: toks) = let val (m', toks) = parseNExp toks
                                         in  (H.Plus (m, m'), toks)
                                         end
    | parseArithOp (m, toks)           = (m, toks)

  and parseNParen toks =
    let val (m, L.RParen :: toks) = parseNExp toks
    in  (m, toks)
    end
    handle Bind => raise SyntaxError ("Error parsing numeric expression: right parenthesis expected. " ^ L.toString toks)

  fun parseBExp []                     = raise SyntaxError "Error parsing boolean expression: incomplete input"
    | parseBExp (L.Num n :: toks)      = parseBExpTail (parseRelOp (H.Num n, toks))
    | parseBExp (L.Var id :: toks)     = parseBExpTail (parseRelOp (H.Var id, toks))
    | parseBExp (L.Bool true :: toks)  = parseBExpTail (H.Bool true, toks)
    | parseBExp (L.Bool false :: toks) = parseBExpTail (H.Bool false, toks)
    | parseBExp (L.Not :: toks)        = let val (b, toks) = parseBExp toks
                                         in  parseBExpTail (H.Not b, toks)
                                         end
    | parseBExp (L.LParen :: toks)     = let val (b, toks) = parseBExpParen toks
                                         in  parseBExpTail (b, toks)
                                         end
    | parseBExp toks                   = raise SyntaxError ("Boolean expression expected. " ^ L.toString toks)

  and parseBExpTail (b, [])                      = (b, [])
    | parseBExpTail (b, all as L.Do :: toks)     = (b, all)
    | parseBExpTail (b, all as L.Then :: toks)   = (b, all)
    | parseBExpTail (b, all as L.RParen :: toks) = (b, all)
    | parseBExpTail (b, L.RBrace :: toks)        = (b, toks)
    | parseBExpTail (b, L.And :: toks)           = let val (b', toks) = parseBExp toks
                                                   in  (H.And (b, b'), toks)
                                                   end
    | parseBExpTail (b, L.Or :: toks)            = let val (b', toks) = parseBExp toks
                                                   in  (H.Or (b, b'), toks)
                                                   end
    | parseBExpTail (b, L.Impl :: toks)          = let val (b', toks) = parseBExp toks
                                                   in  (H.Impl (b, b'), toks)
                                                   end
    | parseBExpTail (_, toks)                    = raise SyntaxError ("Unexpected token after boolean expression. " ^ L.toString toks)

  and parseRelOp (m, L.Minor :: toks)  = let val (m', toks) = parseNExp toks
                                         in  (H.Minor (m, m'), toks)
                                         end
    | parseRelOp (m, L.Equal :: toks)  = let val (m', toks) = parseNExp toks
                                         in  (H.Equal (m, m'), toks)
                                         end
    | parseRelOp (_, toks)             = raise SyntaxError ("Relational operator expected. " ^ L.toString toks)

  and parseBExpParen toks =
    let val (b, L.RParen :: toks) = parseBExp toks
    in  (b, toks)
    end
    handle Bind => raise SyntaxError ("Error parsing boolean expression: right parenthesis expected. " ^ L.toString toks)

  fun parseProg []                             = raise SyntaxError "Error parsing program: incomplete input"
    | parseProg (L.Skip :: toks)               = parseProgTail (H.Skip, toks)
    | parseProg (L.Var id :: L.Assign :: toks) = let val (m, toks) = parseNExp toks
                                                 in  parseProgTail (H.Assign (id, m), toks)
                                                 end
    | parseProg (L.While :: toks)              = let val (pWhile, toks) = parseProgWhile toks
                                                 in  parseProgTail (pWhile, toks)
                                                 end
    | parseProg (L.If :: toks)                 = let val (pIf, toks) = parseProgIf toks
                                                 in  parseProgTail (pIf, toks)
                                                 end
    | parseProg toks                           = raise SyntaxError ("Program expected. " ^ L.toString toks)
    
  and parseProgTail (p, all as L.Else :: toks) = (p, all)
    | parseProgTail (p, all as L.End :: toks)  = (p, all)
    | parseProgTail (p, L.LBrace :: toks)      = (p, toks)
    | parseProgTail (p, L.Comp :: toks)        = let val (p', toks) = parseProg toks
                                                 in  (H.Comp (p, p'), toks)
                                                 end
    | parseProgTail (_, toks)                  = raise SyntaxError ("Unexpected token after program. " ^ L.toString toks)

  and parseProgWhile toks =
    let val (g, L.Do :: toks) = parseBExp toks
        val (p, L.End :: toks) = parseProg toks
    in  (H.While (g, p), toks)
    end
    handle Bind => raise SyntaxError ("Error parsing while statement. " ^ L.toString toks)

  and parseProgIf toks =
    let val (g, L.Then :: toks) = parseBExp toks
        val (p1, L.Else :: toks) = parseProg toks
        val (p2, L.End :: toks) = parseProg toks
    in  (H.If (g, p1, p2), toks)
    end
    handle Bind => raise SyntaxError ("Error parsing if statement. " ^ L.toString toks)

  fun parseForm []                 = raise SyntaxError "Empty input"
    | parseForm (L.LBrace :: toks) = let val (pre, toks) = parseBExp toks
                                         val (p, toks) = parseProg toks
                                         val (post, toks) = parseBExp toks
                                     in  H.Triple (pre, p, post)
                                     end
    | parseForm toks               = let val (b, _) = parseBExp toks
                                     in  H.Prop b
                                     end
  
  fun read input = parseForm (L.scan input)
end
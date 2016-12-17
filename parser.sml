signature PARSER = 
sig
	exception InvalidArgument of string

	(* val parse : unit -> Hoare.hoare *)
	val parse : unit -> string * string * string
end

structure Parser :> PARSER = 
struct
	exception InvalidArgument of string

	fun parse () =
		let
			val input = Utils.readLine TextIO.stdIn
			val toks = String.tokens (fn x => x = #"|") input
		in
			(List.nth (toks, 0), List.nth (toks, 1), List.nth (toks, 2))
		end
		handle Subscript => raise InvalidArgument "Invalid input"
		
end;

(* test *)
Parser.parse ()
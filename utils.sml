signature UTILS = 
sig
	val trim     : string -> string
	val compare  : string * string -> bool
	val readLine : TextIO.instream -> string
end

structure Utils :> UTILS = 
struct
	local
		fun trimEnd nil accum = rev accum
			| trimEnd (#"\n"::t) accum = rev accum
			| trimEnd (h::t) accum = trimEnd t (h::accum)
	in
		fun trim s =
			String.implode (trimEnd (String.explode s) nil)
	end

	fun compare (s1, s2) =
		case String.compare (s1, s2)
			of LESS => false
			| EQUAL => true
			| GREATER => false

	local
		fun toString NONE = ""
			| toString (SOME s) = s
	in
		fun readLine stream =
			trim (toString (TextIO.inputLine stream))
	end
end
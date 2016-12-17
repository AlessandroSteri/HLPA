signature CONTROLLER = 
  sig
  	val goal     : string -> unit
  	val by       : Rule.tactic * int -> unit
  	val pr       : Rule.state -> unit
  	val getState : unit -> state
  end
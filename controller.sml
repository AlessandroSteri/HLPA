signature CONTROLLER = 
  sig
    val goal     : string -> unit
    val by       : int -> Rule.tactic -> unit
    val pr       : Rule.state -> unit
    val getState : unit -> Rule.state
  end
signature CONTROLLER = 
  sig
  (*val goal     : string -> unit*)
    val goal     : Hoare.form -> unit
    val by       : Rule.tactic -> unit
    val pr       : Rule.state -> unit
    val getState : unit -> Rule.state
  end

structure Controller :> CONTROLLER = 
  struct
    val currState = ref (Rule.init (Hoare.Prop(Hoare.Meta " No goal yet!")))

    fun printGoals (_, []) = ()
      | printGoals (n, f::fs) = (DisplayHoare.displayForm n f; printGoals (n+1, fs));

    fun pr st =
        let val fs = Rule.getForms st
        in if Rule.isFinal st then print "No subgoals left!\n"
           else printGoals(1,fs)
        end

    fun setState state = (pr state; currState := state)

      (*val goal = setState o Rule.init o ParseFol.read*)
    fun by tac = setState(tac (!currState))
               (*Devono essere gestite tutte le eccezioni*)
    	           handle _ => print "** Tactic FAILED! **\n"

    fun goal form = setState (Rule.init form) 
    fun getState() = !currState
  end
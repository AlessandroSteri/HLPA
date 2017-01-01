signature CONTROLLER = 
  sig
    val pr       : Rule.state -> unit
    val getState : unit -> Rule.state
    val goal     : string -> unit
    val by       : Rule.tactic -> unit
    val undo     : unit -> unit
  end

structure Controller :> CONTROLLER = 
  struct
    val states = ref [Rule.init (Hoare.Prop (Hoare.Meta "No goal yet!"))]

    fun pr state =
      let val forms = Rule.getForms state
          fun prForms (_, [])     = ()
            | prForms (n, h :: t) = (Printer.prForm n h; prForms (n + 1, t))
      in  if Rule.isFinal state then print "No subgoals left! Milner says: <<Good job bro!>>\n"
          else prForms (1, forms)
      end

    fun getState () = hd (!states)

    fun goal input =
      let val state = Rule.init (Parser.read input)
      in  pr state; states := [state]
      end
      handle Parser.SyntaxError msg => print (msg ^ "\n")

    fun pushState state = (pr state; states := state :: !states)

    fun popState () =
      let val state = getState ()
      in  states := tl (!states); state
      end

    fun by tac =
      pushState (tac (getState ()))
      handle Rule.InvalFormIndex => print "Out of range index!\n"
           | Rule.InvarError     => print "Cannot find loop invariant!\n"
           | _                   => print "Tactic failed!\n"

    fun undo () =
      if length (!states) = 1 then print "Cannot undo main goal!\n"
      else pr (popState ())
  end
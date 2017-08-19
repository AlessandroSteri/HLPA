# HLPA
Hoare Logic Proof Assistant in SML

## Overview
HLPA supports the user in prooving partial correctness of a program written in an imperative language called *Imp* defined in te section **Abstract Syntax**.

In order to do so, HLPA takes as input a formula *F* of Hoare's logic wich represents the formal specification of the program, and help the user to create the derivation tree from the original formula *F* by aplying the rule of inference, called *tatics*, selected by the user himself.

Any time during execution, HLPA keeps in memory the current state of the proof. Such a state consist of a list of formulas that reppresent the leafs of derivation tree builded up till that moment.

Th einitial state *s0* of the proof contains only the formula *F* provided as input. Starting from a state *S* the user generates the following state *S'* specifyng a tactic *T* to apply and a formula f ∈ *S* to be the target of the tattic *T*..
The state *S'* is obtained by subtituting f in *S* with the premises derived by *T(f)*.
If a formula in a state is an axiom, can be removed since is truts holds with no assumptions, this entails that *F* is proved when is reached an empty state.

Some tatics introduce meta-variable (placeholder), where the tactic can't compute the precondition or postcondition inside of a formula.

Mooving forward in the proof is it possible to insert expressions when such precondition or postcondition appear clear.

## HLPA Structure
HLPA consist of 6 modules:
* Hoare: defines abstract sintax of the *Imp* language and of the Hoare formulas. It also includes utilities functions to handling formulas.
* Lexer: implements a lexer that tokenize the formula *F* passed in input as string.
* Parser: translates the output of the lexers in formulas by applyng contructors from abstract systax.
* Printer: converts formulas in printable string.
* Rule: defines the functions that implements tatics.
* Controller: expose the methods to drive the proof.

## Abstract Syntax
The Abstract Syntax is represented by the following datatypes:

```ML
     datatype numExp = Num  of int
                     | Var  of string
                     | Plus of numExp * numExp

    datatype boolExp = Bool    of bool
                     | Not     of boolExp
                     | Meta    of string
                     | MetaVal of string * boolExp
                     | And     of boolExp * boolExp
                     | Or      of boolExp * boolExp
                     | Impl    of boolExp * boolExp
                     | Minor   of numExp * numExp
                     | Equal   of numExp * numExp

       datatype prog = Skip
                     | Comp   of prog * prog
                     | Assign of string * numExp
                     | While  of boolExp * prog
                     | If     of boolExp * prog * prog

       datatype form = Prop   of boolExp
                     | Triple of boolExp * prog * boolExp

```
The cinstructor *Meta* defines a meta-variable *_a* as a placesholder for a boolean expression inside a formula in the case it can't be computed by tactics in the current state.
When you apply a tactic to a formula that contains a meta-variable *_a* in such a state that is possible to compute the boolean exp that *_a* is holdimg the place for, you can define by the constructor *MetaVal* a meta-value that is a parir *(a, exp)* that associates  the computed boolean expression exp to the meta-variable *_a* and is written in HLPA as *a : exp*.

## Tactics
TODO
# Instructions

Vedere Guida pratica all'utilizzo

## Practical Guide
In this section we describe the exposed interface of **Controller** that let's you drive the proof, followed by a pratical example of proof.
* goal: takes as input a Hoare's triple as a string and inizialize the state.
* by: takes as input a tactic and the index of the target formula and update the state with the new premises. In case of failure give an oppoprtune message. tacMeta cant be passed as argument of by, must be used by calling Meta.
* meta: takes as input the name of a meta-variable and a boolean expression and invokes tacMeta
* pr: prints current state.
* undo: undoes the latest change to the state restoring the previous state.
* getState: returns the current state.

As example of usage we will show how to derive the folloving Hoare's triple *F*:
```bash
{x=1} skip; if(x<0) then x := x+1 else x := x+2 end {x=2}
```
Open the terminal in the project folder and start ML REPL:
```bash
Standard ML of New Jersey v110.78 [built: Thu Jul 23 11:21:58 2015]
-
```
Open HLPA files:
```bash
Standard ML of New Jersey v110.78 [built: Thu Jul 23 11:21:58 2015]
-  use "files.sml";
...
```
Open structure Controller to pbtain the interface:
```bash
- open Controller;
opening Controller
  val pr : unit -> unit
  val getState : unit -> Rule.state
  val goal : string -> unit
  val by : Rule.tactic -> unit
  val meta : string -> string -> unit
  val undo : unit -> unit
-
```
Then insert the formula *F* using goal (at each step of the proof HLPA will print the current state of the proof enumering each formula in it, at this moment just the initian one):
```bash
- goal "{x=0}skip;if(x<0)then x:=x+1 else x:=x+2 end{x=2}";
1. {x = 0}skip; if (x < 0) then x := x + 1 else x := x + 2 end{x = 2}
val it = () : unit
-
```
Let's apply the tactic *tacComp* on the only formula we have, in order to do so we use the function *by*, and since tacComp is a meta rule it add the meta-variable *_a*:
```bash
- by(Rule.tacComp 1);
1. {x = 0}skip{_a}
2. {_a}if (x < 0) then x := x + 1 else x := x + 2 end{x = 2}
val it = () : unit
-
```

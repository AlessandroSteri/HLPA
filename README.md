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
There are two types of tactic:
* base tactic: does not introduce meta-variable
  *  tacAxiom, tacSkip, tacAssign, tacWhile, tacIf.
* meta tactic: does introduce meta-variable
  * tacStr, tacWeak, tacComp

The tacAxiom tactic verify that the target formula is an axiom and in such a case it remove the formula from the current state.

tacSkip and tacAssign are axioms but they differ from tacAxiom since they are used to compute preconditions or postconditions when a meta-variable is used as a placeholder instead of the corresponding expression.

All the other tactic are simply an implementation of the matching rule of inference.

There are other two special tactic:
* tacNorm
* tacMeta

tacNorm applied to a formula that contains a meta-value of the form * a : exp* substitute con *exp* all the occurrency of meta-variable and  meta-value named *_a* in the current state.
This process is called normalization.

tacMeta takes as arguments a string *_a* and a boolean expression *exp* and substitute with exp all the meta-variable and meta-value named *_a* in the current state allowing the user to provide manually the value associated to a meta-variable.

## Practical Guide
In this section we describe the exposed interface of **Controller** that let's you drive the proof, followed by a pratical example of proof.
* goal: takes as input a Hoare's triple as a string and inizialize the state.
* by: takes as input a tactic and the index of the target formula and update the state with the new premises. In case of failure give an oppoprtune message. tacMeta cant be passed as argument of by, must be used by calling Meta.
* meta: takes as input the name of a meta-variable and a boolean expression and invokes tacMeta
* pr: prints current state.
* undo: undoes the latest change to the state restoring the previous state.
* getState: returns the current state.

As example of usage we will show how to derive the folloving Hoare's triple *F*:
```
{x=1} skip; if(x<0) then x := x+1 else x := x+2 end {x=2}
```
Open the terminal in the project folder and start ML REPL:
```
Standard ML of New Jersey v110.78 [built: Thu Jul 23 11:21:58 2015]
-
```
Open HLPA files:
```
Standard ML of New Jersey v110.78 [built: Thu Jul 23 11:21:58 2015]
-  use "files.sml";
...
```
Open structure Controller to pbtain the interface:
```
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
```
- goal "{x=0}skip;if(x<0)then x:=x+1 else x:=x+2 end{x=2}";
1. {x = 0}skip; if (x < 0) then x := x + 1 else x := x + 2 end{x = 2}
val it = () : unit
-
```
Let's apply the tactic *tacComp* on the only formula we have, in order to do so we use the function *by*, and since tacComp is a meta rule it add the meta-variable *_a*:
```
- by(Rule.tacComp 1);
1. {x = 0}skip{_a}
2. {_a}if (x < 0) then x := x + 1 else x := x + 2 end{x = 2}
val it = () : unit
-
```
Since now in the current state there are two premises you can choose wich to target by a tactic, let's choose to apply tacIf on formula **2.** :
```
- by(Rule.tacIf 2);
1. {x = 0}skip{_a}
2. {_a & x < 0}x := x + 1{x = 2}
3. {_a & x >= 0}x := x + 2{x = 2}
val it = () : unit
-
```
Let's apply then tacStr on formula **3.** and on the resulting state tacAssign on formula **4.**:
```
- by(Rule.tacStr 3);
1. {x = 0}skip{_a}
2. {_a & x < 0}x := x + 1{x = 2}
3. _a & x >= 0 -> _b
4. {_b}x := x + 2{x = 2}
val it = () : unit
- by(Rule.tacAssign 4);
1. {x = 0}skip{_a}
2. {_a & x < 0}x := x + 1{x = 2}
3. _a & x >= 0 -> _b
4. {_b : x + 2 = 2}x := x + 2{x = 2}
val it = () : unit
-
```
* (Note that meta-variabile *_b* become meta-value *b : x + 2 = 2* when it's computed by the tactic tacAssign)

Let's then normilize the meta-variable *_b* using tacNorm; this means that all the occurrency of *_b* will become the expression x + 2 = 2:

```
- by(Rule.tacNorm 4);
1. {x = 0}skip{_a}
2. {_a & x < 0}x := x + 1{x = 2}
3. _a & x >= 0 -> x + 2 = 2
4. {x + 2 = 2}x := x + 2{x = 2}
val it = () : unit
-
```
Since formula **4.** is an axiom can be remooved with tacAxiom:

```
- by(Rule.tacAxiom 4);
1. {x = 0}skip{_a}
2. {_a & x < 0}x := x + 1{x = 2}
3. _a & x >= 0 -> x + 2 = 2
val it = () : unit
-
```
Let's observe that formula **3.** isn't an Hoare's triple!It is an aritmetic expression, in order to remoove it as an axiom the meta-variable *_a* must acquire a value. Such a value must be provided by the user using the function meta:
```
- meta "_a" "x = 0";
1. {x = 0}skip{x = 0}
2. {x = 0 & x < 0}x := x + 1{x = 2}
3. x = 0 & x >= 0 -> x + 2 = 2
val it = () : unit
```
* Note that in such a case the normalizing process is automatic.

Starting from the latest state, and applying in sequence
* by(Rule.tacStr 2)
* by(Rule.tacAssign 3)
* by(Rule.tacNorm 3)
the following state is obtained:

```
- pr();
1. {x = 0}skip{x = 0}
2. x = 0 & x < 0 -> x + 1 = 2
3. {x + 1 = 2}x := x + 1{x = 2}
4. x = 0 & x >= 0 -> x + 2 = 2
val it = () : unit
-
```
* Note the use of function pr

Obviously all four formulas are axioms and can be easily remooved by tacAxiom.
Let's suppose the last one we remoove is formula **1.** we can conclude our proof in such a way:
```
- pr();
1. {x = 0}skip{x = 0}
val it = () : unit
- by(Rule.tacAxiom 1);
No subgoals left! Milner says: <<Good job bro!>>
val it = () : unit
-
```

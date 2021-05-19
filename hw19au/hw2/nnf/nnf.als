module nnf

-- A formula has zero or more formulas as children. 
abstract sig Formula { children: set Formula }

-- And, Or, Not, and Var are all formulas.
sig And, Or, Not, Var extends Formula { }  

-- A literal is a Var or a negation of a Var.
fun Literal [] : Formula {
	Var + Not & children.Var
}

-- The set of Booleans consists of two values, True and False. 
enum Boolean { True, False }

-- The following global properties ensure that Formulas are 
-- syntactically well-formed. In particular:
-- (1) A Var has no children.
-- (2) A Not formula has exactly one child.
-- (3) And and Or formulas have more than one child.
-- (4) No formula contains itself (i.e., no cycles in the formula AST).
-- Your specification of these properties should use the multiplicity predicates
-- (some, one, lone, no) rather than cardinality expressions. 
fact wellFormedFormulas {
	-- Encode the four properties as a conjunction of 4 constraints.
	-- You can use more constraints if that's easier, though many more 
	-- shouldn't be necessary.
}

-- Returns true iff the given formula is in NNF. 
pred NNF [f: Formula] {
	--- This can be expressed with 1 constraint.
}

-- Returns true iff the relation V is a function that maps 
-- every Formula f to a Boolean value that is the meaning of f
-- with respect to V's interpretation of f's variables.   
pred valuation[ V: Formula -> Boolean ] {
	--- This can be expressed with 4 constraints.
}

-- Returns true iff V is a valuation that satisfies f.
pred satisfies[V: Formula -> Boolean, f: Formula] {
	--- This can be expressed with 2 constraints.
}

-- Returns the positive set of the valuation V with respect to the formula f.
-- That is, when given a relation v that is a valuation for the formula f, 
-- returns the set of all literals in f that are satisfied by V. 
fun pos[ V: Formula -> Boolean, f: Formula ] : set Formula {
	--- This can be specified as a short relational expression (1 line). 
	--- The following is a **placeholder expression** to let Alloy compile the skeleton spec. 
	--- Replace the placeholder with your solution.
	Formula
}

-- The Monotonicity of NNF theorem from Homework 1: 
-- If f is an NNF formula and V1 and V2 are valuations such that 
-- V1 satisfies f and pos[V1, f] is a subset of pos[V2, f], then V2 satisfies f as well.
pred MonotonicityOfNNF [] {
	all f : Formula, V1, V2: Formula -> Boolean | 
		(NNF[f] and 
		valuation[V1] and valuation[V2] and 
		satisfies[V1, f] and pos[V1, f] in pos[V2, f]) => 
		satisfies[V2, f]
}

-------------------- Quick Start --------------------

-- Below is a set of sample commands you can use to test 
-- your definitions. These tests are not meant to be exhaustive; 
-- you may want to write more to make sure your definitions are correct. 

-- First, select "Glucose" as your solver from Options -> Solver.
-- To run the ith command, select it from the Execute menu.  Or 
-- just Execute -> Execute All to execute all commands.

-- Click on an "Instance found" link (in the right window pane) to see 
-- a visualization of the model (if any) found by executing a command.

-- To get a nicer visualization of a particular model, use the menu 
-- bar to navigate to Theme -> Load Theme and select the 
-- custom "nnf.thm" theme from this file's directory.  The theme 
-- is designed to work well for all sample commands, but it will 
-- have to be loaded separately for each instance.  You can also 
-- get an automatically prettified theme by clicking the "Magic Layout"
-- button.

-- You can drag and move nodes in a visualized graph if 
-- you don't like the way they are automatically laid out.

-- If you are getting unexpected models, consider using the Evaluator 
-- feature to help with debugging.  The Evaluator lets you type 
-- in an Alloy formula or expression, which is then evaluated against 
-- the current model. Note that models will include relations that you 
-- didn't define, with names that start with "$"; these are Skolem 
-- constants.  You can refer to them in your Evaluator queries.


-------------------- Sample Commands --------------------

-- The "run" command searches for a model of the given predicate or Alloy
-- formula enclosed in the braces. The "for" keyword specifies how many
-- instances of each signature (set) may be included in the universe.
-- The "expect 1" or "expect 0" clause is optional: it's a comment that
-- says we are expected the "run" command to have a model or not.  The
-- Alloy Analyzer will report whether the expectation is met or not.

-- Returns true iff f contains each kind of Formula.
pred interesting[f: Formula] {
	let descendents = f.*children | 
		some descendents & And and 
		some descendents & Or and 
		some descendents & Not
}

-- Search for an interesting formula in a universe consisting
-- of at most 8 formulas. A formula is interesting if it contains each
-- different subtype of formula as a descendent.
run interesting for 8 Formula expect 1 

-- Search for formula that is both interesting and in NNF.
showNNF: 
run { 
	some f : Formula | interesting[f] and NNF[f]
} for 8 Formula expect 1 

-- Search for an interesting formula and a valuation. 
showValuation : 
run {
	some f : Formula, V: Formula -> Boolean | 
		interesting[f] and valuation[V]
} for 8 Formula expect 1 

-- Search for an interesting formula and a valuation that satisfies it,
-- and that maps at least one variable to True and at least one to False.
showSAT : 
run {
	some f : Formula, V: Formula -> Boolean | 
		interesting[f] and satisfies[V, f] and Boolean in V[Var]
} for 8 Formula expect 1 


-- Check the validity of the MonotonicityOfNNF theorem in a universe with at
-- most 8 formulas. If a model is found, it represents a counterexample to 
-- the theorem. The "expect 0" clause is optional:  
-- it's a comment that says we are expecting these constraints to have no model.
checkTheorem: 
check { MonotonicityOfNNF } for 8 Formula expect 0 

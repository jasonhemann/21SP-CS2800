---
title: The trinity
date: 2021-03-10
---

## The data-function-induction (DFI)trinity:

### 1. Data definitions give rise to predicates recognizing such definitions. (recognizers)

   We must have a proof that these predicates  terminate. (Otherwise they are inadmissible by theDefinitionalPrinciple.) 

   These predicates' bodies give rise to a recursion scheme, e.g.,
   `tlp` gives rise to the common recursion scheme for iterating over
   a list.
  
### 2.Functions over these data types are defined by using the recursion scheme as a template.

   !! (The function template from F1!!!)

   Templates allow us to define correct functions by assuming that the functionwe are defining works correctly in the recursive case.

### 3.The Induction Principle: 

  Proofs by induction involving such data definitions and functions
  over them should use the same recursion scheme to generate /proof
  obligations/. Proof obligations are the inductive cases of an
  induction scheme turned inside out.

	A & B -> C
	ORRR
	C <- if A and B

### Recursion-induction connection

   Notice also that induction and recursionare tightly related
   (Induction is recursion inside-out)
   
   - when defining recursive functions, we get to assume that the
     function works on smaller inputs

   - when proving theorems with induction, we get to assume that the
     theorem holds on smaller inputs

### Both straightforwardly extend the code/equational reasoning

   For each recursive case, we assume the theorem under any
   substitutions that map the formals to arguments in that recursive
   call. This is about as simple an extension to straight-line code as
   we can imagine.

  Recursion provides us with a significant increase in expressive
  power, allowing us to define functions that are not expressible
  using only straight-line code.

  Non-recursive cases are proven directly.

  Induction provides us with a significant increase in theorem proving
  power over equational reasoning, analogous to the increase in
  definitional power weget when we move from straight-line code to
  recursive code.

## Continued Induction Proofs from last time

# Extended Example

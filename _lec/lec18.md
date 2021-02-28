---
title: More Undecidability/Halting Results + Induction
date: 2021-03-03
---

# Continuation 

# Left off

## `http://www.lel.ed.ac.uk/~gpullum/loopsnoop.html`

## A Third View:

  [Diagonalization
  POV](https://en.wikipedia.org/wiki/Halting_problem#Sketch_of_proof)

## So What? 
 
 It gets worse! 

## What else can we not decide? 


#### Rice's theorem:
   
   Decision problem for any non-trivial extensional property of Turing
   machines is undecidable.

It states that for any non-trivial property, there is no general
decision procedure that, for all programs, decides whether the
partial function implemented by the input program has that
property.

   - Non-trivial – not "all of them" or "none of them"
   - Extensional – not about "does it have 3 "0"s in its source code,
     about how it *behaves*
   
   You can't tell much of anything about every program. 

## Although ACL2 cannot do it for us! 


# Induction begins.

## What is an induction Scheme? 

## Every terminating function gives rise to an induction Scheme

  EDIT: Do list induction, way more interesting. 
  
  ```lisp
  (definec nat-ind (n :nat) :nat
    (if (zp n)
	  0
	  (nat-ind (1- n))))
  ```

  ```
  1. (not (natp n)) ⇒ φ
  2. (natp n) ∧ (zp n) ⇒ φ
  3. (natp n) ∧ (not (zp n)) ∧ φ|((n n-1)) ⇒ φ
  ```

 Separate /induction/ cases from /base cases/. 
 
  1. Expand out all the macros (we know what our macros are).

  2. Find the terminals (doesn't have ifs, not (subexpr of) the test), no casing. Undconditional code execution. 

  3. Maximal non-terminals are the biggest ones. the return calls.

  4. Every terminals has the conditions required to reach it. (how you get down to it)

  5. Set of recursive calls are the ones that have to be executed to execute the terminal. 

  6. Let t1 ... tk be the maximal terminals with corresponding conditions
	  each with its corresponding sequence of recursive calls to f under a subst.
	 
	 ¬ic ⇒ φ
	 For all ti that are basic terminals: ic ∧ ci ⇒ φ
	 For all ti that are recursive terminals: (ic ∧ ci ∧ ∧ 1 ≤ j ≤ |ri| φ|σji) ⇒ φ
	 
     
  
  

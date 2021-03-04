---
title: Induction Schemes
date: 2021-03-04
---

## Induction: more complicated in ACL2

  More complicated than you've seen before. More kinds of data. 
  More concerned with ensuring termination. 

## What is an induction scheme? 

## Q: Where do induction schemes come from? 

## Every terminating function gives rise to an induction scheme


  EDIT: Do `ccc` induction example.
  
  ```lisp
  (definec ccc (a :all) :nat
    (cond
	  ((atom a) 0)
	  (t (+ 1 (ccc (car a)) (ccc (cdr a))))))
	  
  (defunc ccc (a)
	:ic t
	:oc (natp (ccc a))
	(if (atom a) 
		0
		(+ 1 (+ (ccc (car a)) (ccc (cdr a))))))

  ;; Two base cases
  1. (not t) => ϕ  ;; the contract case
  2. (and t (atom a)) => ϕ 
  ;; One inductive case
  3. (and t (not (atom a)) ϕ|((a (car a))) ϕ|((a (cdr a)))) => ϕ 
  
  (∀P1) ^ (∀P2) ^ (∀P3) => (∀ϕ)

  ;;!!!! Does not equal the below, not the same.
  (∀P1 ^ P2 ^ P3 => ϕ)
  ```
## Separate /induction/ cases from /base cases/. 
 
  1. Expand out all the macros 
  2. Find the terminals (doesn't have `if` s, not (subexpr of) the test), no casing. Unconditional code execution. 
  3. Maximal non-terminals are the biggest ones. the return calls.
  4. Every terminal has the conditions required to reach it. (how you get down to it)
  5. Set of recursive calls are the ones that have to be executed to execute the terminal. 
  6. Let `t₁ ... tₖ` be the maximal terminals with corresponding conditions
	 each with its corresponding sequence of recursive calls to `f` under a subst.
	 
	 `¬ic ⇒ φ`
	 For all basic terminals `tᵢ`: `ic ∧ cᵢ ⇒ φ`
	 For all recursive terminals `tᵢ`: `(ic ∧ cᵢ ∧ /\ 1≤j≤|rᵢ| φ|_ {σᵢʲ}) ⇒ φ`


  BTW: List induction is way more interesting. 
  
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
	 
	 - ¬ic ⇒ φ
	 - For all ti that are basic terminals: ic ∧ ci ⇒ φ
	 - For all ti that are recursive terminals: (ic ∧ ci ∧ ∧ 1 ≤ j ≤ |ri| φ|σji) ⇒ φ

# HW Setup Q

## Examples:

   ```lisp 
   (definec f (x :all) :all
     x)
	 
   (definec g (x :tl) :all
     x)
   ```	 

# Fib example: to start w/
```lisp
(definec fib (n :nat) :nat
  (cond
   ((< n 2) n)
   (t (+ (fib (1- n)) (fib (1- (1- n)))))))
```

## To what induction scheme does `fib` give rise?
```lisp
;; 
(not (natp n)) -> \phi

(and (natp n) (< n 2)) -> \phi

(and (natp n) (not (< n 2))
     \phi|((n (- n 1)))
     \phi|((n (- n 2)))
     )
```	 

## Let's (for the moment!) use the ind. scheme for `len`
```lisp
;; ((\phi (natp (fib n))))

(not (natp n)) -> (natp (fib n))

(and (natp n) (< n 2)) -> (natp (fib n))

(and (natp n) (not (< n 2)) (natp (fib (- n 1)))) -> (natp (fib n))

;; Conjecture
(implies (and (natp n) (not (< n 2)) (natp (fib (- n 1))))
	 (natp (fib n)))

;; Contract Completion
(implies (and (natp n)
	      (natp (- n 1))
	      (not (< n 2))
	      (natp (fib (- n 1))))
	 (natp (fib n)))

;; Context
C1 (natp n)
C2 (natp (- n 1))
C3 (not (< n 2))
C4 (natp (fib (- n 1)))

;; Derived Context
D1 (<= 2 n) {C3}

;; Goal
(natp (fib n))

;; Proof
(natp (fib n))
= {Def fib}
(natp
 (cond
  ((< n 2) n)
  (t (+ (fib (1- n)) (fib (1- (1- n)))))))
= {EQR}
(natp (+ (fib (1- n)) (fib (1- (1- n)))))
= {Arith}
(and (natp (fib (1- n))) (natp (fib (1- (1- n)))))
= {C4}
(and t (natp (fib (1- (1- n)))))
= {Taut}
(natp (fib (1- (1- n))))
= {Def fib}
(natp
 (cond
  ((< (- n 2) 2) (- n 2))
  (t (+ (fib (- n 3)) (fib (- n 4))))))

;; ಠ_ಠ
```

## What went wrong?

The shape of the data did not guide the shape of the program!! 

Well, because we didn’t define `fib` using the `Nat` data definition,
so why should the data definition give rise to the induction scheme we
need?

# The shape of the data guides the shape of the program

	"The structure of a program must be based on the structures of all of
	the data it processes." --- Michael Jackson, Structured Programming, 1975

## Why was this drilled from F1? 

  Because of truths from Logic and Comp! This is not something that
  @NEU invented or someone's hobby-horse. This is revealing something.

## Elsewhere else-ways

	"There are certain close analogies between the methods used for
	structuring data and the methods for structuring a program which
	processes that data." -- Hoare, Notes on Structured Programming, 1972

	"Show me your flowcharts and conceal your tables, and I shall continue
	to be mystified. Show me your tables, and I won’t usually need your
	flowcharts; theyll be obvious." --- Brooks, The Mythical Man-Month, 1975 
		
	"Show me your code and conceal your data structures, and I shall
	continue to be mystified. Show me your data structures, and I won’t
	usually need your code; it’ll be obvious." --- Raymond, The Cathedral
	and the Bazaar, 1999

	"The central theme of this book has been the relationship between data
	and program structures. The data provides a model of the problem
	environment, and by basing our program structures on data structures
	we ensure that our programs will be intelligible and easy to
	maintain." --- Michael Jackson, Structured Programming, 1975

# How do we follow the structure of our data? 

## What induction scheme do we get for data definitions? 

Use the definition of the recognizer that `defdata` generates to
determine the induction scheme.

## Try another one

   ```lisp
   (defdata n (oneof '(Z) (cons 'S n)))
   ```

### What do elements of this datatype look like?

   ```lisp
   (Z)
   (S Z)
   (S S S S Z)
   ```

### If ACL2 admits `n` automagically get np (the predicate. The recognizer.)

Arabic numerals aren't structurally recursive in the same way!

  ```
  1. (not (natp n)) ⇒ φ
  2. (natp n) ∧ (zp n) ⇒ φ
  3. (natp n) ∧ (not (zp n)) ∧ φ|((n n-1)) ⇒ φ
  ```

### Playground arithmetic, dartboard arithmetic, is!


   ````
   Me    You
   |||    ||||
   ````

## How do I deal with a `cond` w/cases inside predicate? 

   ```lisp
   (< (cond 
        (ϕ ψ)
		(δ γ)
		(t ζ))
	 τ)
   ```

   ```
   ...
   Proof: 
      (< (cond 
        (ϕ ψ)
		(δ γ)
		(t ζ))
	 τ)
   = {Def. if, ...}
   (if ϕ 
     (< ψ τ)
     (< (cond 
        (ϕ ψ)
		(δ γ)
		(t ζ))
	 τ))
   = {Cn }
      (if ϕ 
		  t
		 (< (cond 
			(ϕ ψ)
			(δ γ)
			(t ζ))
		 τ))
	= {Def. if}
	      (if ϕ 
		  t
		  (if δ 
		 (< (cond 
			(ϕ ψ)
			(δ γ)
			(t ζ))
		 τ)
		 (< (cond 
			(ϕ ψ)
			(δ γ)
			(t ζ))
		 τ))
    = { ...} 
		= {Def. if}
	      (if ϕ 
		  t
		  (if δ 
			  (< γ τ)
			  (< (cond 
				 (ϕ ψ)
				 (δ γ)
				 (t ζ))
			  τ))
	
   ```

   Use `if` axioms, in the "inverse mode" to simplify away. 
   
   ```lisp
   C1. (< ψ τ)
   C2. (implies ¬ϕ (< δ τ))
   C3. (implies ¬ϕ (implies ¬δ (< ζ τ)))
   ```
   
   Then we can use `if` as a way to show that if it holds in any of
   the cases, the logic makes it hold generally.



# Proofs by induction


# PollE Q to start w/

	 
     
  
  

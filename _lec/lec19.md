---
title: Induction Schemes
date: 2021-03-04
---

## Induction: more complicated in ACL2

- You have seen induction, proofs by induction before. 

-  Induction is more complicated in ACL2 than what you've seen before. Why? 
  - More kinds of data. 
  - More concerned with ensuring termination. 

### There's a lot of vocabulary! 

   - "terminals" 
   - "maximal terminals"
   - "schemata"

## What is a /scheme/ generally? 

   The words "scheme" "schema" "schematization" in this context mean
   "a template for." It's the name for an abstraction in a different 
   context.
   
## What is an /induction scheme/? 

   It's a way of abstracting over a particular set of proof
   obligations. What we've abstracted out here is the precise
   conjecture ϕ. 

   Q: Where do induction schemata come from? 

   A: Every terminating function gives rise to an induction scheme.

   For a given function f, the induction scheme f generates is what
   gives us the (generic, parameterized) proof obligations it would
   take to prove some universal formula ϕ.


### Example: natrec 

   We might want to prove the formula `(equal (rec-sum n) (/ (* n (+ n
   1)) 2))`. We could imagine using induction on the function
   `rec-sum`.

  ```lisp 
  (defunc rec-sum (n)
	:ic (natp n)
	:oc (natp (rec-sum n))
	(if (zerop n)
	 0
	 (+ n (rec-sum (1- n)))))
  ```

  ```
  1. (implies (not (natp n)) φ)
  2. (implies (and (natp n) (zp n)) φ)
  3. (implies (and (natp n) (not (zp n)) φ|((n n-1))) φ)
  ```


  BTW: List induction is way more interesting. This is probably to
  what you are used, though: 
  
  
## Many functions give rise to the same induction scheme

  ```lisp
  (defunc nat-ind (n)
    :ic (natp n)
	:oc (natp (nat-ind n))
    (if (zp n)
	  0
	  (nat-ind (1- n))))
  ```

### Example: `ccc` induction scheme
  
  `ccc` is a recursive implementation of the `cons-size` function
  we've seen before.
  
  ```lisp
  (definec ccc (a :all) :nat
    (cond
	  ((atom a) 0)
	  (t (+ 1 (ccc (car a)) (ccc (cdr a))))))
  ```

  I've unrolled it here to a `defunc`, so that I have the input
  contract here in front of me all in one place. I've also [expanded
  out macros](#to-expand-out) in the body. 
  
  ```lisp
  (defunc ccc (a)
	:ic t                                     ;; the contract case 
	:oc (natp (ccc a))
	(if (atom a) 
		0                                     ;; the base case
		(+ 1 (+ (ccc (car a)) (ccc (cdr a)))) ;; the one inductive case
		))
  ```

### The `ccc` induction scheme 

  1. (not t) => ϕ                                                    ;; the contract case
  2. (and t (atom a)) => ϕ                                           ;; the one base case
  3. (and t (not (atom a))  ϕ|((a (car a))) ϕ|((a (cdr a)))) => ϕ    ;; the one inductive case
  
  This is the induction scheme /because/ 

  `(∀P1) ^ (∀P2) ^ (∀P3) => (∀ϕ)`

## How do we find a function's induction scheme? 

### Step-by-step 
 
  1. Expand out all the macros 

  2. Find the /maximal terminals/ 
 
  4. For each maximal terminal, collect the conditions required to
     reach it. (How you get down to that one? What must be true in the
     control logic?)

  5. For each maximal terminal, collect the set of recursive calls
     that have to be executed to execute the terminal.

  6. Let `t₁ ... tₖ` be the maximal terminals with corresponding conditions
	 each with its corresponding sequence of recursive calls to `f` under a subst.
	 
## Separate /induction/ cases from /base cases/. 	 

### Base 
	 `¬ic ⇒ φ`
	 For all basic terminals `tᵢ`: `ic ∧ cᵢ ⇒ φ`
	 
### Recursive 	 

    For all recursive terminals `tᵢ`: `(ic ∧ cᵢ ∧ /\ 1≤j≤|rᵢ| φ|_ {σᵢʲ}) ⇒ φ`

### Example: `remove-all` and `flatten`:

```lisp
(defdata nested-tls (oneof nil
			   (cons atom nested-tls)
			   (cons nested-tls nested-tls)))

(defunc remove-all (x my-nested-list)
  :ic (and (atom x) (nested-tlsp my-nested-list))
  :oc (nested-tlsp (remove-all x my-nested-list))
  (cond
    ((endp my-nested-list) nil)
    ((nested-tlsp (car my-nested-list))
     (cons
      (remove-all x (car my-nested-list))
      (remove-all x (cdr my-nested-list))))
    ((atom (car my-nested-list))
     (if (equal x (car my-nested-list))
	 (remove-all x (cdr my-nested-list))
       (cons (car my-nested-list) (remove-all x (cdr my-nested-list)))))))

(defunc flatten (ls*)
  :ic (nested-tlsp ls*)
  :oc list-of-atomp (flatten ls*)
  (cond
    ((endp ls*) '())
	((nested-tlsp (car my-nested-list))
	 (append (flatten (car ls*)) (flatten (cdr ls*))))
    ((atom (car my-nested-list))
	 (cons (car ls*) (flatten (cdr ls*))))))
```

### Inductive proofs, next time

```
Lemma del-ordered:
(implies (and (lorp x)
              (rationalp e)
              (orderedp x))
         (orderedp (del e x)))

Proof by induction on (tlp x).
|#

Lemma del-ordered-contract:
(implies (not (allp x))
         (implies (and (lorp x)
                       (rationalp e)
                       (orderedp x))
                  (orderedp (del e x))))

Exportation:
(implies (and (lorp x)
              (not (allp x))
              (rationalp e)
              (orderedp x))
         (orderedp (del e x)))

Context:
C1. (lorp x)
C2. (not (allp x))
C3. (rationalp e)
C4. (orderedp x)

Derived Context:
D1. nil { C2, PL }

QED



Lemma del-ordered-base:
(implies (not (consp x))
         (implies (and (lorp x)
                       (rationalp e)
                       (orderedp x))
                  (orderedp (del e x))))

Exportation:
(implies (and (lorp x)
              (not (consp x))
              (rationalp e)
              (orderedp x))
         (orderedp (del e x)))

Context:
C1. (lorp x)
C2. (not (consp x))
C3. (rationalp e)
C4. (orderedp x)

Derived Context:
D1. (== x nil) { Def tlp, C1, C2 }

Goal: (orderedp (del e x))

Proof:
(orderedp (del e x))
= { Def del, D1 }
(orderedp nil)
= { Eval }
t
```

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

	 
     
### To expand out
 
 BTW: If you want to expand out, use `trans1`:

  ```lisp 
  > (trans1
	 '(cond
		((atom a) 0)
		(t (+ 1 (ccc (car a)) (ccc (cdr a))))))
  (IF (ATOM A)
	  0 (+ 1 (CCC (CAR A)) (CCC (CDR A))))
  ```

### Terminals

A maximal terminals are (the biggest) expressions of straight-line
code that you'd execute, after working your way down all the
conditional logic. A /terminal/ (doesn't have `if` s, not (subexpr of)
the test), no casing: unconditionally executed, "stright-line" code.
The maximal terminals are the biggest ones.


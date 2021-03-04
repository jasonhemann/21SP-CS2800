---
title: Induction Schemes
date: 2021-03-04
---

## Induction: more complicated in ACL2

- You have seen induction, proofs by induction before. 

-  Induction is more complicated in ACL2 than what you've seen before. Why? 
  - More kinds of data. 
  - More concerned with ensuring termination. 

## What is an induction /scheme/? 

 The proof obligations to prove a universal formula by reference to a given function definition 

 Q: Where do induction schemes come from? 
 
 A: Every terminating function gives rise to an induction scheme

### There's a lot of vocabulary! 

   - "maximal terminals"

### Example: `ccc` induction scheme
  
  ```lisp
  (definec ccc (a :all) :nat
    (cond
	  ((atom a) 0)
	  (t (+ 1 (ccc (car a)) (ccc (cdr a))))))
  ```

  ```lisp
  (defunc ccc (a)
	:ic t ;; the contract case 
	:oc (natp (ccc a))
	(if (atom a) 
		0 ;; the base case
		(+ 1 (+ (ccc (car a)) (ccc (cdr a)))) ;; the one inductive case
		))
  ```

## Scheme 
  1. (not t) => ϕ  ;; the contract case
  2. (and t (atom a)) => ϕ  ;; the one base case
  3. (and t (not (atom a))  ϕ|((a (car a))) ϕ|((a (cdr a)))) => ϕ    ;; One inductive case
  
  (∀P1) ^ (∀P2) ^ (∀P3) => (∀ϕ)


## How 

### Step-by-step 
 
  1. Expand out all the macros 
  2. Find the terminals (doesn't have `if` s, not (subexpr of) the test), no casing. Unconditional code execution. 
  3. Maximal non-terminals are the biggest ones. the return calls.
  4. Every terminal has the conditions required to reach it. (how you get down to it)
  5. Set of recursive calls are the ones that have to be executed to execute the terminal. 
  6. Let `t₁ ... tₖ` be the maximal terminals with corresponding conditions
	 each with its corresponding sequence of recursive calls to `f` under a subst.
	 
## Separate /induction/ cases from /base cases/. 	 

### Base 
	 `¬ic ⇒ φ`
	 For all basic terminals `tᵢ`: `ic ∧ cᵢ ⇒ φ`
	 
### Recursive 	 

    For all recursive terminals `tᵢ`: `(ic ∧ cᵢ ∧ /\ 1≤j≤|rᵢ| φ|_ {σᵢʲ}) ⇒ φ`

### Natrec Example

BTW: List induction is way more interesting. This is probably to what
you are used, though
  
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


### Example: `flatten` and `remove-all` lists:

```lisp
(defdata nested-tls (oneof nil
			   (cons atom nested-tls)
			   (cons nested-tls nested-tls)))


;; Yes
(check (nested-tlsp '(1 (2 3 4) ((b c)) d)))
(check (nested-tlsp '(() 2 3 ((5)))))
(check (nested-tlsp '()))
(check (nested-tlsp '(a)))

;; Out
(check= (nested-tlsp '(a . d)) nil)
(check= (nested-tlsp 'cat) nil)
(check= (nested-tlsp '((a b . d) e)) nil)
(check= (nested-tlsp '(((a . d)))) nil)

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


(defunc remove-all (x my-nested-list)
  :ic (and (atom x) (nested-tlsp my-nested-list))
  :oc (nested-tlsp (remove-all x my-nested-list))
  (case-match my-nested-list
    (() ())
    (((aa . ad) . d) `(,(remove-all x `(,aa . ,ad)) . ,(remove-all x d)))
    ((!x . d) (remove-all x d))
    ((a . d)  (cons a (remove-all x d))))))
    

(implies (nested-tlp ls) (tlp ls))


(defunc flatten (ls*)
  :ic (nested-tlsp ls*)
  :oc list-of-atomp (flatten ls*)

```

### Inductive proof 

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

	 
     
  
  

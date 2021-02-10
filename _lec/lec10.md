---
title: The Proving Ground
date: 2021-02-10
---


# Today's topics

## On the computational nature of conjectures, 
## using ACL2s to understand conjectures and counterexamples, 
## equational reasoning with nested Boolean operators

# Pick up where we last left. 
 
 (Generally)


# Quiz on PollE (on What?)

# Recap

 `(= (len (cons x y)) (len (cons z y)))`

## How do we get there?

## Do the LHS

```lisp
(len (cons x z))
= { definition of len }
(if (consp (cons x z))
   (+ 1 (len (cdr (cons x (list z)))))
   0))
= { consp axioms }
(if t
   (+ 1 (len (cdr (cons x z))))
   0))
= { if axioms }
 	 (+ 1 (len (cdr (cons x z))))
= { cdr axioms }
   (+ 1 (len z))
```   
## Then what? 

### Two choices: 

  - RHS, do it again, or
  - LHS, as a lemma. Instantiate!

## Lemmas
 - Prove once, use over and over. 
 - We used implicitly symmetry of equality, in reverse!, in using it. 

(Same status as a theorem, just POV.)

```lisp
(len (cons x z))
= { Lemma }
(+ 1 (len z))
= { Lemma with instantiation ((x y)) }
(len (cons y z))
```

# Equality, equivalence relations

## Equality

### Equality is a (trivial kind of) equivalence relation.

### An important, special kind of relationship.

Q: Give me another equivalence relation. Tell me a set, and an equivalence relation on that set. Go. 

E.g. NBA players and "teammate."

What's neat, and what you notice, is that you /partition/ the set into /blocks/. 

## Equivalence relation:
   Reflexive, symmetric, and transitive. 
   These are axioms of equality (really any equivalence relation, by defn of one)

We can use these whenever we want, instantiated.

## Equality axiom scheme for functions: 

For all x_1 ... x_n and y_1 ... y_n, if x_1 = y_1 and ... x_n = y_n, then
f(x_1 ... x_n) =  f(y_1 ... y_n)

## Definitional axioms.

With every function /you/ define, you get some axioms.

### When we define a function we get the axioms:

   When we define a function 
   f :ic 
     :oc 

   we get the axioms:
   IC   =>  ( (f x1 x2 …) = body )
   IC   =>   OC

### Example

(defunc mydiv (x y)
	:input-contract (and (intp x) (intp y) 
					(not (equal y 0)))
	:output-contract (rationalp (mydiv x y))
	(/ x y))

## Built-in functions, built-in axioms.

Axioms for if:
x = nil => (if x y z) = z
x ≠ nil => (if x y z) = y

Axioms for car, cdr:
(car (cons x y)) = x
(cdr (cons x y)) = y

Axiom for consp:
(consp (cons x y)) = t

## Axioms
 - Equality axioms
 - Equality axiom scheme for functions

## built-in axioms for built-in functions
 - Axioms for if:
 - Axioms for car, cdr:
 - Axiom for consp:

### Substitution. 

Important operation. 

## Our proofs, in detail:

Important to have a standardized format. 

### Machine checkable.

### Which is great! Definition of substitution wrong, for about ~20yrs

Hilbert, Ackermann... etc. 

### Proof Format

  ```
  Context: the list of our hypotheses
  Derived context: additional hypotheses that we can easily derive from the context
  Goal: what is left to prove
  Proof: the proof itself, typically in equational form
  ```

There are other parts to a proof that we will see later
Not all parts are mandatory, e.g., derived context is optional, and even context may be sometimes empty
We will see all these rules in detail as we go along

## Set-up a context. 

This is where if you see an implication, how you get there. Write down
all the things that we get to assume, and then show some equality
holds in that context. Here, using it both technical and non-technical.

"In this context." "In this situation" "When the state of the world is like this, then ..."

### Example Context:

   ```
   C1: (tlp x)
   C2: (tlp y)
   C3: (tlp z)
   C4: (endp x)
   Goal:  (aapp (aapp x y) z) = (aapp x (aapp y z))
   Proof:
   (aapp (aapp x y) z)
   = { def aapp }
   (aapp (if (endp x) y (cons (car x) (aapp (cdr x) y))) z)
   = { C4, if axioms }
   (aapp y z)
   = { C4, if axioms }
   (if (endp x) (aapp y z) 
		   (cons (car x) (aapp (cdr x) (aapp y z))))
   = { def aapp }
   (aapp x (aapp y z))
   ```

## Derived Contexts

Derived context are useful information you can get from the actual
context and known axioms (e.g. function definitions)

Without yet involve the goal itself at all.
  
### Axiom: 

D1. (tlp (rest x)) { Def tlp, C1, C3 }
D2. (in a (rest x)) { Def in, C6, C3, C4, PL }
D3. (in a (app (rest x) y)) { C5, D1, D2, MP }


  ```
  (
  (tlp x) & (tlp y) & (tlp z) & (consp x) &
  (aapp (aapp (cdr x) y) z) = (aapp (cdr x) (aapp y z))
  )
  =>
  (aapp (aapp x y) z) = (aapp x (aapp y z))
  ```
  
### Axiom: (tlp x) & (consp x) => (not (endp x))

Derived context are: Useful information you can get from known axioms
and the actual context, without having to involve the goal itself at
all.


## Claim 
(implies (consp x)
         (implies (and (tlp x) (tlp y))
                  (implies (endp x)
                           (equal (aapp x y) (rrev y)))))
  

# Example

(defunc recip (x)
  :ic (and (rationalp x) (not (zip x)))
  :oc (rationalp (recip x))
  (/ x))
  
# What contracts do we get from accepting this definition?

## Our proofs, in detail:

### Proof of what? Start w/a claim


```lisp
(implies (consp x)
         (implies (and (tlp x) (tlp y))
                  (implies (endp x)
                           (equal (aapp x y) (rrev y)))))
```  


### Proof Format

  ```
  Context: the list of our hypotheses
  Derived context: additional hypotheses that we can easily derive from the context
  Goal: what is left to prove
  Proof: the proof itself, (typically) in equational form
  ```

There are other parts to a proof that we will see later
Not all parts are mandatory, e.g., derived context is optional, and even context may be sometimes empty
We will see all these rules in detail as we go along


## Set-up a context. 

### Exportation is one of the things we will do.

This is where if you see an implication, how you get there. Write down
all the things that we get to assume, and then show some equality
holds in that /context/. Here, using it both technical and
non-technical.

Setting up a context is listing the premises. The obvious ones. No
need to try and enumerate an infinite set.

"In this context." "In this situation" "When the state of the world is like this, then ..."

### We may also do some other boolean simplifications. Tautologically equivalent

#### E.g. `A ^ !C => !B` to `A ^ B => C` 

### Example Context:

   ```
   C1: (tlp x)
   C2: (tlp y)
   C3: (tlp z)
   C4: (endp x)
   Goal:  (aapp (aapp x y) z) = (aapp x (aapp y z))
   Proof:
   (aapp (aapp x y) z)
   = { def aapp }
   (aapp (if (endp x) y (cons (car x) (aapp (cdr x) y))) z)
   = { C4, if axioms }
   (aapp y z)
   = { C4, if axioms }
   (if (endp x) (aapp y z) 
		   (cons (car x) (aapp (cdr x) (aapp y z))))
   = { def aapp }
   (aapp x (aapp y z))
   ```



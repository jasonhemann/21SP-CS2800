---
title: Limitations of Boolean Algebra
date: 2021-02-08
---

# A first example: `536 + 487 = 586 + 437`

That maneuver always holds. How would we show it? 

# We know things like this via algebra. 

## Bear in mind: `A ∧ B = B ∧ A`

## What's an algebra? 

  Sure we know of the subject matter from elementary algebra (grade
  school algebra). But we have heard now of boolean algebras,
  algebraic geometry, linear algebra, modern or universal
  algebra. Even if we don't know all that subject matter, we do know
  that algebra is more than just the one subject that we already
  learned before.

### BTW

  ```
   | a |  * | -1 |  1 |
   |---+----+----+----|
   | a | -1 |  1 | -1 |
   | a |  1 | -1 |  1 |

   | a | + (\2) | 1 | 0 |
   |---+--------+---+---|
   | a |      1 | 0 | 1 |
   | a |      0 | 1 | 0 |

   | a | ∪ | ∅ | U |
   |---+---+---+---|
   | a | ∅ | ∅ | U |
   | a | U | U | U |


   | a | ∩ | ∅ | U |
   |---+---+---+---|
   | a | ∅ | ∅ | ∅ |
   | a | U | ∅ | U |
  ```

`max` and `min` 

## Boolean Algebras 

   Non-empty subset of 2^{X} (for a non-degenerate X) closed under
   union, intersection, and complement (wrt X). 

## The simplest Boolean algebras {∅,2ˣ} 

   The simplest Boolean algebras {∅,2ˣ} map to propositional
   logic. Everything or nothing. 2 objects to talk about.
   
### We can handle w/a truth table

## Aside: notice these two things do not mean the same:

 We distinguish based on the usage whether we are writing a universal
 claim, describing a subset of a given universal set/asking for a
 solution (the latter being a special case).

  - `x + y = y + x` (Commutativity)
  - `y = 5/3x + 2` 
  - `20/100 = x/45.50` (Describing a set)

 Framed as decision problems, distinguish existential quantification
 (∃) from universal (∀).


### What about statements in more complex algebras? 

With more than 2 values. In fact, describing areas with
infinitely-many values integers, reals, etc. "Data that are the inputs
to ACL2 programs"


### Counterexample to invalidate, sure. 

Disprove universal claims with counterexamples (not *easy*, but not
the worst). 

### But how to prove? 

### We can't exhaustively list the possibilities. 

### We can try a whole bunch of them, and gain some amount of confidence ...

### Proof is a *finite* object that describes infinite behavior. 

### We will cover 

   - Reasoning about the ACL2s universe
     - Boolean logic, equality, built-in functions, defined functions, 
   - Equational reasoning
     - reasoning about straight-line code
     - how to structure proofs
     - implications & dealing with propositional structure, 
     - After that: Definitions, Termination, Induction, 

## Substitution. 

 Important operation. Critical to what we have done and will do

### Definition of substitution wrong, for about ~20yrs

Hilbert, Ackermann... etc.  (cf. Hilbert-Ackermann "... Logic ..." book)

### How we will check your hw : `http://checker.atwalter.com/`

```

(definec ccc (tr :all) :nat 
  (cond 
    ((atom tr) 0)
    (t (+ 1 (ccc (car tr)) (ccc (cdr tr))))))
    
Lemma Cons-up-tree-node:
  (= (ccc (cons x y)) (+ 1 (ccc x) (ccc y)))
 
Goal: 
  (= (ccc (cons x y)) (+ 1 (ccc x) (ccc y)))

Proof: 
  (ccc (cons x y))
= {Def ccc} 
  (+ 1 (ccc (car (cons x y))) (ccc (cdr (cons x y))))
= {car-cdr axioms}
  (+ 1 (ccc x) (ccc y))

QED

Conjecture 1:
(implies (natp x)
         (implies (natp y)
                  (= (+ y (len2 (cons x z)))
                     (+ 1 y (len2 z)))))

Exportation:
(implies (and (natp x)
              (natp y))
         (= (+ y (len2 (cons x z)))
            (+ 1 y (len2 z))))

Contract Completion:
(implies (and (natp x)
              (natp y)
              (tlp z))
         (= (+ y (len2 (cons x z)))
            (+ 1 y (len2 z))))

Context:
C1. (natp x)
C2. (natp y)
C3. (tlp z)

Goal:
(= (+ y (len2 (cons x z)))
   (+ 1 y (len2 z)))

Proof:
(+ y (len2 (cons x z)))
= {C3, lemma len2-of-cons ((y z))} ; you have to provide substitutions for user-defined lemmas
(+ y 1 (len2 z))
= {Arith, C2}
(+ 1 y (len2 z))

QED
```

# Reflect: 

  - How many rows in the truth table for?

## Yet we showed that this infinite truth table has all Ts
## with only a finite amount of work: our proof

# When we admit a function we get two axioms

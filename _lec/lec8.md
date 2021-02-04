---
title: Decision problems, Complexity; P ?= NP; Reductions, Boolean logic in ACL2s; normal forms; set theory; 
date: 2021-02-04
---

## Decision Problems 

  Let's do one more concept: that of a _decision problem_.

  A decision problem is one where the answer is "yes" or "no". That
  is, a function where the co-domain is a set of exactly two elements
  (because it's isomorphic). In the world of theory of computation,
  there's often little difference between the answer you want out of a
  program and the decision problem version of it. 

  E.g., "How many partitions does this graph have?" vs. "Does this
  graph have at least `k` partitions?"
	
### e.g. satisfiability

  We know that the satisfiability of a boolean formula is a decision
  problem. We have a computable procedure for solving it: the truth
  table. We've used it.

### Reductions

  We frequently use the idea of a decision procedure to help reason
  through other sorts of problems. We use them in one of two ways:
  
  1. We _reduce_ the new problem in terms of the old problem for which
     we already have a decision procedure. This gives us a way to
     solve the new problem. "One way to solve problem P2 is to convert
     every instance of P2 into an instance of P1. Since we already
     know how to solve P1, we can solve P2."

     ```
			   |-\ /---------------|
			   |      Sat?         |-Y
			   |                   |
			   |                   |-N
			   |-------------------|


	     |-------\ /---------------------------------|	
         |       | |  Unsat?                         |
         |       | |                                 |
	     |     |-\ /---------------|                 |
         |     |      Sat?         |-Y-----\ /-------|-Y 
         |     |                   |        X        |
	     |     |                   |-N-----/ \-------|-N
	     |     |-------------------|                 |
         |                                           |
         |                                           |
	     |--------------------------------------------
	 ```
	 
  2. If, by contrast, we've discovered that there is no general
     solution to a certain decision problem PR1. Then, if we can
     reduce PR1 to some problem PR2, we now know there cannot be a
     general solution to the decision problem PR2. For if there were,
     that would smuggle in a solution to PR1. 
	 
## `f : X -> Bool iff { x ∈ X s.t. f(x) } ⊆ X`

  That is all to say that we can use the function from `X` into `Bool`
  to pick out a particular subset of `X`. That's what a Boolean
  function is: it's a way to pick out a subset of all the inputs.

## So finally: instantiation.

We can see a formula `(ϕ -> (ψ -> ϕ))` as a general form describing
all the particular instances of formulae of that shape.

  - `(a -> (b -> a))` 
  - `((d ≡ b) -> (g -> (d ≡ b)))` 
  - `((p -> q) -> ((p -> q) -> (p -> q)))` 
  - `(d -> (d -> d))` 
  
  In fact we can see this formula schema `(ϕ -> (ψ -> ϕ))` as
  describing the _set_ of all its instances. And partially
  instantiating `(ϕ -> ((d ≡ b) -> ϕ))` means that a whole bunch of
  formulae no longer fit this scheme. We've shrunk the set of
  instances that it represents. Instantiation is a kind of
  restriction. 

# Complete Boolean Bases

To say we have a "complete basis" means it's sufficient to support
writing any boolean function. 

## We have `&, v, ≡, =>, ><, !`. Is this a complete basis? 

### How can we know? 

We need a way to transform a description of any boolean function into
an expression using these operators. (This feels like a kind of
reduction.) We know for every boolean function there exists a truth
table that computes it. That truth table might be unreasonably big,
but it exists. That truth table describes, for boolean function `f(x₁
... xₙ)`, the subset of `<x₁, ... ,xₙ>` that satisfy f. And we can
list out those rows of the truth table, one after another

     ```
       (x₁ & x₂ & !x₃ & x₄ & !x₅) v 
       (!x₁ & !x₂ & !x₃ & x₄ & !x₅) v
       ...
     ```

Since this process applies to every Boolean function, we have
demonstrated that our set of operators is a complete
basis. Furthermore, we have demonstrated that we have more than enough
operators. All we needed were `{ &, v, !}`. 

# CNF, DNF, and Normal Form.

The process we describe above produces formula in what logicians call
"disjunctive normal form." It is a big sequence of disjunctions, where
each disjunct is a big sequence of conjunctions of literals.

## Terminology detour: "Normalization"

  Remember what my graduate school advisor taught me: big word, simple
  concept. It's the short words that get you. Cf. "this" in the
  dictionary. So, "normalization" is an easy one too.

## Normalization (re: "Regularization") 
 
  The word "normalization" means to get rid of the oddities. To
  regularize. To make more regimented. 
  
  Here, our process resulted every expression in a "normal form". 

### Detour example: cf. numbers

  As an example, we already know about bringing numbers to at least
  one kind of normal form: scientific notation. In high school physics
  or chemistry, we'd've written the number `918.082` is in normalized
  form is `9.18082 x 10^2`. Why is that the normal form for numbers?
  No real good reason. It's just one that happened to be useful, so
  people settled on it. 
   
  It's the same for us. We brought our boolean expressions into
  something called "disjunctive normal form." People came up with a
  name for it because it's useful, and because there are other
  situations or cases where other kinds of normal forms are more
  appropriate.
  
  E.g. "conjunctive normal form" has the conjunctions first and then
  the disjunctions second. 
  
  There are several reasons having such a regularized form might be
  useful. One is, for instance having to do a proof by cases. Right?
  If you had 8 operators, that's likely 8 cases. So if you can reduce
  the number of cases over which you have to prove, or give yourself a
  better structured data to compute or prove over, then you'd likely
  want to do so.


#### BTW: P vs NP

### Can we do better? 

  So from that discussion might arise a natural next question. Okay,
  can we do better still than this 3-operator basis? 
  
  Via De Morgan's laws, we could, for instance, take each of the inner
  conjuncts, and transform it to a disjunction of negations. This
  would no longer be in our normal form. But we could by that process
  eliminate all of the conjunctions. 
  
  Starting from CNF, we could perform a similar operation to eliminate
  the disjunctions. 
  
  So here we have found two complete 2-operator bases: `{&, !}` and
  `{v, !}`. It turns out that for instance `{&, =>}` is also a
  complete basis, and sometimes minimalistic presentations of logic
  systems actually use this basis. 
  
### Can we do better?

  So, alright. We have several 2-operator bases. And it seems like we
  cannot get rid of either of the remaining operators in any of the
  bases we have constructed so far. We aren't out of tricks yet,
  though.

## How many binary boolean operators are there? 

  Let's find out how many *binary* *boolean* operators there are. How
  would we determine that? Being boolean means that there must be two
  options for what to do for the first argument, and then for each of
  those choices two options for what do for the second argument. 
  
  For each of 4 possible combinations of inputs (`<tt,ff>`, `<tt,tt>`
  ...) there are two possible values for the result. This means `2^4`,
  or 16, possibilities.
  
  At least two of them, called `NAND` and `NOR` (or "slash and dagger"
  notation for the Scheffer stroke and the Quine dagger. Although this
  naming convention is objectively cooler, the former is more
  prevalent in computer science.) These are less common operations in
  everyday thought. `NAND` translates in English to "not both." 
  
## Q: How might we show that `NAND` is a complete basis?
  
  So, there are 1-operator bases. Several of them. There is no
  smallest complete basis. 
  
## Q: Why do electrical engineers bother with `AND` and `OR` gates? Why don't our electronics and logics work in terms of `NAND`s?

## Exercise: Express `(! A) v B` with just `NAND`. 

---
title: A Whole New World 
date: 2021-03-29
---

# Transition

## Programming, Logic, Logic Programming

## Compare and Contrast ACL2 v. LP

# Story and History of LP

# The Background

-   Stoy\'s \"Denotational Semantics\" isn\'t published until 1977
-   They cite Alonzo Church (1956)
-   Pattern matching wasn\'t a thing

## Colmerauer, the Marseilles group

  - Colmerauer was doing NLP
  - [Colmerauer & Roussel](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.85.7438&rep=rep1&type=pdf)


  BTW, if you want to know more, please see: 

  1. [Kowalski](https://www.doc.ic.ac.uk/~rak/papers/the%20early%20years.pdf)
  1. [Cohen](https://dl.acm.org/doi/10.1145/35043.35045)

### "Predicate Logic as a Programming Language" Kowalski 74 IFIP

\"99. In man-machine symbiosis, it is man who must adjust: The machines
can\'t.\" -- Alan Perlis, \"Epigrams on Programming\"

## The semantics of such languages

Two usual kinds of semantics for programs: operational and fix-point
(denotational).

We can take any sentence in predicate logic, and rewrite it in an
equivalent clausal form. The one is satisfiable if the other is.
Skolemize away variables, rewrite it in clausal form

So, let\'s think about predicate logic in this context.

Clausal form {p(a)} ...

Syntax. Signature. function symbols, predicate symbols. We\'re building
meaningless strings, then attaching a mechanism of meaning.

Usual distinctions and relationships of syntax and semantics map onto
the connections between semantics. We can translate any sentence in FOL
into clausal form.

Specifically, into Horn clausal form ... at most one positive literal.

```
{\~p v \~r v \~q v \~t v s v \~d}

{s v \~p v \~r v \~q v \~t v \~d}

{p \^ r \^ q \^ t \^ d -\> s}

{s \<- p \^ r \^ q \^ t \^ d}
```

### Operational Semantics : Proof theoretic.

It\'s unusual at this point in time to think of logic as something you
machine execute. 1965 Robinson.

backwards chaining. Different than the approach of just generate
formulae from axioms, that may be irrelevant.

This is an existing proof procedure.

If I want to know if the theory implies something, that\'s exactly to
view it as a function definition.

Unification, getting the *most* general unifier.

`f(a,b,c) \<- subroutine calls`

Resolution, a theorem-proving procedure.

```
a \^ b \^ c \^ d 
=========================
\~p \^ (p \<- a \^ b \^ c \^ d)
```

Want to make it look like a standard computation in the machines work.

if A \|- p(t1,...,tn), then there\'s a refutation of A & not p(x1,...xn)
so that it instantiates x1,..xn with terms that have t1,..,tn as an
instance.

### Intermediate step -- Model-theoretic semantics :

Now, let\'s talk about grafting a *meaning* onto this syntax. Herbrand
Universe - ground terms Herbrand Base - ground predicates Herbrand
interpretation - subset of Herbrand base. clauses are true when ....,
sentences are true when .... A set of clauses is true in I, iff each c
is true in I.

If a set of clauses A is true in I, then we say I models A.

Think court fact pattern, and then tell the story.

A sentence in clausal form has a model iff it has a Herbrand model.
\"Skolem-Lowenheim theorem\".

`A \|= P(t1,...tn) `

"every interpretation that models A also models p(t)"

...

So, we can get to, least Herbrand model. It\'s time-consuming and
difficult to check whether something is true in all models. May be
impossible to do exhaustively, if there\'s infinitely many elements. Now
we just have to check one model.

The operational semantics are a complete proof procedure.

Completeness means you \"get everything\", soundness means \"you don\'t
go astray\".

### Fix-point semantics

How fix-point semantics work generally.

The T^(\_)^. what\'s called the \"Imm. Conseq. fn\".

Think of these as the meanings of all the predicates.

T~P~, the function that maps sets of facts to the sets of facts that the
rules of the program imply. You keep going until, at the limit, there\'s
a complete set of things that the program implies.

Think of the immediate consequence operation as the result of
substituting arguments into the body of a function.

C(A) are the Herbrand interpretations closed under T(\_). That is, we
don\'t generate a contradiction under the transformation.

The consequence-closed interpretations of A. Consistent are exactly the
Herbrand Models of A.

### Skolemization

## miniKanren vs. Prolog

## miniKanren: it's like Racket

### `==`

### `run`

### `fresh`

### `conde`

### recursion

### `appendo`





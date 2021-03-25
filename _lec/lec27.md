---
title: SAT, DIMACS, DPLL
date: 2021-03-25
---

## Every one of your groups should be meeting w/1 of the designated staff members!!!

Half the week has gone by, but we have not seen half of the groups. We
need to meet w/each and every group to OK a sign-off, or work through
an idea w/you. This /is/ a part of your grade, this is a requirement,
please do not let this slip by.

## Last time:

We recapitulated that SAT solvers solve instances of the SAT problem,
finding a satisfying assignment to propositional variables. One of the
major driving forces in this area is the `DPLL` algorithm, a basis for
many of the modern solvers.

We learned that SMT solvers replace propositional variables with
formulae in some known theory. An extension, `DPLL(T)` (again `T` for
"theory", the approach parameterized by a particular theory)
essentially transforms SMT problems into SAT problems. (One of) the
trick is to check consistency against the theory, and in the case of
failure, return a /reason/ for that failure in a way that can help the
SAT solver. 

## DPLL Algorithm
 
 - [Davis](https://en.wikipedia.org/wiki/Martin_Davis_(mathematician))
 - [Putnam](https://en.wikipedia.org/wiki/Hilary_Putnam)
 - Logemann
 - Loveland
 
 Not that the others don't have wiki pages, but I know Davis and
 Putnam are famous in their fields for work beyond this, too.
 
 ### "It's gotta be one or the other"
 
For many of the reasons we've already seen, people work on determining
satisfiability of formulae in clausal form.
 
The general backtracking approach to determining satisfiability of a
set of clauses is to simply pick a literal in a clause, assign it a
truth value, then recur trying to satisfy the (now 1 literal simpler)
formula. If it turns out that the simplified formula was not
satisfiable, then try again with that same literal assigned the
opposite truth value. If neither value works, then return unsat.

### DPLL improvements 

So what does the DPLL procedure add to this approach? The clever
choice of literals. An awareness of /singletons/ and /easily
dismissed/ literals. A DFS may wind us up w/a particularly poor
sequence, so let's be smarter. 

### Singletons

When you have a clause $$ ... \{ l \} ... $$, with $$l$$ a single
literal, then the entire formula is satisfiable iff this literal has
the required truth value. So in one way of looking at it, using this
information well cuts the search space in *half*. So we force that
literal to the appropriate truth value, and simplify the formula from
there. You wouldn't probably *start* a problem with these cases, but
you might wind up with one in the middle of solving. This is a great
idea because simplifying viz. one produces another, and so on. 

This technique is called /unit propagation/. Unit, because these are
called "unit clauses"; this use of the term "unit" is in the same
sense as the [unit circle](https://en.wikipedia.org/wiki/Unit_circle)
(the number 1 is sometimes called
"[unity](https://en.wikipedia.org/wiki/1)").

## Easily dismissed literals.

If some variable only occurs in the formula with one /polarity/
(i.e. negative or positive), then there's no question that we can
assign a value to such literals. The formula is satisfiable exactly
when the formulae with that literal removed from every one of the
clauses is satisfiable. This is another simple technique to simplify
the problem.

### Further techniques

They discovered this basic technique in ~1960, and it's still the
basis of most modern SAT implementations. But we've come a long way
since then, too.

- Clause learning 
- backjumping /non-chronological backtracking/
- random restarts
-  heuristics for choices of decision variables, polarity


### DIMACS format

```
c  <comment>
c  <comment>
p <problem type> <num-vars> <num-clauses>
p cnf 3 2
2 3 -1 0
1 -3 0
```
Clauses begin w/the index of the positive literal. Negative nums index negative literals. `0` is the end of clause char. 

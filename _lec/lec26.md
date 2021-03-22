---
title: SAT v. SMT v. CP "Rat's eye view"
date: 2021-03-22
---

## Project Submissions

### Scheduling meeting

[Scheduling](https://v2.waitwhile.com/book/280018279office)

## SAT(-esque!) options:

### One kind of project

#### NB: What you are /not/ implementing

You would not be implementing a SAT solver. Please. There are plenty
of world-class implementations that are *suuupper* duper fast that
have already been implemented.

Yours would suck by comparison.

You are implementing, if you will, a Sudoku solver. Or an 0h-h1
solver. Specifically, you implement it by implementing a
(polynomial-time, many-one) _reduction_.

Karp's [paper on the subject](http://cgi.di.uoa.gr/~sgk/teaching/grad/handouts/karp.pdf)

#### Many-one

For ` A <=_M B` you only get to invoke the solver of B, once, in the end. 

#### Poly-time 

The reduction must be polynomial in the size of the input, or else we
haven't really *reduced* the problem to the problem in question.

### SAT : how?

### DPLL

The main basis for solvers in this space. There are multiple
advancements in this area, driven largely by heuristics and the fact
that industrial, actual instances have more regularity and structure
than random instances. 

![Improvement over time]({{ site.baseurl }}/assets/images/SAT-cactus-plots.jpeg)

#### NP-/easy/

[Solver-aided systems](https://www.youtube.com/watch?v=P7ItR7WgnOk)

### Other space: SAT v. SMT

![SMT]({{ site.baseurl }}/assets/images/SMT-and-friends.jpg)

### Satisfiability, modulo theory. 

Theory? Some interpreted theory with a specialized solver. Normally we
find if something is true under any interpretation. Often we don't
care, and having to encode these behaviors as truth under all
interpretations causes too much work. Instead we can rely on
specialized solvers for given theories, under interpretations.

If you want a background-y ["technical introduction to SMT"
Video](https://www.youtube.com/watch?v=TgAVIqraCHo)

### Examples of what theories? 

#### Bit-vectors 

#### Arrays

#### Integers 

### Extended Example.

[Example]({{ site.baseurl}}/assets/code/zero-h-h-one.py)





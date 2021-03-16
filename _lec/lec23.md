---
title: Reasoning with accumulators
date: 2021-03-15
---


## Introduction 1: For and against accumulators 

As far back as the first assignment, we've discussed programming with
accumulators. This discussion roughly amounted to: You are not allowed
to write your programs with accumulators. At the time this was
something of a dictate from on high. 

"something something additional properties to prove correct something
something accumulator"

So, instead we forced you to think in the naturally recursive style of
programming, where the only parameters I allowed you to modify were
parameters that "shrunk" on the recursive calls upon which you tested
in the base case, and I only permitted you to test on parameters that
you shrunk in the recursion.

We did this even when it was maybe more "natural" for Fundies
programmers to reach for the accumulator, e.g. `bb->nat`. 

We are now in a position to explain more rigorously why we had that
prohibition, and what we meant by "more difficult to prove correct"
and that "additional property we were required to prove" to assert the
accumulator program's correctness.

### Introduction 2: Scylla and Charybdis

The text presents us with two definitions of `reverse`. One version is
simple, clearly correct, and inefficient. We also have another
definition of this function that is more complex, but efficient. This
was an issue of asymptotic complexity. 

#### Where have we seen a similar issue of asymptotic complexity lately? 

`app2-assoc` ? 

#### Continuing 

We have also seen that tail recursive functions can be more _space_
efficient: 

```lisp
(definec plus (n :nat m :nat) :nat
  (cond
    ((zp m) n)
	(t (1+ (plus n (1- m))))))
```

```lisp
;; A secret: We have fused the accumulator and 
;; the first argument into one single value!
(definec plus-acc (n :nat m :nat) :nat
  (cond
    ((zp m) n)
	(t (plus-acc (1+ n) (1- m)))))
```

```
> (trace$ plus)
> (trace$ plus-acc)
> (plus 2 3)
...
> (plus-acc 2 3)
...
```

## How should we proceed? 

In the case of both `reverse` and both `plus` functions, it seems like
we face choosing between two different negatives: opaque or
inefficient.

Like we saw with the `app2-assoc` proof, the equivalence of two kinds
of functions, or two kinds of expressions, permits us to move between
these two different implementations, using whichever is more
convenient. It would be great to prove that the accumulator based
versions are drop-in replacements for the natural recursion versions,
and vica versa.

 CAN
+HAS
CASH

 CAN
+HAZ
CASH

  CAN
* HAZ
 CASH



## The problem w/accumulators 

v001, v002, v003, v004

v001 or v002 or v003 or v004 ... 
v001 ^ (not v101) ^ 


v00, v01, ... v09
v10, ...
.
.
.
v90, ......... v99

. . . . . . .

. . ._. . . .
        | |
. . . . ._. .

. . . . . . ._ 

. . . . . . .

. . . . . . .
  | 

```
(rev2 ls)
  | |
   v
(rev2 (cdr ls))
  | |
   v
...
(rev2 '())
```

vs. 

```
(revt ls acc) ;; IF you start with the right initial value for the acc
  | |
   v
(revt (cdr ls) (do-something acc))
  | |
   v
(revt (cdr (cdr ls)) (do-something (do-something acc)))
  | |
   v
(revt '() (do-something (... (do-something acc))))
```

## The main relationship

```
(rev2t '(c d e) '(b a)) ==   (app2 (app2  (rev2 '(c d e)) '(b)) '(a))
                ~~~~~~       ~~~~~~~~~~~                  ~~~~~~~~~~~
```

```
(rev2t '(c d e) '(b a)) ==   (app2 (rev2 '(c d e)) '(b a))
```

## Project description, status, setup, due dates, examples.

### Project Aim, goals

So, here's my thinking.
  
  - I'd like you to see and experience *using* the tools of formal methods as a part of solving programming exercises. 

  - I want you to experience, writ small, some of what it's like to
    mechanize/prove theorems for a living, or how to add
    SAT/constraint solvers to your personal problem-solving toolbox.

  - I want to do a bit of "authentic assessment" without steering too far into (for we 2800ers) uncharted territory.

  - I'd you all to have some experience with this on an area of your own devise for some quasi-extrensic end. 

  - I want us to deploy the creative process and that same design recipe in this slightly different context. 

I have laid out for you two different kinds of general areas in which
you should work: (1) SAT-reduction projects, or (2) ACL2 theorem
proving projects.

### Option 1 SAT reduction 

Problems in this space share having multiple discrete choices of
answers, all of which you'd have to check to mechanically rule out a
solution. When thinking through the problem yourself, you'll usually
have to backtrack when working out a problem because the first
potential solution you tried ended up not working, and you had to
rearrange. 

I /don't/ want you to select something that's a known, well-solved or
well-explored example and re-do that. 

 Typical well-known examples include:

 - Sudoku
 - Graph coloring
 
*Do* use these to come up with an idea on how to do work in this
space. *Don't* plan to use these as the/a project itself. We will
search, google and github, for prior art describing a solution. 

One easy choice to consider are newer/uncommon/understudied logic
puzzle/game kinds of things, the kind with a finite number of moves at
every stage.

 - [Kenken](https://slate.com/human-interest/2020/11/best-puzzle-books-crosswords-kenken.html)
 - [lists of scores of these sort of puzzles](https://www.puzzle-nonograms.com/) (Scores! of them)!
 - [Mah-jongg and the 15 puzzle](https://arxiv.org/abs/cs/0512049)
 - [nonogram]()
 - [picross]()

Another way to go is to make a slight tweak on some existing problem:
one I know still outstanding is to solve [0h h1]() on the surface of a
cube.

You want to think about "how would I write a program that would make
my instance of this problem true IFF a certain satisfiability problem
holds, and such that the answer to that SAT problem gives me a
solution to mine?"

### Exemplary model submissions

####  For comparison and reference see 

  1. [that article/blog post](https://research.swtch.com/boolean) on Minimal Boolean Formulae
  2. [another]({{ site.baseurl }}/assets/docs/Back_to_Basics_Solving_w_SAT.PDF)
 
#### Student submissions 

  1. [The Crowded Chessboard Problem]({{ site.baseurl }}/assets/docs/last-projects/crowded.pdf)
  2. [DPLL via the Design Recipe]({{ site.baseurl }}/assets/docs/last-projects/htdp-dpll.pdf) [cf]()
  3. [Solving Skeletal Division problems with SAT]({{ site.baseurl }}/assets/docs/last-projects/skeletal-division.pdf) [cf]()
  4. [Blackbox]() [cf](http://www.wikipedia.org/) [cf](https://boardgamegeek.com/thread/185907/its-mastermind-meets-minesweeper) [cf](http://www.youtube.com/) 
  5. [Lights out]({{ site.baseurl }}/assets/docs/last-projects/lightsout.pdf) [cf]() 

### Option 2: ACL2 Theorems

For this option, I strongly suggest you come up with some claims from
properties of one or a couple of small, pure programs. Think about
programs from Fundies I or maybe the first half of Fundies II. (John
Hughes talks about more of these in a video on the enrichment
page). You could also think of questions about some of the earlier
functions from this class.

I want you to come up with something that you can mechanically prove
in ACL2s. The proofs themselves should be a little harder than the
last couple of homeworks. They key difference is I'd want you to use
real ACL2. Pen-and-paper proving these aren't going to suffice---but I
bet that would be a *great* thing to sketch out as you work toward
automating. I want it to have several discrete steps that you can
build to---necessary lemmas---and I hope that these scaffold up, so
that we can go get half a loaf at a time. No "moonshots."

If you work in this area, as you likely now know from doing your own
proofs, the space between trivial, doable, and dissertion-length is
about thiiis far apart. It's far easier to start small and scale up.

I think it's dangerous to, unless you've already been working with
some other theorem prover and its logic, to try to change horses in
midstream. Prof. Tripakis' class used something called Lean last term
and if you know anyone in that class, I think if you compare notes
you'll find that there's plenty of differences, even though Lean and
ACL2 have similar broad aims. Mutatis muntandis Coq, Agda, Isabelle,
HOL, ELF, etc. 

#### Other Project ideas in this space

  1. [Japanese multiplication](https://vm.tiktok.com/J6j9TaW/)
  2. Comparing two different definitions of `zip-lists` recursive and not. 
  3. Russian peasant multiplication 
  4. Type checker, types with STLC 
  5. Lam-calc to deBruijn indices?
  6. DeBruijn `add1`, or that a closed `lambda` calc expr generates a closed DeBruijn index term.
  7. Combinatory logic basis.
  8. Quines? (I don't yet know how to attack this problem)!
  9. [McCarthy reverse]() is way too big
  10. `(implies (sorted-tree tree) (sorted-listp (preorder-walk tree)))`
  11. Permutation question on old/last semester HW assignment
  12. Pascal triangle behavior implementation equivalence.

### Exemplary model pieces: 

#### For comparison and reference, see 
 
 1. [Shankar's Godel incompleteness theorem formalized](https://www.cambridge.org/core_title/gb/112885) is waaaay too much 
 2. Potentially, small parts of almost any work in ACL2 itself. 
 
#### Student submissions 

 1. [Reverse of a list is a permutation](Payne)
 2. [Length of a powerlist](Peter Kim)
 3. [Magnetic Tower of Hanoi](Sean Kelly)
 4. [Zip-unzip round trip](Siddharth Chennan)
 5. [Valid family trees](Kelly Thieren)
 6. [Natural v. tail recursive Max-btree](Justin Bian)
 7. [Natural v. tail recursive triangular numbers](Ryan Jung)
 8. 



# We permit other kinds of projects too!! 

If you *have* some other ideas though---something else on the topic of
logic and computation that's grabbed your attention, or something else
that you're interesting in pursuing, or that's related to the
confluence of your other interests, talk to me! I'd like to help you
author relevant learning experiences! Cross-course and
interdisciplinary work is great! I'd still want it to be something
exploratory, with at least a component of novelty.

If I can say more that's helpful to you, please let me know!



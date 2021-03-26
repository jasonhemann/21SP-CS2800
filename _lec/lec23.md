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

## The problem w/accumulators 


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
  ...
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

1. Be reading to book a proposal review appointment w/us, as outlined: 


[Book your appointment here](https://v2.waitwhile.com/book/280018279office)

Please make sure you get a confirmation email. If you do not get a confirmation email, you do not have an appointment. 


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

### Project area option 1: Encoding in SAT/SMT/CP

Problems in this space share having multiple discrete choices of
answers, all of which you'd have to check to mechanically rule out a
solution. When thinking through the problem yourself, you'll usually
have to backtrack when working out a problem because the first
potential solution you tried ended up not working, and you had to
rearrange. 

I /don't/ want you to select something that's a known, well-solved or
well-explored example and re-do that. 

 Typical well-known examples include:

 - [Sudoku](http://anytime.cs.umass.edu/aimath06/proceedings/P34.pdf)
 - map/graph coloring
 - Any examples listed [here, e.g.](https://sat-smt.codes/SAT_SMT_by_example.pdf)
 - [subset-sum](https://en.wikipedia.org/wiki/Subset_sum_problem) on nats
 - word search w/<=k words?
 - Crosswords w/a dictionary: feasible? 
 - [The four fours puzzle](https://steemitimages.com/p/o1AJ9qDyyJNSg3j52kokyfvD831NZY7nurgU1VdMURXcJsQgN?format=match&mode=fit&width=1280)
 - [Marble/peg solitaire 15 golf ball tees puzzle](https://ai.stanford.edu/~chuongdo/satpage/index.html)
 - The n-queens problem in chess
 - [A recent set of solutions to a bunch of the common puzzles](https://www.researchgate.net/project/Combinatorial-problem-solving-with-Z3)
 - [Rubik's cube](https://arxiv.org/abs/1105.1436) (although there are [plenty of open variants!](http://www.twistypuzzles.com/))
 - [Filomino](https://github.com/tomvbussel/fillomino)
 - [nonogram/picross](https://en.wikipedia.org/wiki/Nonogram)

*Do* use these to come up with an idea on how to do work in this
space. *Don't* plan to use these as the/a project itself. We will
search, google and github, for prior art describing a solution. 

One easy choice to consider are newer/uncommon/understudied logic
puzzle/game kinds of things, the kind with a finite number of moves at
every stage. Some of the below will have been done, others will not
have been.


 - [lists of scores of these sort of puzzles](https://www.puzzle-nonograms.com/) (/Scores/ of them)!
 - [Mah-jongg and the 15 puzzle](https://arxiv.org/abs/cs/0512049)
 - [Coingrab](https://steemit.com/gaming/@vibvir/coin-grab-game-2-for-gamezop)
 - [0hn0](https://0hn0.com/)
   - although [battleship is solved](http://www.mountainvistasoft.com/docs/lect-04.pdf) there are [many](http://www.mountainvistasoft.com/variations.htm) [variants](http://www.mountainvistasoft.com/omnibus.htm#:~:text=Marcelo%20Inglesias%27s%20puzzle%20blog) still open.
 - [go-to board game reference, logic puzzles or not](https://boardgamegeek.com/)
 

Another way to go is to make a slight tweak on some existing problem:
one I know still outstanding is to solve [0h h1](https://0hh1.com/) on
the surface of a cube. Another one I know of still outstanding is to
find all the [English](https://www.powerthesaurus.org/send/synonyms) [language](https://www.powerthesaurus.org/more/synonyms) [semantic equivalents](https://www.powerthesaurus.org/money/synonyms) of
`SEND+MORE=MONEY`. Or, if you know another language, perhaps you could
use a corpus of that language instead.

I'd like these to all be unique, so first come first served!

You want to think about "how would I write a program that would make
my instance of this problem true IFF a certain satisfiability problem
holds, and such that the answer to that SAT problem gives me a
solution to mine?"

### Exemplary model submissions

####  For comparison and reference see 

  1. [that article/blog post](https://research.swtch.com/boolean) on Minimal Boolean Formulae
  2. [another exemplary piece]({{ site.baseurl }}/assets/docs/Back_to_Basics_Solving_w_SAT.PDF)
 
#### Student submissions 

  1. [The Crowded Chessboard Problem]({{ site.baseurl }}/assets/docs/last-projects/crowded.pdf)
  2. [DPLL via the Design Recipe]({{ site.baseurl }}/assets/docs/last-projects/htdp-dpll.pdf) ([cf](https://htdp.org/2018-01-06/Book/part_preface.html#%28part._sec~3asystematic-design%29))
  3. [Solving Skeletal Division problems with SAT]({{ site.baseurl }}/assets/docs/last-projects/skeletal-division.pdf) ([cf](https://twitter.com/jhemann/status/1227749215406718982/retweets/with_comments))
  4. [Blackbox]({{ site.baseurl }}/assets/docs/last-projects/blackbox.pdf) ([cf](https://en.wikipedia.org/wiki/Black_Box_(game)) [cf](https://boardgamegeek.com/thread/185907/its-mastermind-meets-minesweeper) [cf](https://www.youtube.com/watch?v=dUZhqBCpXA0)) 
  5. [Lights out]({{ site.baseurl }}/assets/docs/last-projects/lightsout.pdf) ([cf](https://en.wikipedia.org/wiki/Lights_Out_(game))) 

Note: student submissions from previous terms, as well as other
published works, did not necessarily have the same length detail
requirements, and were not assessed under exactly the same
rubric. Please pay close attention to the guidelines as we lay out for
your assignment for this semester.

I am not opposed to you using SMT/CP/CLP solvers as well or in
addition. If you have a reasonable justification for why you would
want to go that direction, please let us know, make it clear that's
what you're doing, and why, and this will likely be okay. 

### Project area option 2: Proving a mildly non-trivial theorem in ACL2. 

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
about thiiis far apart. It's far easier to start small and scale
up. Mechanizing the proof will be more difficult than doing it on pen
and paper, so factor that in when you choose a problem.


I think it's dangerous to, unless you've already been working with
some other theorem prover and its logic, to try to change horses in
midstream. Prof. Tripakis' class used something called Lean last term
and if you know anyone in that class, I think if you compare notes
you'll find that there's plenty of differences, even though Lean and
ACL2 have similar broad aims. Mutatis muntandis Coq, Agda, Isabelle,
HOL, ELF, etc. But please see further below.

#### Other Project ideas in this space

  1. [Japanese multiplication (EDIT: WITS prove the by-hand multiplication alg. correct)](https://vm.tiktok.com/J6j9TaW/)
  2. `zip` through `reverse` functions.
  3. [Peasant multiplication (EDIT: unfortunately too easy!)]({{ site.baseurl }}/assets/code/russian-peasant.lisp)
  4. Type checker, types with STLC 
  5. Lam-calc to deBruijn indices?
  6. DeBruijn `add1`, or that a closed `lambda` calc expr generates a closed DeBruijn index term.
  7. Combinatory logic basis.
  8. Quines? (I don't yet know how to attack this problem)!
  9. [McCarthy reverse]({{ site.baseurl }}/assets/docs/CAR-section-4.pdf) is way too big
  10. `(implies (sorted-tree tree) (sorted-listp (inorder-walk tree)))`
  11. questions about lists and permutations. 
  12. Equiv. of the two Pascal triangle implementations
  13. `walk-symbol` termination 
  14. [`interleave-lists` simultaneously vs one-after-the-other (Edit: unfortunately, turns out it's trivial!)]({{ site.baseurl }}/assets/code/interleave-lists.lisp)

### Exemplary model pieces: 

#### For comparison and reference, see 
 
 1. [Shankar's Godel incompleteness theorem formalized](https://www.cambridge.org/core_title/gb/112885) is waaaay too much 
 2. Potentially, small parts of almost any work in ACL2 itself. 
 
#### Student submissions 

 1. [Reverse of a list is a permutation]({{ site.baseurl }}/assets/docs/last-projects/rev-perm.pdf)
 2. [Length of a powerlist]({{ site.baseurl }}/assets/docs/last-projects/len-pow.pdf)
 3. [Magnetic Tower of Hanoi]({{ site.baseurl }}/assets/docs/last-projects/mtoh.pdf)
 4. [Zip-unzip round trip]({{ site.baseurl }}/assets/docs/last-projects/zip-unzip.pdf)
 5. [Pascal Triangle Problems]({{ site.baseurl }}/assets/docs/last-projects/pascal.pdf)
 6. [Natural v. tail recursive triangular numbers]({{ site.baseurl }}/assets/docs/last-projects/nth-triangular.pdf)


Note: student submissions from previous terms, as well as other
published works, did not necessarily have the same length detail
requirements, and were not assessed under exactly the same rubric. In
addition, you might find some of these were a bit simpler than I will
expect of you all. Please pay close attention to the guidelines as we
lay out for your assignment for this semester.

# We permit other kinds of projects too!! 

If you *have* some other ideas though---something else on the topic of
logic and computation that's grabbed your attention, or something else
that you're interesting in pursuing, or that's related to the
confluence of your other interests, talk to me! If your group has some
other kind of idea for something related to theorem proving---if for
instance your mob has experience with Agda and you want to pursue
something related to that, that's a suggestion. If you find yourselves
really intrigued by Prolog or miniKanren and want to try your hand
with something related that way, I can help guide you. Maybe you're
[interested in ML and want to apply it to theorem
proving](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.751.3626&rep=rep1&type=pdf). If
you are by chance interested in non-classical logics or alternate
proof systems and think you've got something interesting to say or to
do here (if any, you should still try to make an educational,
pedagogical contribution) that too might strike your fancy. If you
want to try [mixing](https://arxiv.org/abs/1509.06082),
[merging](https://www.cse.iitk.ac.in/users/karkare/pubs/icsews13formalise-id1-p-16138-preprint.pdf)
these technologies together, that works too! I'd like to help you
author relevant learning experiences! Cross-course and
interdisciplinary work is great! I'd still want it to be something
exploratory, with at least a component of novelty.

If I can say more that's helpful to you, please let me know!

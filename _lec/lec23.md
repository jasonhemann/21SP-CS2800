---
title: Reasoning with accumulators
date: 2021-03-15
---

## With accumulators 

### Scylla and Charybdis

We have one definition that is simple, but inefficient. We also have
another function definition that is more complex, but efficient.

```lisp
(definec plus (n :nat m :nat) :nat
  (cond
    ((zp m) n)
	(t (1+ (plus n (1- m))))))
```

```lisp
(definec plus (n :nat m :nat) :nat
  (cond
    ((zp m) n)
	(t (plus (1+ n) (1- m)))))
```

## How should we proceed? 


## Where have we seen similar situations before? 

`app2-assoc` ? 

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

* I want you all to have some experience *doing* a theorem of your own devise in ACL2, for some quasi-extrensic end.

* I want to do a bit of "authentic assessment" without steering too far into (for we 2800ers) uncharted territory.

* I want you to experience, writ small, some of what it's like to mechanize/prove theorems for a living.

* I want us to deploy the creative process and that same design recipe in this slightly different context. 

So, how do we get there? 

I strongly suggest you come up with some claims from properties circa fundies I programs or maybe the first half of fundies II, perhaps some other kinds of pure programs (I think John Hughes talks about more of these in a video on the enrichment page). I want you to come up with something that you can mechanically prove in ACL2s. These shouldn't be too much harder than the last couple of homeworks, and it oughtn't be a whole lot more. I want it to have several discrete steps that you can build to, and I hope that these scaffold up, so that we can go get half a loaf. No "moonshots." Pen-and-paper proving these aren't going to suffice---but I bet that would be a *great* thing to sketch out as you work toward automating. 

I think it's dangerous to, unless you've already been working with some other theorem prover and its logic, to try to change horses in midstream. Prof. Tripakis' class are using something called Lean this term, and if you know anyone in that class, I think if you compare notes you'll find that there's plenty of differences, even though Lean and ACL2 have similar broad aims. 

If you *have* some other ideas though---something else on the topic that's grabbed your attention, or something else that you're interesting in pursuing, or that's related to the confluence of your other interests, talk to me though. I'd like to help you author relevant learning experiences! Cross-course and interdisciplinary work is great!

If I can say more that's helpful to you, please let me know!

# Other Project Ideas

  1. Japanese multiplication (https://vm.tiktok.com/J6j9TaW/)
  2. Russian peasant multiplication 
  3. McCarthy reverse? --- way too big & scary, IMHO
  4. Quines? How?
  5. STLC types? 
  6. Type checker? 
  7. Combinatory logic basis.
  8. Lam-calc to deBruijn indices?
  9. DeBruijn `add1`, or that a closed `lambda` calc expr generates a closed DeBruijn index term.
  10. `(implies (sorted-tree tree) (sorted-listp (preorder-walk tree)))`
  11. Comparing two different definitions of `zip-lists` recursive and not. 

## Further thoughts, places:

https://boardgamegeek.com/thread/185907/its-mastermind-meets-minesweeper

Mah-jongg and the 15 puzzle: 

https://arxiv.org/abs/cs/0512049

[https://slate.com/human-interest/2020/11/best-puzzle-books-crosswords-kenken.html]

nonogram 
picross
https://www.puzzle-nonograms.com/ (there's Scores! of them)!


### Exemplary model pieces 

##  For comparison and reference see [Shankar's Godel incompleteness theorem formalized](https://www.cambridge.org/core_title/gb/112885).

  Minimal boolean formulae. 
  [another]({{ site.baseurl }}/assets/docs/Back_to_Basics_Solving_w_SAT.PDF)

### Student submissions 

  1. [a]({{ site.baseurl }}/assets/docs/last-projects/crowded.pdf)
  2. [b]({{ site.baseurl }}/assets/docs/last-projects/htdp-dpll.pdf)
  3. [c]({{ site.baseurl }}/assets/docs/last-projects/skeletal-division.pdf)





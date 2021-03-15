---
title: Induction like a Pro
date: 2021-10-29
---

# Induction like a pro: not so hard! 

  What's the difference? 
  
  The order in which you approach

  1. Wishlist in a queue-like discipline
  2. Bigger steps
    (not so big that you jump over an inductive proof)

## "Yak Shave"

 When you build up an inordinate blocking chain dependencies.

## Induction like a pro. Battle plan.

## Top-down proof design. 

  We have so far been able to get there by building up. Let's figure
  it out as we go.


## Step back and look.

  Every admissible recursive definition leads to a valid induction scheme.

  What underlies both recursion and induction is _termination_.

  So, terminating functions give us both recursion schemes and induction schemes.

## Predef Functions

```lisp
(definec in (a :rational L :lor) :bool
  (and (consp L)
       (or (== a (car l))
	       (in a (cdr L)))))

(definec del (a :rational L :lor) :lor
  (cond 
    ((endp L) L)
    ((== a (car L)) (cdr L))
    (t (cons (car L) (del a (cdr L))))))
	
(definec permp (x :lor y :lor) :bool
  (if (endp x) (endp y)
	  (and (in (car x) y)
	       (permp (cdr x) 
		   (del (car x) y)))))

 (definec app2 (x :tl y :tl) :tl
	(if (endp x)
		y
	  (cons (first x) (app2 (rest x) y))))

  (definec rev2 (x :tl) :tl
	(if (endp x)
		x
	  (app2 (rev2 (cdr x)) (list (car x)))))
```

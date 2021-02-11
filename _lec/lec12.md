---
title: Pen-and-paper proofs
date: 2021-02-17
---

## How to Prove it:

### Given a conjecture you want to prove you should:

 -  Check contracts & perform contract completion if needed
 -  Make sure you understand what the conjecture is claiming (restate
    to yourself in english)
 -  See if you can find a counterexample
 -  If you can't try to prove that the conjecture is a theorem
    (One often iterates over the preceding two steps)

### 2nd Level Techniques (more tactics and less overall campaign objectives): 

 -  Possibly split the conjecture into lemmas (e.g., if it is of the
    form `(and ...)`)
 -  If the conjecture seems true, can you generalize it?
 -  For example given `(app (app a a) a) = (app a (app a a))`,
    generalize to `(app (app a b) c) = (app a (app b c))` (Notice that
    the generalized conjecture is much more powerful. Often times the
    more powerful general case is easier to solve, because we wipe
    away some of the inadvertent details.)
	

### What do we have in our repertoire? 

During the proof process, we have at our disposal: 

 - All the axioms (`cons` axioms, `if` axioms, ...)
 - All the theorems we have proved so far
 - All the definitional axioms (Def of `app`, `len`, ...)
 - All the contract theorems (Contract of `app`, `len`, ...)

  Recall that you can invoke these axioms and theorems at any time in
  the proof with *any* instantiation. We "roll deep."

  This is a vast arsenal that will help you prove theorems.
  
  
### If you get a statement in English 

#### (Make that mathematical English or even better, some more formal language)

   Starting with words "`^v^v^v^v^v^v^v^v^v^v^v^v`" (A la Charlie Brown)
   
   -1. Reformulate the conjecture into our formal language.
    0. Generalize the claim to the actual interesting version
	1. Write down this version as `Conjecture:` and give it a name usually
	2. Exportation: extract the context by rewriting the conjecture into the form: where there are as many hyps as possible. Any propositional logic simplification is allowed (e.g., negate and swap).
    3. Contract completion
	4. Context
	5. Derived Context. (These are the obvious things that follow from the context + axioms/theorems) 
	   - Common patterns:
         -- (endp x), (tlp x): (== x nil)
         -- (tlp x), (consp x): (tlp (rest x))
         -- φ1 ∧ … ∧ φn -> ψ: Derive φ1,…,φn and use MP to derive ψ
    6. Goal
	7. Proof. Use the proof format in class.

	- For equality, start with LHS/RHS and end with RHS/LHS or start w/ LHS & reduce, then start w/ RHS & reduce to the same thing
	- For transitive relations (㱺, <, ≤, …) the same proof format works
	- For anything else reduce to t

## Continuing from last time

   ```lisp
   (implies (ne-tlp ls)
	 (implies 
	   (implies (tlp (rest ls))
		 (implies (in2 a (rest ls))
				  (in2 a (rev2 (rest ls)))))
	   (implies (tlp ls)
		 (implies (in2 a ls)
				  (in2 a (rev2 ls))))))
	 ```

### A taste of things to come.

     ```lisp
	 (defthm in-rev
       (implies (tlp ls)
         (implies (in2 a ls)
                  (in2 a (rev2 ls)))))
     ``` 

## Exemplary Proofs

   A. If a is in l, b is in l and c is in l, then the length of l <= 3.
   
   B Suppose 1 is in x and 2 is in x.
     Suppose further that if a is a value that differs from 1 and from 2, it is not in x.
     Then the length of is precisely 2.
   
   C 
       ```lisp
	   (definec del2 (a :all l :tl) :tl
		 (cond 
		   ((endp l) l)
		   ((equal a (car l)) (cdr l))
		   (t (cons (car l) (del2 a (cdr l))))))
	   ```
  Suppose that every element of `x` is in `y`.
  Suppose further I delete `a` from `x` and call the result `x^`.
  Suppose further still I delete `a` from `y` and call the result `y^`
  Then every element of `x^` is in `y^`
  
  Is that true? What do we do?


# Class Examples of Pen-and-Paper Proofs.
  ```lisp
  (definec len2 (x :tl) :nat
	(if (endp x)
		0
	  (+ 1 (len2 (rest x)))))

  (definec in2 (a :all l :tl) :bool
	(if (endp l)
		nil
	  (or (== a (first l)) (in2 a (rest l)))))

  (definec app2 (x :tl y :tl) :tl
	(if (endp x)
		y
	  (cons (first x) (app2 (rest x) y))))

  (definec rev2 (x :tl) :tl
	(if (endp x)
		x
	  (app2 (rev2 (cdr x)) (list (car x)))))
  ```
  
### Is this true?


   ```lisp
   (implies (endp ls)
     (implies (tlp ls)
	   (implies (in2 a ls)
	            (in2 a (rev2 ls))))
	 ```


   ```lisp
   (implies (consp ls)
	 (implies 
	   (implies (tlp (rest ls))
		 (implies (in2 a (rest ls))
				  (in2 a (rev2 (rest ls)))))
	   (implies (tlp ls)
		 (implies (in2 a ls)
				  (in2 a (rev2 ls)))))
	 ```


### Fix it and prove.

#|

Homework 5a. Substitution and Equational Reasoning

|#


#| 

To refresh yourself on substitutions, see [Ch 4. pp
76-77](https://pages.github.ccs.neu.edu/jhemann/21SP-CS2800/readings/).

You all will recall we have expressed our substitutions as `let`
bindings. And you know `test?` presents substitutions in this same
style. 

But as you might have (briefly) seen, acl2 has a built-in substitution
operation `acl2::subst`.

|# 

;; We can use a `let` expression, with a substitution and a property,
;; to test an example of that property.

(let ((x 5) (y 10))
  (equal (* (+ x y) (+ x y)) 
         (+ (* x x) (* 2 x y) (* y y))))

;; We could also try, however, to use ACL2's `subst` operation a couple
;; of times, and then pick up the result and run it.

(acl2::subst '10 'y
  (acl2::subst '5 'x 
    '(equal (* (+ x y) (+ x y)) (+ (* x x) (* 2 x y) (* y y)))))

;; > (acl2::subst '10 'y
;;     (acl2::subst '5 'x
;;       '(equal (* (+ x y) (+ x y))
;;               (+ (* x x) (* 2 x y) (* y y)))))
;; (EQUAL (* (+ 5 10) (+ 5 10))
;;        (+ (* 5 5) (* 2 5 10) (* 10 10)))
;; > (EQUAL (* (+ 5 10) (+ 5 10))
;;          (+ (* 5 5) (* 2 5 10) (* 10 10)))
;; T

;; ACL2's `subst` is, however, a little naive in its behavior. Whereas
;; we can let-bind those values in this modified expression and get
;; the same answer, subst doesn't work.

(let ((x 5) (y 10))
  (equal (let ((x (+ x y)))
	   (* x x))
	 (+ (* x x) (* 2 x y) (* y y))))

(acl2::subst '10 'y
  (acl2::subst '5 'x 
    '(equal (let ((x (+ x y)))
              (* x x))
            (+ (* x x) (* 2 x y) (* y y)))))

;; > (acl2::subst '10 'y
;;     (acl2::subst '5 'x 
;;       '(equal (let ((x (+ x y)))
;; 		(* x x))
;; 	      (+ (* x x) (* 2 x y) (* y y)))))
;; (EQUAL (LET ((5 (+ 5 10))) (* 5 5))
;;        (+ (* 5 5) (* 2 5 10) (* 10 10)))
;;
;; ACL2 Error in TOP-LEVEL:  The form (LET ((5 (+ 5 10))) (* 5 5)) is
;; an improper let expression because it attempts to bind 5, which is
;; not a symbol.

;; Furthermore, even if we now remove this let binding, we get the
;; wrong answer:

;; > (EQUAL (* 5 5)
;;          (+ (* 5 5) (* 2 5 10) (* 10 10)))
;; NIL

;; 1. What is ACL2s `subst` operation not doing correct? (You should
;; answer in a sentence or two)

#| 

Answer: 

|# 

;; We might instead try and construct let expressions we can pick up
;; and run:

(defconst *example-subst* '((x 5) (y 10)))

(defconst *example-prop* 
  '(implies (consp (cons x y)) (consp (cons y x))))

`(let ,*example-subst* ',*example-prop)

;; > `(let ,*example-subst* ,*example-prop*)
;; (LET ((X 5) (Y 10))
;;      (IMPLIES (CONSP (CONS X Y))
;;               (CONSP (CONS Y X))))
;; > (LET ((X 5) (Y 10))
;;      (IMPLIES (CONSP (CONS X Y))
;;               (CONSP (CONS Y X))))
;; T

;; 2. Does this approach to working with substitutions have the same
;; problem we faced with our `subst` approach? 

(defconst *has-same-problem* )

(boolp *has-same-problem*)

;; 3. Why, or why not? (You should answer in a sentence or two)

#| 

Answer: 

|# 





;; Apply the following substitution to the following expressions:

;; 1.

;; 2.

;; 3.

#| 

In this half of the homework, we will practice equational reasoning
proofs. So that we can focus on the reasoning part, we are using
these same familiar functions.

|#

(definec len2 (x :tl) :nat
  (if (endp x)
      0
    (+ 1 (len2 (rest x)))))

(definec app2 (a :tl b :tl) :tl
  (if (endp a)
      b
    (cons (first a) (app2 (rest a) b))))

(definec rev2 (x :tl) :tl
  (if (endp x)
      nil
    (app2 (rev2 (rest x)) (list (first x)))))

(definec in2 (a :all X :tl) :bool
  (and (consp X)
       (or (== a (first X))
           (in2 a (rest X)))))

(definec del (a :all X :tl) :tl
  (cond ((endp X) nil)
        ((== a (car X)) (del a (cdr X)))
        (t (cons (car X) (del a (cdr X))))))

#|

Recall that for each of the definitions above we have both a
definitional axiom, and a contract theorem.  For example for len2, we
have the definitional axiom:

(implies (tlp x)
         (== (len2 x)
             (if (endp x)
                 0
               (+ 1 (len2 (rest x))))))

The contract theorem is:

(implies (tlp x)
         (natp (len2 x)))

You can use definitional axioms and contract theorems for free,
i.e., you don't have to prove anything. When you use a definitional
axiom, say for len2, your hint is "Def len2". When you use a contract
theorem, say for len2, your hint is "Contract len2".

For the rest of your lab questions, you can assume that the following
is a theorem. 

Lemma len2-app2
(implies (and (tlp x) 
              (tlp y))
         (== (len2 (app2 x y))
             (+ (len2 x) (len2 y))))

When you use the lemma above, your hint is "Lemma len2-app"
but you have to provide a substitution.

We need to prove that the above is a theorem in ACL2s so that you can
use it.

|#

(defthm len2-app2
  (implies (and (tlp x) 
                (tlp y))
           (== (len2 (app2 x y))
               (+ (len2 x) (len2 y)))))

#|

We will use an equational reasoning proof checker to check your
proofs. It's still work in progress, but it useful for finding bugs in
your proofs before you submit them. Go to
http://checker.atwalter.com/. If you click on "Select a file" you can
see some example proofs. If you then click on "Validate" the proof
checker will check the example.

I cannot /force/ you to, but you should absolutely use ACL2s to check
the conjectures you come up with, and you should use the [automated
proof checker](http://checker.atwalter.com/checker) to check the
proofs with which you come up. You can _know_ before you submit, that
you got things right! Wouldn't it be great if you had that for English
class!?

EDIT. You might /try/ it, but it's quite WIP, so YMMV. 

To check your proofs, below, click on "Browse" and select this
file. Then click on "Validate" and the proof checker will check 
your solution.

To use the proof checker, you have to write legal ACL2s syntax, e.g.,
you can't write x=nil; instead you have to write (== x nil).  Also,
we have to be able to parse your proofs, so you might see parser
errors. If you do, look at the sample examples and the error message
to fix the issues you get.

Here is the structure of of proofs. 

I will use an example proof to show the proof format.
|#

#| 
Conjecture 0:
(implies (tlp x)
         (implies (and (consp x)
                       (not (== a (first x)))
                       (implies (tlp (rest x))
                                (implies (in2 a (rest x))
                                         (in2 a (app2 (rest x) y)))))
                  (implies (in2 a x)
                           (in2 a (app2 x y)))))

Exportation:
(implies (and (tlp x)
              (consp x)
              (not (== a (first x)))
              (implies (and (tlp (rest x))
                            (in2 a (rest x)))
                       (in2 a (app2 (rest x) y)))
              (in2 a x))
         (in2 a (app2 x y)))

Contract Completion:
(implies (and (tlp x)
              (tlp y)
              (consp x)
              (not (== a (first x)))
              (implies (and (tlp (rest x))
                            (in2 a (rest x)))
                       (in2 a (app2 (rest x) y)))
              (in2 a x))
         (in2 a (app2 x y)))

Context:
C1. (tlp x)
C2. (tlp y)
C3. (consp x)
C4. (not (== a (first x)))
C5. (implies (and (tlp (rest x)) (in2 a (rest x)))
             (in2 a (app2 (rest x) y)))
C6. (in2 a x)

Derived Context:
D1. (tlp (rest x)) { Def tlp, C1, C3 }
D2. (in2 a (rest x)) { Def in2, C6, C3, C4 }
D3. (in2 a (app2 (rest x) y)) { C5, D1, D2, MP }

Goal: (in2 a (app2 x y))

Proof:
(in2 a (app2 x y))
= { Def app2, C1, C3 }
(in2 a (cons (first x) (app2 (rest x) y)))
= { Def in2, cons axioms, C3 }
(or (== a (first x)) (in2 a (app2 (rest x) y)))
= { D3, PL }
t

QED
|# 

#|

You can skip the Exportation section if no exportation is possible and
you can skip then Contract Completion section if that section would be
equivalent to the section preceding it.

You can skip the Context section if there are no hypotheses, something
that rarely happens. You can skip the Derived Context if you have
nothing to derive.

If you derive nil, you can skip the remaining sections.

If you derive the goal, you can skip the remaining sections.

Finally you need the QED to indicate the end of your proof.

You need to populate the space between the conjectures and the QEDs
with a proof.  As you complete proofs, click on "Validate" again to
check them. You can have multiple proofs in a file if you wish.

You may have to add hints we normally do not add in class.  One
example is that you have to sometimes add Contract hints. If you need
to use arithmetic, you can just write "arith" or "arithmetic".  You
can also use any propositional reasoning and can justify it by
writing "PL". The proof checker always allows arithmetic and PL
reasoning even if you don't provide the hints, but it is good style to
do so.

|#




;; 3. Prove the following using equational reasoning. IMHO this one is
;; comparatively easy, so if you are stuck here then please reach out!

Conjecture 1:
(== (len2 (app2 x y)) 
    (len2 (app2 y x)))

...

QED



;; 4. Prove the following conjecture using equational reasoning. related conjectures.
;; Conjecture 2:
(implies (endp x)
	 (== (len2 (rev2 x)) (len2 x)))


...

QED

;; 5. Prove the following conjecture using equational reasoning. Note
;; this and the preceding conjecture are related.

;; Conjecture 3:
(implies (tlp x)
         (implies (and (not (endp x))
	      	       (implies (tlp (rest x))
                                (== (len2 (rev2 (rest x)))
			            (len2 (rest x)))))
		  (== (len2 (rev2 x)) (len2 x))))

...

QED


#|

HW 6b. Admissibility, Measures, and Totality

|#

#| 

We now deliberately removed our earlier allowances that let us elide
checking function and body contracts. But we now know what to do about
these! 

|#

;; Part I : INADMISSIBLE FUNCTIONS

#| 

Recall that Section 5.1 of the Manolios text describes the
Definitional Principle for ACL2s. This includes 6 conditions that an
ACL2s function definition must satisfy to be admissible.

The "functions" in this section are inadmissible. I've commented them
out, so that they don't interfere with running your file---ACL2
wouldn't put up with these inadmissible functions!

For each, identify *a* condition that's violated. (If multiple
 conditions are violated, you only have to find one such
 condition. You can assume that Condition 1 is met: the symbol used is
 a new function symbol.)

- If conditions 2 or 3 are violated (formals are not distinct or body
  is not a term), select that choice and explain the violation in
  English prose, as a string.

- If conditions 4, 5, or 6 are violated, provide as quoted data an
  input that would satisfy the input contract but that would cause the
  condition to fail.

|#

(defdata failure (oneof (cons 2 string)
			(cons 3 string)
			(cons 4 all)   ;; these should be quoted data
			(cons 5 all)   ;; these should be quoted data
			(cons 6 all))) ;; these should be quoted data

;; Here are a few examples:
(check (failurep '(2 . "x is provided as an argument twice")))
(check (failurep '(5 . ((x 3) (y '(1 2 3))))))

;; (definec f1 (x :nat) :int 
;;   (if (zp x)
;;       3
;;     (- 23 (f1 (+ f1 x)))))

(defconst *f1-failure* )
(failurep *f1-failure*)

;; (definec f2 (x :nat) :int
;;   (if (zp x)
;;       3
;;     (* 2 (f2 (- x 2)))))

(defconst *f2-failure* )
(failurep *f2-failure*)

;; (definec f3 (x :tl a :int) :nat
;;   (if (endp x)
;;       (* a (1+ a))
;;     (f3 (rest x) (1+ a))))

(defconst *f3-failure* )
(failurep *f3-failure*)


;; PART II ADMISSIBLE FUNCTIONS 

;; You can ignore these; they adjust some ACL2s settings so that
;; definec only uses the provide measure function to prove
;; termination, and so that definec doesn't do as much magic as it
;; would otherwise.
(set-termination-method :measure)
(set-well-founded-relation n<)
(set-defunc-typed-undef nil)
(set-defunc-generalize-contract-thm nil)
(set-gag-mode nil)

#| 

The functions in this section are admissible. We will prove this. In
this section, I have broken each question down into smaller steps.

|#

(definec len2 (x :tl) :nat
  (if (endp x)
      0
    (+ 1 (len2 (rest x)))))

;; Example: Here, I work for you an example all the way through

;; A
(definec app2-measure (x :tl y :tl) :nat
  (declare (ignorable y))
  (len2 x))

;; B

;; Notice that the measure has to be of the form (if IC (m ...) 0), 
;; not implies, and 0 for the case the contracts don't hold.
(definec app2 (x :tl y :tl) :tl
  (declare (xargs :measure (if (and (tlp y) (tlp x)) (app2-measure x y) 0)))
  (if (endp x)
      y
    (cons (first x) (app2 (rest x) y))))

;; C
(defconst *app2-contract-theorem*
  '(implies (and (tlp x) (tlp y)) (tlp (app2 x y))))

(make-event `(thm ,*app2-contract-theorem*))

;; D

Conjecture app2-terminates:
(implies (and (tlp x)
              (tlp y)
              (not (endp x)))
         (< (app2-measure (rest x) y)
            (app2-measure x y)))

Context:
C1. (tlp x)
C2. (tlp y)
C3. (not (endp x))

Derived Context:
D1. (tlp (rest x)) {C1, C3}

Goal:
(< (app2-measure (rest x) y)
   (app2-measure x y))
   
Proof:
(< (app2-measure (rest x) y)
   (app2-measure x y))
= { Def app2-measure } 
(< (len2 (rest x))
   (len2 x))
= { Def len2 }    
(< (len2 (rest x))
   (if (endp x)
       0
    (+ 1 (len2 (rest x)))))
= { C3 } 
(< (len2 (rest x)) (+ 1 (len2 (rest x))))
= { Arith } 
t 

QED     

#| 

As per the lecture notes, ACL2s will complain about the definition of
your measure function unless you tell it to ignore arguments you don't
use. It is simpler to tell it all arguments can be ignored, hence
my (set-ignore-ok t) form.

|# 

(set-ignore-ok t)


;; A. Provide a measure function to prove termination.

;; The function definitions for e/o?, f4, f5, f6, and f7 can be found
;; in Part B below.

(definec e/o?-measure

  )

(definec f4-measure

  )

(definec f5-measure

  )

(definec f6-measure

  )

(definec f7-measure

  )

;; B. Finish adding your measure to the function definition, to help
;; ACL2 show termination. You can modify the function definition if
;; you need, but make sure it's equivalent

(definec e/o? (flag :bool n :nat) :bool
  (declare (xargs :measure ))
  (cond
   (flag
    (cond
     ((zp n) nil)
     (t (e/o? (not flag) (1- n)))))
   (t
    (cond
     ((zp n) t)
     (t (e/o? (not flag) (1- n)))))))


(definec f4 (x :tl y :int) :tl
  (declare (xargs :measure ))
  (if (< y 0)
      x
    (f4 (rest x) (- y 1))))


(definec f5 (x :int l :tl) :pos
  (declare (xargs :measure ))
  (cond
    ((endp l) 1)
    ((> 0 x)  (1+ (f5 (len2 l) l)))
    (t (1+ (f5 (1- x) (rest l))))))


(definec f6 (x :nat y :nat) :nat
  (declare (xargs :measure ))
  (cond 
    ((zp x)   y)
    ((<= y x) x)
    ((<= y 1) (f6 (1+ x) (1+ y)))
    (t        (f6 (1- x) (1+ y)))))


(definec f7 (x :tl y :nat) :tl
  (declare (xargs :measure ))
  (cond 
    ((zp y)   nil)
    ((endp x) (list y))
    ((= y 1)  (f7 (rest x) y))
    (t        (f7 (cons y x) (- y 1)))))


;; C. State, as a quoted list, the contract theorem (condition 5).

(defconst *e/o?-contract-theorem*

  )

(make-event `(thm ,*e/o?-contract-theorem*))

(defconst *f4-contract-theorem*

  )

(make-event `(thm ,*f4-contract-theorem*))

(defconst *f5-contract-theorem*

  )

(make-event `(thm ,*f5-contract-theorem*))

(defconst *f6-contract-theorem*

  )

(make-event `(thm ,*f6-contract-theorem*))

(defconst *f7-contract-theorem*

  )

(make-event `(thm ,*f7-contract-theorem*))


;; D. Prove that the function is terminating using your measure
;; function and equational reasoning

Conjecture e/o?-terminates: 

...

QED

Conjecture f5-terminates:

...

QED


Conjecture f4-terminates:

...

QED


Conjecture f6-terminates:

...

QED


Conjecture f7-terminates:

...

QED

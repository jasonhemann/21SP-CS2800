; ****************** BEGIN INITIALIZATION FOR ACL2s MODE ****************** ;
; (Nothing to see here!  Your actual file is after this initialization code);
(make-event
 (er-progn
  (set-deferred-ttag-notes t state)
  (value '(value-triple :invisible))))

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading the CCG book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "acl2s/ccg/ccg" :uncertified-okp nil :dir :system :ttags ((:ccg)) :load-compiled-file nil);v4.0 change

;Common base theory for all modes.
#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s base theory book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "acl2s/base-theory" :dir :system :ttags :all)


#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s customizations book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "acl2s/custom" :dir :system :ttags :all)

;; guard-checking-on is in *protected-system-state-globals* so any
;; changes are reverted back to what they were if you try setting this
;; with make-event. So, in order to avoid the use of progn! and trust
;; tags (which would not have been a big deal) in custom.lisp, I
;; decided to add this here.
;; 
;; How to check (f-get-global 'guard-checking-on state)
;; (acl2::set-guard-checking :nowarn)
(acl2::set-guard-checking :all)

;Settings common to all ACL2s modes
(acl2s-common-settings)
;(acl2::xdoc acl2s::defunc) ;; 3 seconds is too much time to spare -- commenting out [2015-02-01 Sun]

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem loading ACL2s customizations book.~%Please choose \"Recertify ACL2s system books\" under the ACL2s menu and retry after successful recertification.") (value :invisible))
(include-book "acl2s/acl2s-sigs" :dir :system :ttags :all)

#+acl2s-startup (er-progn (assign fmt-error-msg "Problem setting up ACL2s mode.") (value :invisible))

(acl2::xdoc acl2s::defunc) ; almost 3 seconds

; Non-events:
;(set-guard-checking :none)

(set-inhibit-warnings! "Invariant-risk" "theory")

(in-package "ACL2")
(redef+)
(defun print-ttag-note (val active-book-name include-bookp deferred-p state)
  (declare (xargs :stobjs state)
	   (ignore val active-book-name include-bookp deferred-p))
  state)

(defun print-deferred-ttag-notes-summary (state)
  (declare (xargs :stobjs state))
  state)

(defun notify-on-defttag (val active-book-name include-bookp state)
  (declare (xargs :stobjs state)
	   (ignore val active-book-name include-bookp))
  state)
(redef-)

(acl2::in-package "ACL2S")

; ******************* END INITIALIZATION FOR ACL2s MODE ******************* ;
;$ACL2s-SMode$;ACL2s

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

;; A. Provide a measure function that you will use to prove
;; termination of functions e/o?, f4, and f5.

;; You will find the definitions of functions e/o?, f4, and f5 in Part
;; B below.

(definec e/o?-measure

  )

(definec f4-measure

  )

(definec f5-measure

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

(definec f5 (x :nat y :nat) :nat
  (declare (xargs :measure ))
  (cond 
    ((zp x)   y)
    ((<= y x) x)
    ((<= y 1) (f5 (1+ x) (1+ y)))
    (t        (f5 (1- x) (1+ y)))))


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


;; D. Prove that the function is terminating using your measure
;; function and equational reasoning

Conjecture e/o?-terminates: 

...

QED


Conjecture f4-terminates:

...

QED


Conjecture f5-terminates:

...

QED


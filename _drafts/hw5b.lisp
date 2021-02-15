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

HW 5b PART II: MORE EQUATIONAL REASONING

This part of your homework is partly a continuation of the last
problem on hw5a. So, please do start with that. In this half of the
homework, we will practice equational reasoning proofs. You will prove
a number of related conjectures. You need to populate the space
between the conjectures and the QEDs with a proof. That is, for each
of the conjectures below:

1. Perform conjecture contract checking, and if necessary, contract
   completion. Contract completion is the process of adding the
   minimal set of hypotheses needed to guarantee that the input
   contracts for all functions used in the conjecture are
   satisfied. See the lecture notes and recent readings. Do not add
   anything else to the conjecture.

2. Determine if the resulting conjecture is valid or not. If it is
   valid, provide a proof, as outlined in the lecture notes and in
   class. If it is invalid, provide a counterexample in the form of a
   quoted substitution (i.e. the bindings you'd use for a `let`
   expression).
|#

#|

So that we can focus on the reasoning part, we are once again using
the following now-familiar functions.

|#

;; Function Definitions

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

As we noted, we have deliberately removed the earlier allowances that
let us elide checking function and body contracts. We now know what
these are and understand them, so we use this HW to get more
comfortable. Recall that for each of the definitions above we have
both a definitional axiom, and a contract theorem.  For example for
len2, we have the definitional axiom:

(implies (tlp x)
         (== (len2 x)
             (if (endp x)
                 0
               (+ 1 (len2 (rest x))))))

The contract theorem is:

(implies (tlp x)
         (natp (len2 x)))

You can use definitional axioms and contract theorems for free, i.e.,
you don't have to prove anything. When you use a definitional axiom,
say for len2, your hint is "Def len2". When you use a contract
theorem, say for len2, your hint is "Contract len2".

For the rest of this part of your hw questions, you can assume that
the following theorem. We need to prove that the above is a theorem in
ACL2s so that you can use it.

|#

(defthm len2-app2
  (implies (and (tlp x) 
                (tlp y))
           (== (len2 (app2 x y))
               (+ (len2 x) (len2 y)))))

#|

When you use the lemma above, your hint is "Lemma len2-app" but you
have to provide a substitution. As discussed, our substitution into
programs will only apply to free variable occurrences. For a
re-refresher see Ch 4. pp 76-77.

|#

#|

Note on Tooling:

We will use an equational reasoning proof checker to check your
proofs. I cannot /force/ you to, but you should consider using the
[automated proof checker](http://checker.atwalter.com/checker) to
self-check the proofs with which you come up. You can _know_
beforehand that you got things right! Wouldn't it be great if you had
that for English class!? It's still work in progress, but it useful
for finding bugs in your proofs before you submit them. Go to
http://checker.atwalter.com/. If you click on "Select a file" you can
see some example proofs. If you then click on "Validate" the proof
checker will check the example.

To check your proofs below, click on "Browse" and select this
file. Then click on "Validate" and the proof checker will check your
solutions. You can have multiple proofs in a file if you wish, so as
you complete your proofs, click on "Validate" again to check them one
at a time.

To use the proof checker, you have to write legal ACL2s syntax, e.g.,
you can't write x=nil; instead you have to write (== x nil).  Also,
we have to be able to parse your proofs, so you might see parser
errors. If you do, look at the sample examples and the error message
to fix the issues you get.

You can skip the Exportation section if no exportation is possible and
you can skip then Contract Completion section if that section would be
equivalent to the section preceding it.

You can skip the Context section if there are no hypotheses, something
that rarely happens. You can skip the Derived Context if you have
nothing to derive.

If you derive nil, you can skip any remaining sections of that proof.

If you derive the goal, you can skip any remaining sections of that
proof.

Finally you need the QED to indicate the end of your proof.

You may have to add hints we normally do not add in class.  One
example is that you have to sometimes add Contract hints. If you need
to use arithmetic, you can just write "arith" or "arithmetic".  You
can also use any propositional reasoning and can justify it by
writing "PL". The proof checker always allows arithmetic and PL
reasoning even if you don't provide the hints, but it is good style to
do so.

The example proof below shows the tool's expected proof format and
structure. See your notes from lab for more explicit instructions.

|#


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


#| 

;; IMHO this first one is a comparatively easy revv'er-upper, so if
;; you are stuck here then please reach out!

|# 

Conjecture 1:
(implies (boolp (boolp (boolp x))) (boolp (boolp x)))

...

QED

Conjecture 2:
(== (len2 (app2 x y)) 
    (len2 (app2 y x)))

...

QED


Conjecture 2:
(implies (endp x)
	 (== (len2 (rev2 x)) (len2 x)))


...

QED

;; Note this and the preceding conjecture are related.

Conjecture 3:
(implies (tlp x)
         (implies (and (not (endp x))
	      	       (implies (tlp (rest x))
                                (== (len2 (rev2 (rest x)))
			            (len2 (rest x)))))
		  (== (len2 (rev2 x)) (len2 x))))

...

QED



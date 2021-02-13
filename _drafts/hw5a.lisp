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

Homework 5a. Substitution, Normalization, and Equational Reasoning

|#


#| 

To refresh yourself on substitutions and the definition of
instantiation, see [Ch 4. pp
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

#| 

It seems like we could use some more practice with disjunctive
clauses. It will be useful to represent disjunctive clauses so that
ACL2 can recognize them.

|#

;; 4. Define a datatype for ACL2 literals. Notice here we use the ACL2
;; format, rather than the format than we used for SATP on 3b.

(defdata literal )

(check= (literalp '(not a)) t)
(check= (literalp '(not (not x))) nil)


;; 5. Define a datatype for ACL2 disjunctive clauses. Notice here we
;; use the ACL2 format, rather than the format than we used for SATP
;; on 3b.

(defdata disjunctive-clause )

(check= (disjunctive-clausep '(or)) t)
(check= (disjunctive-clausep '(or (not z))) t)
(check= (disjunctive-clausep '(or (not a) b (not c))) t)
(check= (disjunctive-clausep '(or (not a) b (not c) d)) t)
(check= (disjunctive-clausep '(or (not a) b (not (not a)) d)) nil)
(check= (disjunctive-clausep '(and (not a) b (not (not a)) d)) nil)
(check= (disjunctive-clausep '(not (and (not a) b (not (not a)) d))) nil)

#| 

We will still be using propositional equalities as we move into ACL2s
proofs. Some of the propositional equalities of Ch 3 fail to hold if
we naively translate them into ACL2 propositions.

|#

;; p ∨ true ≡ true (No 1)
;; p ⇒ true ≡ p    (No 3)
;; ¬¬p ≡ p         (No 22)
;; p ∨ ¬p ≡ true   (No 31)

(must-fail (thm (equal (or p t) t)))
(must-fail (thm (equal (implies p t) p)))
(must-fail (thm (equal (not (not x)) x)))
(must-fail (thm (equal (or p (not p)) t)))

;; 6. Why are these true in propositional logic but not theorems in
;; ACL2? 

#| 

Answer: 

|# 


#|

We define here a new version of `not` with a more restricted input
contract, and would like to prove a theorem about it. 

|# 

(definec bnot (p :bool) :bool
  (not p))

;; Conjecture: Double negation.
'(equal (bnot (bnot x)) x)))

;; 7. Write, as a quoted list, the result of performing contract
;; completion on the above conjecture.

(defconst *contract-completion* )

(thm *contract-completion)

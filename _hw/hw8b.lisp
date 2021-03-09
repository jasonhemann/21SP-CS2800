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

HW 8b: Completing an inductive proof

|#

#| 

In this second part of your homework, you will complete the remainder
of that larger proof. If you did not manage to complete this all on
the previous part, then now is a great time to get it fix it!

|# 

;; Place here the proofs of any other lemmas or theorems you decided you
;; need.


Lemma app2-nil:
(implies (tlp x) 
  (equal (app2 x nil) x))

Proof by: Induction on (tlp x)

;; COPY OVER YOUR SOLUTION TO THIS CASE FROM PART A
Induction Case app2-nil-base:
(implies (not (consp x))
  (implies (tlp x)
    (equal (app2 x nil) x)))

... 

QED

;; COPY OVER YOUR SOLUTION TO THIS CASE FROM PART A
Induction Case app2-nil-cons:
(implies (and (consp x)
	      (implies (tlp (cdr x))
		(equal (app2 (cdr x) nil) (cdr x))))
  (implies (tlp x)
    (equal (app2 x nil) x)))

...

QED

QED

Lemma rev2-app2:
(implies (and (tlp x) (tlp y))
  (equal (rev2 (app2 x y)) (app2 (rev2 y) (rev2 x))))

Proof by: Induction on (tlp x)

;; 1. YOU DO NOW
Induction Case rev2-app2-base:
(implies (not (consp x))
  (implies (and (tlp x) (tlp y))
    (equal (rev2 (app2 x y)) (app2 (rev2 y) (rev2 x)))))

Exportation:
(implies (and (not (consp x)) (tlp x) (tlp y))
  (equal (rev2 (app2 x y)) (app2 (rev2 y) (rev2 x))))

Context:
C1. (not (consp x))
C2. (tlp x)
C3. (tlp y)

Derived Context:
D1. (equal x nil) { C1, C2 }

Goal:
(equal (rev2 (app2 x y)) (app2 (rev2 y) (rev2 x)))

Proof:
(equal (rev2 (app2 x y)) (app2 (rev2 y) (rev2 x)))
= { D1 }
(equal (rev2 (app2 nil y)) (app2 (rev2 y) (rev2 nil)))
= { Def app2 }
(equal (rev2 y) (app2 (rev2 y) nil))

... 

QED

Conjecture rev2-rev2:
(implies (tlp x)
  (equal (rev2 (rev2 x)) x))

Proof by: Induction on (tlp x)

;; COPY OVER YOUR SOLUTION TO THIS CASE FROM PART A
Induction Case rev2-rev2-base:
(implies (not (consp x))
  (implies (tlp x)
    (equal (rev2 (rev2 x)) x)))

...

QED

;; 2. YOU DO NOW
Induction Case rev2-rev2-ind:
(implies (and (consp x)
              (implies (tlp (cdr x))
                (equal (rev2 (rev2 (cdr x))) (cdr x))))
  (implies (tlp x)
    (equal (rev2 (rev2 x)) x)))
    
Exportation:
(implies (and (consp x)
              (implies (tlp (cdr x))
                (equal (rev2 (rev2 (cdr x))) (cdr x)))
              (tlp x))
  (equal (rev2 (rev2 x)) x))
  
Context:
C1. (consp x)
C2. (implies (tlp (cdr x)) 
      (equal (rev2 (rev2 (cdr x))) (cdr x)))
C3. (tlp x)

Derived context:
D1. (tlp (cdr x)) { C1, C3 } 
D2. (equal (rev2 (rev2 (cdr x))) (cdr x)) { D1, C2 } 

Goal:
(equal (rev2 (rev2 x)) x)

Proof:
(rev2 (rev2 x))
= { Def rev2 }
(rev2 (app2 (rev2 (cdr x)) (list (car x))))

...

QED

QED

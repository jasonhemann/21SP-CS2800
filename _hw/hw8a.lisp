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

HW 8a: Inductive Proofs

|#

#| 

PART I - GOOF BY INDUCTION

It appears that, [despite what you may have
heard](https://www.youtube.com/watch?v=5T6umD9Z8HA) all horses are the
same color!?!? Read the following "proof" by induction, and answer the
questions below. This example is taken from:

Cohen, Joel "On the nature of mathematical proofs". A Random Walk In
Science. R.L. Weber, compiler, and E. Mendoza, editor. (New York:
Crane, Russak & Co. Inc., 1973), p 34.

|# 

#| 

Theorem:  All horses are the same colour (by induction).

Proof. It is obvious that one horse is the same colour. Let us assume
the proposition P(k) that k horses are the same colour and use this to
imply that k + 1 horses are the same colour. Given the set of k + 1
horses, we remove one horse; then the remaining k horses are the same
colour, by hypothesis. We remove another horse and replace the first;
the k horses, by hypothesis, are again the same colour. We repeat this
until by exhaustion the k + 1 sets of k horses have each been shown to
be the same colour. It follows then that since every horse is the same
colour as every other horse, P(k) entails P(k + 1). But since we have
shown P(1) to be true, P is true for all succeeding values of k, that
is, all horses are the same colour.

|#

;; This specious proof is using natural number induction. Look back at
;; the beginning of section 6.1 to see this. For this question you
;; should ignore the contract case.

;; 1. What does the author say is the base case for his induction? 

#| 
Answer: 

|#

;; 2. What does the author say is the inductive hypothesis of his
;; inductive case?

#| 

Answer: 

|#

;; 3. The inductive case is well-argued. On the basis of his inductive
;; hypothesis for k, he proves the claim for k + 1. It is also true
;; that in all one-horse sets, every horse in the set has the same
;; color. Yes, we all know it fails for 2 horses. Give a technical
;; kind of answer to the question: Why this proof is flawed?

#| 

Answer: 

|# 

;; Copy-and-paste mark.
;; Ye olde pre-existing definitions
(definec app2 (x :tl y :tl) :tl
  (if (endp x)
	  y
    (cons (first x) (app2 (rest x) y))))

 (definec rev2 (x :tl) :tl
   (if (endp x)
	   x
     (app2 (rev2 (cdr x)) (list (car x)))))

#| 

PART II - BEGINNING A BIG OL' INDUCTIVE PROOF

I want you to have the experience of working *through* a proof in the
order in which we'd come to it.

But I want to give you some structure as you work through a
non-trivial inductive proof.

And I want to avoid you having to re-write a whole lot of this. 

So, I'm going to give you a starting bit. I have stated some parts of
the proof. I have also indicated when we "get stuck" and need to
resort to a lemma.

You will complete some key lemmata in this part of your homework.

In the second part of your homework, we will then use those complete
lemmata to complete the remainder of the larger proof.

|# 

;; Place here the proofs of any other lemmas or theorems you decided you
;; need.



;; NB. This is the main proof we begin here and complete in the
;; homework.

#| 

Conjecture rev2-rev2:
(implies (tlp x)
  (equal (rev2 (rev2 x)) x))

Proof by: Induction on (tlp x)

|# 

;; 4. YOU DO THIS ONE NOW
;; Induction Case rev2-rev2-base:
Lemma rev2-rev2-base:
(implies (not (consp x))
  (implies (tlp x)
    (equal (rev2 (rev2 x)) x)))
    

QED

#| 

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

... ¯\_(ツ)_/¯

Intermezzo: 

Right now, we're stuck here. We cannot proceed because there's an app2
call in the way and we don't know about the relationship between app2
and rev2. 

That app2 call is in the way of our making good use of the inductive
hypothesis.  

We need a lemma!  

Hrmm.... How do app2 and rev2 relate to one another? An example:

(rev2 (app2 '(1 2 3) '(4 5 6))) = (app2 (rev2 '(4 5 6)) (rev2 '(1 2 3)))
'(6 5 4 3 2 1)                    

So h'bout (equal (rev2 (app2 x y)) (app2 (rev2 y) (rev2 x)))

|#

#| 

Lemma rev2-app2:
(implies (and (tlp x) (tlp y))
  (equal (rev2 (app2 x y)) (app2 (rev2 y) (rev2 x))))

Proof by: Induction on (tlp x)

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

¯\_(ツ)_/¯

But here we get stuck again! Whatever shall we do! 
We don't know enough to reduce the other side of the
inequality. Whatever shall we do? ... INDUCTION!

|# 

Lemma app2-nil:
(implies (tlp x) 
  (equal (app2 x nil) x))

Proof by: Induction on (tlp x)

;; 5. YOU DO NOW
Induction Case app2-nil-base:
(implies (not (consp x))
  (implies (tlp x)
    (equal (app2 x nil) x)))

... 

QED

;; 6. YOU DO NOW
Induction Case app2-nil-cons:
(implies (and (consp x)
	      (implies (tlp (cdr x))
		(equal (app2 (cdr x) nil) (cdr x))))
  (implies (tlp x)
    (equal (app2 x nil) x)))

...

QED

QED

;; We will continue from here in the homework.



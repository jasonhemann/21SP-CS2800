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

HW 7b. More on Measures, Admission and Definition

|#

#| 

After a slight reprieve, I want to re-visit this measure function
stuff, because we /will/ need it again.

As a reminder, you are welcome to try these functions on ACL2s, but
you cannot simply report what ACL2s reports to you. You have to
provide your own measure arguments.


1. There are many functions that are admissible, but ACL2s will not
   admit them without help from the user. 

2. It is possible that ACL2s has bugs! If so, it is possible that it
   erroneously proves admissibility in some cases.

|# 

#| 

 Here is a function that you may find useful when constructing a
 measure function for the magnitude function given below.
 The ceil function is non-recursive, so you don't have to prove
 its termination.

|# 

(definec ceil (x :non-neg-rational) :nat
  (ceiling x 1))

; Here are some tests showing that (how) ceil works.

(check= (ceil 11) 11)
(check= (ceil 10/3) 4)
(check= (ceil 10/101) 1)

(definec len2 (x :tl) :nat
  (if (endp x)
      0
    (+ 1 (len2 (rest x)))))

;; You can ignore these; they adjust some ACL2s settings so that
;; definec only uses the provide measure function to prove
;; termination, and so that definec doesn't do as much magic as it
;; would otherwise.

(set-termination-method :measure)
(set-well-founded-relation n<)
(set-defunc-typed-undef nil)
(set-defunc-generalize-contract-thm nil)
(set-gag-mode nil)


;; 1. Come up with an acceptable measure function for each of the
;; functions below. We once again use the (set-ignore-ok t) form.
(set-ignore-ok t)


(definec f-measure

  )

(definec magnitude-measure

  )

;; B. Finish adding your measure to the function definition, to help
;; ACL2 show termination. You can modify the function definition if
;; you like, but make sure it's equivalent.

(definec f (x :tl y :tl acc :tl) :tl
  (declare (xargs :measure ))  
  (cond
    ((and (endp x) (endp y)) acc)
    ((endp x) (f x (rest y) (cons (first y) acc))) ;; a
    (t (f (rest x) y (cons (first x) acc)))))      ;; b

;; Oh no! ACL2s is in trouble! It needs *YOUR* help!
:program

(definec magnitude (x :non-neg-rational) :integer
  (declare (xargs :measure ))
  (cond 
    ((equal x 0) 0)
    ((>= x 10) (+ 1 (magnitude (/ x 10)))) ;; a
    ((< x 1) (* -1 (magnitude (/ 1 x))))   ;; b
    (t 1)))

#| 

NB: You might need to understand how magnitude works. One way to do
that is to "trace" it and try running it on examples.

|# 

(trace$ magnitude)

(magnitude 20)
(magnitude 200000)
(magnitude 200000000000)
(magnitude 1/20)
(magnitude 1/200000)
(magnitude 1/200000000000)

(untrace$ magnitude)

:logic

;; C. State, as a quoted list, the contract theorem (condition 5).


(defconst *f-contract-theorem*

  )

(make-event `(thm ,*f-contract-theorem*))


(defconst *magnitude-contract-theorem*

  )

(make-event `(thm ,*magnitude-contract-theorem*))

;; D. Prove that the function is terminating using your measure
;; function and equational reasoning for the recursive calls.


Conjecture fa-terminates:

...

QED


Conjecture fb-terminates:

...

QED



Conjecture magnitudea-terminates:

...

QED


Conjecture magnitudeb-terminates:

...

QED


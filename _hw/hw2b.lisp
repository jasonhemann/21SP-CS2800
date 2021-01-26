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


#| 
  - These commands are simplifying your interactions with ACL2s

  - Do not remove them.

  - To learn more about what they do, see Ch2 found on the course
	readings page
|#

(set-defunc-termination-strictp nil)
(set-defunc-function-contract-strictp nil)
(set-defunc-body-contracts-strictp nil)


;; Part I: Defining naturally-recursive functions.

#| 
 For this part, you will complete a set of programming exercises. Give
 yourselves enough time to develop solutions and feel free to define
 helper functions as needed.

 NOTE: You can scatter properties, checked with THM or TEST?,
 throughout your solution. These are not a required part of the
 homework; we often use them as (1) an aid in debugging the code and
 (2) machine-checked documentation (3) in our autograder. You are of
 course welcome to start to play with them.

|# 

;;; 1. Define the function DROP-LAST that takes a non-empty true-list
;;; and returns a list without the last element of its input.


#|

 Here, we are saying that DROP-LAST produces a list that is
 one element shorter than its input.

|# 


(defthm drop-list-removes-one
  (implies (ne-tlp xs)
	   (= (len (drop-last xs))
	      (- (len xs) 1))))

;;; 2. Define and test a function INSERT-RIGHT that takes two symbols
;;; and a true list and returns a new list with the second symbol
;;; inserted after each occurrence of the first symbol.


(check= (insert-right 'x 'y '(x z z x y x)) '(x y z z x y y x y))

#|

The Lisp function ASSOC takes x, an element of the universe, and l, a
list of pairs. ASSOC returns the first pair in the list whose CAR is
x. If there is no such pair, ASSOC returns nil. ASSOC is built in. You
can try it at the REPL.

This function is named ASSOC because the second data structure is
called an association list. An association list is a (true) list of
pairs of associated values. For example, the following is an
association list:

((A . 5) (B . (1 2)) (C . A))

BTW, you may find the built-in ALIST and ALISTP of use to you. 
 
|# 

;;; 3. Write MY-ASSOC, your own implementation of the lisp ASSOC
;;; function. You should not use ASSOC anywhere in your definition. 

(defdata maybe-pair (oneof nil cons))



#| 

The Lisp function REMOVE takes x, an element of the universe, and l, a
list. REMOVE returns a list with, as the name implies, every occurence
of x removed. This function does not "recur deeply", it only operates
at the top level of the list. 

> (remove 'x '(a (x y) b x c x d (x)))
(A (X Y) B C D (X))

You can try it at the REPL. In the following problem, you should not
use REMOVE.

|# 

;;; 4. Define and test a procedure REMOVE-FIRST that takes a symbol
;;; and a true list and returns a new list with the first occurrence
;;; of the symbol removed.


;;; 5. Define and test a procedure MIRROR that takes an element of the
;;; acl2 universe (including CONS-constructed binary trees like those
;;; we discussed in lecture) and recursively exchanges each CAR with
;;; its CDR. 

(check= (mirror 'a) 'a)
(check= (mirror '(1)) '(nil . 1))
(check= (mirror '((g h (a . b) . (c . d)) . (e . f)))
	'((f . e) (((d . c) b . a) . h) . g))
(check= (mirror (mirror '(on (the (wall))))) '(on (the (wall))))

;;; 6. Define a function CONS-CELL-COUNT that takes an element of the
;;; acl2 universe and counts the number of CONS cells (i.e. the number
;;; of pairs) in the input.

(check= (cons-cell-count 'fish) 0)
(check= (cons-cell-count '()) 0)
(check= (cons-cell-count '(a . b)) 1)
(check= (cons-cell-count '(a b)) 2)
(check= (cons-cell-count '((a b) c)) 4)
(check= (cons-cell-count '((((((()))))))) 6)

#| 

 We can encode natural numbers as backwards-binary true-lists of
 booleans. These are, in the business, called "little-endian" binary
 numbers. E.g. 9 in this representation is '(t nil nil t). This is
 unambiguous because in our notation these numbers never end in nil.

|#

(defdata pos-bb (oneof '(t) (cons t pos-bb) (cons nil pos-bb)))
(defdata bb (oneof nil pos-bb))

;;; 7. Write a function BB-TO-N that takes one of our backward-binary
;;; numbers and returns the nat to which it corresponds. You should
;;; solve this without changing the method's signature. Nor should you
;;; add an accumulator or auxilliary variable in a help method.

#| 

 Here are some fascinating properties you might want to *prove* for
 such lists

|# 

(test? (implies (and (bbp n) (consp n))
              (and (<= (expt 2 (len (cdr n))) (bb-to-n n))
                   (< (bb-to-n n) (expt 2 (len n))))))

(test? (implies (pos-bbp x)
              (= (* 2 (bb-to-n x)) (bb-to-n (cons nil x)))))

;;; 8. Write a function LIST-INDEX-OF that takes an ACL2 value x, and
;;; a true-list l containing at least one x, and returns the 0-based
;;; index of the first x in l. 


;;; 9. Write a function ZIP-LISTS that takes two true-lists l1 and
;;; l2. ZIP-LISTS returns an alist formed from pairs of elements
;;; taken from with the car coming from l1 and the cdr coming from
;;; l2. If the lists are of uneven length, then drop the tail of the
;;; longer one.


(test? (implies (and (tlp l1) (tlp l2))
		(= (len (zip-lists l1 l2)) (min (len l1) (len l2)))))


;;; 10. Write a function UNZIP-LISTS that recursively deconstructs an
;;;  alist and returns a pair of true-lists. The car of this pair is a
;;;  true list of the cars, in order, of each pair in the input. The
;;;  cdr of this pair is a true list of the cdrs, in order, of each
;;;  pair in the input.


(check= (unzip-lists '((a . b) (c . d) (e . f))) '((a c e) . (b d f)))
(check= (unzip-lists '((()))) '((()) ()))

(test? (implies (alistp e)
		(equal (let ((v (unzip-lists e)))
			 (zip-lists (car v) (cdr v)))
		       e)))

;;; Part II Computational complexity with static & dynamic contract checking

#|

Recall the following definitions from the Manolios text. 

(definec listp (l :all) :bool 
  (or (consp l)
      (equal l () )))

(definec endp (l :list) :bool
  (atom l))

(definec true-listp (l :all) :bool
  (if (consp l)
      (true-listp (cdr l))
    (equal l ())))

(definec binary-append (x :tl y :all) :all
  (if (endp x)
      y
    (cons (first x) (binary-append (rest x) y))))

This exercise will require you to use what you learned solving
recurrence relations from discrete.

In this section, we will explore the difference between static and
dynamic type checking.

To answer the above questions, we will assume (just for this exercise)
that the following operations have a cost of 1:

 cons, first, rest, consp, atom, or, equal, not, if

We will also assume for this first set of questions that static
contract checking is used.  With static contract checking, ACL2s
checks that the arguments to the function satisfy their contract only
once, for the top-level call. For example, if you type:

(binary-append '(1 2 3 4) '(5 6))

ACL2s checks that '(1 2 3 4) is a true-list and no other contracts.

Remember also that we want the worst-case complexity.  So if the
function has an if-then-else, you must compute separately the
complexity of the then branch, the else branch, and then take the
worst case (i.e., the maximum), plus the complexity of the if
condition itself.

To get you going, we will give the complexity of listp as an example.
Checking the contract statically takes no time, since the type of

x is :all. 

Independently of the size of x, there are 3 operations: (consp x),
(equal x nil), and the or. So the complexity is O(3)=O(1), that is,
constant time. 

Notice that we want the complexity of the functions assuming that the
top-level checking has been already been done.

Please include in your answer a sufficient explanation of how you
reached that answer.  

|#

;; 11. What is the computational complexity of endp?

;; 12. What is the computational complexity of true-listp?

;; 13. What is the computational complexity of binary-append?


#| 

One way of implementing dynamic checking is to have every function
dynamically check its input contracts. Think about how you might do
that before reading further. So, the above definitions get transformed
into the following. In essence, we are forcing contract checking to
happen dynamically, during runtime.

(definec listp (x :all) :bool
  (or (consp x)
      (equal x nil)))

(definec endp (x :all) :bool
  (if (listp x)
      (atom x)
    (error)))

(definec true-listp (x :all) :bool
  (if (consp x)
      (true-listp (rest x))
    (equal x nil)))

(definec binary-append (x :tl y :all) :all
  (if (true-listp x)
      (if (endp x)
          y
        (cons (first x) (binary-append (rest x) y)))
    (error)))

Please include in your answer a sufficient explanation of how you
reached that answer.  

|# 

;; 14. What is the computational complexity of the modified listp?

;; 15. What is the computational complexity of the modified endp?

;; 16. What is the computational complexity of the modified true-listp?

;; 17. What is the computational complexity of the modified binary-append?

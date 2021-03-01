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

HW 7a: Undecidability Results

|#


#| PART I: A READING ASSIGNMENT ABOUT BIG NUMBERS |# 				       

;; This is a reading assignment. You will have to read, and be
;; prepared to discuss, the following article:
				       
;; https://www.scottaaronson.com/writings/bignumbers.html
				       
;; You will (likely) have an in-class discussion based on this. So you
;; should absolutely expect to have this understanding checked and
;; verified.
				       
;; If they come up, you should also be prepared to answer some
;; questions individually about this article. You do not need to write
;; anything here, however.

	
#|  Part II: DIAGONALIZATION |# 

;; 1. Using a diagonalization argument as described in section 5.4.2
;; and used in 5.4.2 and 5.4.4, demonstrate the nondenumerability of
;; the real numbers 0 <= r <= 1. Hint: you might find this especially
;; elegant if you work in base 2. Like the examples from the book,
;; this should look much closer to an "informal proof" from a math
;; class than to an ACL2 or pen-and-paper proof.

#| 

Answer: 

|# 

#| Part III: Undecidability of the Busy Beaver Problem. |# 

;; You have now seen and understood proofs of the undecidability of
;; the halting problem. You have encountered this at minimum in the
;; course textbook, in the "Scooping ..." reading and as discussed in
;; lecture.

;; You have also now read and understood a great deal of the proof and
;; history of the busy beaver problem. You should be able to argue its
;; undecidability, via an informal proof. In fact, I don't think you
;; would find that challenging at all.

;; So, for a little added flavor, you will express a proof of the
;; undecidabliity of the busy beaver problem in Seussian rhyme, a la
;; "Scooping." Provide your answer below. You may use slant rhymes if
;; you need, or regional accents if you must (e.g. "data"/"later").

;; Please follow your Seussian proof with an explanation of it in
;; plain English prose. Where not immediately, transparently obvious
;; point us to what each piece is accomplishing. This should take you
;; several stanzas---approximately the same length as "Scooping ...".

;; Consider: if you do this well, you too could get your proof
;; published!!


#| 

Title: 

Poem: 

Commentary: 

|# 

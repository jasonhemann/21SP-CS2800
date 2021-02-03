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

HW4a

|#

(set-defunc-termination-strictp nil)
(set-defunc-function-contract-strictp nil)
(set-defunc-body-contracts-strictp nil)

#|

PART I: SETS AND SETUP
===============================================================================

|#

(defdata binop (enum '(& v => == ><)))
(defdata propexpr (oneof bool
                         var
                         `(! ,propexpr)
                         `(,propexpr ,binop ,propexpr)))

;; 1. You should implement here COUNT-SUBFORMULAE, a function that
;; takes a propexpr p and returns a count of the number of
;; sumformulae. In your implementation here you need not take
;; uniqueness into account. You should use CASE-MATCH to match against
;; the cases of your datatype in your definition.

#| 

NB. Consider the formula itself one of it's own subformulae.

|#


(check= (count-subformulae '((X & X) & X)) 5)
(check= (count-subformulae 'Z) 1)
(check= (count-subformulae '(! Z)) 2)

(thm (implies (and (propexprp X)
   	           (propexprp Y))
	(= (count-subformulae `(,X & ,Y))
	   (+ 1 (count-subformulae X)
	        (count-subformulae Y)))))

;; 2. You should implement here ALL-SUBFORMULAE, a function that takes
;; a propexpr p and returns a sorted list of all the unique
;; subformulae of p. Do notice that like in the previous problem, we
;; consider a formula one of it's own subformulae. You should use
;; CASE-MATCH in your definition.

#| 

You should not implement your own union. You should instead make use
of [the SET library, implemented with ordered
sets](https://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____STD_F2OSETS).

|#


(check= (all-subformulae '((X & X) & X))
	'(X (X & X) ((X & X) & X)))

(check= (all-subformulae '((X & Y) & (X & Y)))
	'(X Y (X & Y) ((X & Y) & (X & Y))))

(test? (implies (and (propexprp P)
 		     (propexprp Q))
		(= (len (all-subformulae `(,P => ,Q)))
		   (len (all-subformulae `(,P >< ,Q))))))

(test? (implies (and (propexprp Q))
		(= (len (all-subformulae `(! ,Q)))
		   (1+ (len (all-subformulae Q))))))


#|
PART II: SIGNED IN BLOOD
===============================================================================
|#

#|

It turns out that we represent truth tables in ACL2s. We represent
them as lists of lists.

The following are websites that construct tables for you. 

https://www.tablesgenerator.com
https://tableconvert.com

If you work in Emacs, you must know you have already a built-in
[spreadsheet
editor/constructor](https://orgmode.org/manual/Tables.html#Tables). With
~orgtbl-mode~ you could use this functionality directly in your
buffer.

Or you could create such a table in a spreadsheet tool, or something
like that. You should know how to programmatically translate these
into our own format. 

|# 

;; 3. In the space below, re-write the following sentence: "I will not
;;    construct these truth tables by hand."  You may hold your hand
;;    to your heart as you do so.

#| 
Answer: 

|# 

#| 

You can use count-subformulae to test that you've haven't missed any
columns to your truth tables.

|# 

#|
PART III: PROPOSITIONAL LOGIC BASICS
===============================================================================

NB. Resources like [the
following](https://truthtablemaker.com/#advanced:~:text=Advanced%20Truth%20Table,-This)
obviously exist online. If they did not, we could easily build one in
ACL2. So let me suggest then that you try to do the below exercises
yourself, and then use something like this to check your final
answer. You get all the practice and should lose no points on this
section. 

For each of the following Boolean formulas:

A. Construct the truth table as a row-major list of lists. Your first
   row must be a "header" listing each of the forumula's
   subexpressions, with the formulas' variables listed first. Notice
   you must fully parenthesize these expressions. You will have
   2^{number of distinct variables in the formula}+1 rows in your
   table.

B. Indicate if the formula is
   'valid
   'unsatisfiable
   
   or, if it is instead both satisfiable and falsifiable, write n, the
   number of satisfying assignments.

C. Answer t if this is a minimal formula for these assignments.
   Here "a minimal formula" means that no propositionally equivalent
   formula has fewer subformulae. If it is not a minimal formula,
   instead construct a truth table for an equivalent minimal
   formula. The variable columns of the tables for the old and new
   formulae should be identical. That is to say, even if you wind up
   simplifying away a variable, include it in this truth table anyway
   so that you can compare the new truth table with the previous
   one. 

   (If you answered "no" for part C, you *could* try to use the ACL2s
   logical operators and write a TEST? expression stating that the
   original formula and your new formula are equivalent. ACL2s
   includes a decision procedure for validity, so you can use it as a
   SAT/validity solver to check your work. For example, you can use
   it to check your characterization of formulas in part B, above.)

   We quote the data here for you so that your file will run. 

   Here is a worked-out example for you.

|#

(defdata partb (oneof (enum '(valid unsatisfiable)) nat))

(defconst *example*
  '((q v (! q)) & p))

;; A: 

(defconst *example-a* 
  '((p   q   (! q) (q v (! q)) ((q v (! q)) & p))
    (T   T   NIL   T           T)
    (T   NIL T     T           T)
    (NIL T   NIL   T           NIL)
    (NIL NIL T     T           NIL)))

;; B:

(defconst *example-b* 2)

;; C: 

(defconst *example-c*
  '((p   q   p)
    (T   T   T)
    (T   NIL T)
    (NIL T   NIL)
    (NIL NIL NIL)))
                             
;; 4.

(defconst *problem-4*
  '((p => nil) == (! p)))


;; A:
(defconst *problem-4a* )

;; B: ...
(defconst *problem-4b* )

;; C: ...
(defconst *problem-4c* )


;; 5.
(defconst *problem-5*
  '(((r => (r => q)) => (r => q))))


;; A: ...					
(defconst *problem-5a*  )

;; B: ...					
(defconst *problem-5b*  )
(partbp *problem-5b)

;; C: ...
(defconst *problem-5c*  )


;; 6. 
(defconst *problem-6*
  '((! (! p)) => p))


;; A: ...					
(defconst *problem-6a*  )

;; B: ...		
(defconst *problem-6b*  )			
(partbp *problem-6b)

;; C: ...		
(defconst *problem-6c*  )			

#|
					
PART IV: Read and React			
=============================================================================== 
					
Read [the following article/blog	
post](https://research.swtch.com/boolean) on Minimal Boolean Formulas
and write a reaction via answers to the following questions. You may 
not understand everything, but that's okay! If you are on reading on 
Desktop, and the fonting produces odd artifacts, consider opening the 
Inspector in your browser (i.e. "Inspect Element") and disable the 
'Minion Pro' font family styling.      
					
You may not need to put all of these information and techniques into 
practice; this is a blog post. But consider reading about [How to read 
a research paper](https://dl.acm.org/doi/10.1145/1273445.1273458). 
					
BTW, although the related website is longer active, you can find the 
author's code archived			
[here](https://code.google.com/archive/p/boolean-oracle/). 
					
Your answers to each should be one to a couple of paragraphs. 
					
|#

;; 7. What is the author's primary result in this work?
#| 
Answer: 

|# 

;; 8. What background problems or related work does the author mention
;;    to motivate/inspire/contextualize his work?
#| 
Answer: 

|# 

;; 9. List three technical terms/results/"computer sciencey things"
;;    that are mentioned in the piece with which you were previously
;;    unfamiliar and explain them as you now understand them (as a
;;    result of reading about them in this context, and from looking
;;    them up to find out more for background.)
;;    
#| 
Answer: 

|# 

;; 10. What data does the author present, and what do they mean? (By
;;    "what do they mean?" we want an answer of sufficient depth and
;;    explanation that someone who had only read titles/descriptions
;;    of those same data would not be able to give the same answer.)
#| 
Answer: 

|# 


;; 11. What problems/questions does the author mention remain
;;     open/unsolved? If this inspires you to any questions of your
;;     own, list some of those here as well.
#| 
Answer: 

|# 



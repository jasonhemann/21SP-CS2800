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

(set-defunc-termination-strictp nil)
(set-defunc-function-contract-strictp nil)
(set-defunc-body-contracts-strictp nil)

#| 

3b is continuates of what you studied in 3a earlier this
week. Therefore, you may find it of considerable interest to complete
3a first, and then return to this afterward. Don't sleep on this one,
though.

|# 

#| 

PART I: PROPOSITIONAL LOGIC BASICS
===============================================================================

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

   Please refer to your 3a for a worked-out example.

|# 
(defdata partb (oneof (enum '(valid unsatisfiable)) nat))

;; 1.
(defconst *problem-1*
  '((p => q) => (! (p => (! q)))))

;; A: ...
(defconst *problem-1a* )

;; B: ...
(defconst *problem-1b* )
(partbp *problem-1b*)

;; C: ...
(defconst *problem-1c* )


;; 2.
(defconst *problem-2*
  '(((r => q) => r) => r))

;; A: ...
(defconst *problem-2a* )

;; B: ...
(defconst *problem-2b* )
(partbp *problem-2b*)

;; C: ...
(defconst *problem-2c* )


;; 3. 
(defconst *problem-3*
  '((r => (! q)) => (q => (! q))))

;; A: ...
(defconst *problem-3a*)

;; B: ...
(defconst *problem-3b*)
(partbp *problem-3b*)

;; C: ...
(defconst *problem-3c*)


#| 
PART II: IN WE RECOGNIZE THE LION BY HIS CLAUSE
===============================================================================

We know that it can be quite complicated to find satisfying
assignments for boolean formulae, or even to know if one exists. In
fact, the boolean satisfiability problem is NP-Complete. This places
it with the hardest of the hard problems in NP. So, we will set our
sights a little lower. We will restrict ourselves to a particular form
of boolean formulae and study if that helps.

We will study here propositional formulae in a form called HSF. HSF
formulae are a special instance of formulae in CNF. The HSF formulae
are, like CNF formulae, a conjunction of disjunctions of literals. In
HSF, each sequence of disjunctions (a sequence of disjunctions is
called a "clause") has at *most* one positive literal (i.e. at *most*
one non-negated propositional variable.) Notice this simplification
removes the boolean constants for truth and falsity themselves.

;; The following are clauses:
|#

'((! C) v A v (! B) v (! D))
'((! C) v (! B) v (! D))
'((! B) v (! D) v (! C))
'((! D) v C)
'(C)

#|

We can simplify the format even more. Since it's logically equivalent,
we will arrange the clause so we list the positive literal at the end,
if the clause contains a positive literal. In fact, for those with a
positive literal, we will write them as implications. A conjunction of
atoms imply one more. E.g., the implication B ^ C ^ D -> A for the
first clause above. 

|#

;; 4. (Offer an explanation, in terms of our propositional
;; equivalences, why or how this transformation is logically sound.)

#| 

We will, in fact, go a step or two farther. We will instead write our
implications, (and our arrows) backwards! In this syntax, Accessing
the positive literal is simply taking the CAR. 

Since the "antecedents" of these implications are always a sequence of
conjunctions, we shall elide writing out the '&'s and the
parentheses. We will just hold the antecedents in a list. But you and
I will know that the '&'s are there. 

This merely syntactic simplification doesn't make the problem easier,
it just makes our lives easier.

Here is the result of the transformation for 3 of the clauses above.

|#

'(A <= (B C D))
'(C <= (D))
'(C <= ())


#| 

We will simplify further still! Since all clauses *without* a positive
literal are all disjunctions of negations, we don't need to write out
the disjunctions. And in fact, we don't *need* to write out the
negations. As long as we promise to keep track of which kind of clause
is which.

|# 

'(C B D)
'(B D C)

#| 

Further, since all formulae in HSF are conjunctions of HCs, we will
likewise omit writing the '&'s between the clauses. And we will list
separately the clauses with a positive literal and those without. Once
again, this bit of notational housekeeping is just for our piece of
mind.

|# 

'((A <= (B C D))
  (C <= ())
  (C <= (D)))

'((C B D)
  (B D C))

#| 

One final bit of housekeeping to simplify our lives, that you may or
may not have already noticed. We will require that our lists of
clauses, and the lists of negative literals in each clause, are listed
in sorted order, and unique. That is to say, we represent them as
ordered sets.

|#

'((B C D))

;; 5. Your task: implement an efficient (polynomial time) algorithm
;; SATP for deciding the satisfiability of an HSF sentence in our
;; format.


#|

First, let us heavily suggest making use of [the SET library,
implemented with ordered
sets](https://www.cs.utexas.edu/users/moore/acl2/manuals/current/manual/?topic=ACL2____STD_F2OSETS).

Secondly, although we do not intend to spoil your fun and excitement
from learning, let us heavily suggest that you digest and entertain
one or both of the algorithms described in [this
paper](https://www.sciencedirect.com/science/article/pii/0743106684900141).

Third, a hint: Sometimes you might need to write a predicate in order
to describe the desired input as an input contract.

Here were two more satp hints that some folk found useful.

1. You might consider writing another function that does the bulk of the work, and calling that from within the body of satp. 

2. You might consider at first presuming all variables are false until you determine otherwise, and keep those variables that you now know to be true, as a set, in an accumulator.
|# 

(defdata gc (listof var))
(defdata gcs (listof gc))
(defdata dc `(,var <= ,gc))
(defdata hsf (listof dc))


#| Given some starter code |# 

(definec satp (prg :hsf goal :gcs) :bool
  ...) 

(check= (satp '((r <= ()))
	      '((r)))
	'nil)

(check= (satp '((r <= (s)))
	      '((r)))
	't)

(check= (satp '((r <= (s))
		(s <= (r)))
	      '((s)))
	t)

(check= (satp '((p <= (a b c d e))
		(p <= (q r s v))
		(r <= ())
		(r <= (v x))
		(s <= ())
		(s <= (p q)))
	      '((r s)))
        nil)

(check= (satp '((p <= (a b c))
                (q <= (a b c))
                (r <= (p q)))
              '((r)))
        t)

(check= (satp '((a <= ())
                (b <= ())
                (c <= ())
                (p <= (a b c))
                (q <= (a b c))
                (r <= (p q)))
              '((q) (r)))
        nil)



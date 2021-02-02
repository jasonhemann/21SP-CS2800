#|

Lab 4

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
;; uniqueness into account. You should use CASE-MATCH in your
;; definition.

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

The following are websites that construct tables for you. You could
type in the data by hand, or you could import it from a spreadsheet
tool, or something like that.

https://www.tablesgenerator.com
https://tableconvert.com/

If you work in Emacs, you must know you have already a built-in
[spreadsheet
editor/constructor](https://orgmode.org/manual/Tables.html#Tables). With
~orgtbl-mode~ you could use this functionality directly in your
buffer.

|# 

;; 3. In the space below, re-write the following sentence:
;;    "I will not construct these ascii truth tables by hand."
;;    You may hold your hand to your heart as you do so.


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

A. Construct the truth table. Create a column for each of the
   forumula's subexpressions. List the formulas' variables in the
   leftmost columns, and list those variables in alphabetical order.

B. Indicate if the formula is (a) valid, (b) unsatisfiable, or (c)
   both satisfiable and falsifiable. 

B2. If you chose (c) also indicate how many assignments satisfy the
   formula.

C. Answer, with "yes" or "no": "Is this a minimal formula for these
   assignments?" Here "a minimal formula" means that no
   propositionally equivalent formula has fewer subformulae. 

C2. If you chose "no", demonstrate this by finding an equivalent
   minimal formula and constructing a truth table for your new
   formula. The final columns of the old and new formulae should be
   identical. If you wind up simplifying away a variable, include it
   in the truth table anyway so that you can compare the new truth
   table with the previous one.

C3. If you chose "no", write a TEST? expression stating that the
   original formula and your new formula are equivalent. This should
   *not* be in a comment. ACL2s includes a decision procedure for
   validity, so you can use it as a SAT/validity solver to check your
   work. (For example, you can use it to check your characterization
   of formulas in part B, above.) Use the ACL2s logical operators for
   your expressions.

 (If you want to also automate the checks for the number of
  subformulae, you would operate over *our* expression language, and
  use `valof` from lec5.lisp, or implement a translator between our
  language and acl2s logic expressions, and show a theorem about how
  they behave wrt one another. We will not do this.)

Here is a worked-out example for you.

((q v (! q)) & p)

A: 

| p  | q  | (! q)  | (q v (! q))  | ((q v (! q)) & p)  |
|----+----+--------+--------------+--------------------|
| tt | tt |     ff |           tt |                 tt |
| tt | ff |     tt |           tt |                 tt |
| ff | tt |     ff |           tt |                 ff |
| ff | ff |     tt |           tt |                 ff |

B: (c) both satisfiable and falsifiable

B2: 2

C: no

C2: 

|  p |  q |  p | 
|----+----+-----
| tt | tt | tt |
| tt | ff | tt |
| ff | tt | ff |
| ff | ff | ff |

C3:
|#

(test? (implies (and (booleanp p)          
                     (booleanp q))
		(equal (and (or q (not q)) p)
		       p)))  
                             
;; 4. ((p => ff) == (! p))

#|
A: ...
B: ...
C: ...
|#


;; 5. (((r => (r => q)) => (r => q)))

#|
A: ...
B: ...
C: ...
|#

;; 6. ((! (! p)) => p)

#|
A: ...
B: ...
C: ...
|#


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


;; 7. 

;; a. What is the author's primary result in this work?

;; b. What background problems or related work does the author mention
;;    to motivate/inspire/contextualize his work?

;; c. List three technical terms/results/"computer sciencey things"
;;    that are mentioned in the piece with which you were previously
;;    unfamiliar and explain them as you now understand them (as a
;;    result of reading about them in this context, and from looking
;;    them up to find out more for background.)
;;    

;; d. What data does the author present, and what do they mean? (By
;;    "what do they mean?" we want an answer of sufficient depth and
;;    explanation that someone who had only read titles/descriptions
;;    of those same data would not be able to give the same answer.)


;; e. What problems/questions does the author mention remain
;;    open/unsolved? If this inspires you to any questions of your
;;    own, list some of those here as well.



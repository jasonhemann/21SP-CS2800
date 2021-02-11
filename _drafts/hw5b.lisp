#|

HW 5

This part of your homework is a continuation of hw5a. So,
please do start with that and then proceed apace.

|#


#|

NB. As we noted, we have deliberately removed our earlier allowances
that let us elide checking function and body contracts. We now know
what these are and understand them, so let's use this lab and HW to
get more comfortable.

|# 

(defdata
  (expr (oneof var
           rational
           (list 'quote expr)
           (list '* expr expr)
           (list 'let (list (list var expr)) expr))))

#| 

As discussed, our substitution into programs will only apply to free
variable occurrences. For a re-refresher see Ch 4. pp 76-77.

|# 


#| 

PART II: MORE EQUATIONAL REASONING

This too is a continuation from the previous homework. Prove the
following related conjectures. That is, for each of the conjectures
below:

1. Perform conjecture contract checking and add hypotheses if
   necessary. Contract completion is the process of adding the minimal
   set of hypotheses needed to guarantee that the input contracts for
   all functions used in the conjecture are satisfied. See the lecture
   notes. Do not add anything else to the conjecture.

2. Determine if the resulting conjecture is valid or not.  If it is
   valid, provide a proof, as outlined in the lecture notes and in
   class. If it is invalid, provide a counterexample. 

See your lab for more explicit instructions. Make sure to follow the
same instructions as in lab when giving proofs. We redefine below the
set of function definitions you can use.

Once again, I cannot /force/ you to, but you should absolutely use
ACL2s to check the conjectures you come up with, and you should use
the [automated proof checker](http://checker.atwalter.com/checker) to
check the proofs up with which you come. 

EDIT. You might /try/ it, but YMMV. 

|#

;;; Function Definitions

(definec len2 (x :tl) :nat
  (if (endp x)
      0
    (+ 1 (len2 (rest x)))))

(definec in2 (a :all l :tl) :bool
  (if (endp l)
      nil
    (or (== a (first l)) (in2 a (rest l)))))
    
(definec app2 (x :tl y :tl) :tl
  (if (endp x)
      y
    (cons (first x) (app2 (rest x) y))))

(definec rev2 (x :tl) :tl
  (if (endp x)
      x
    (app2 (rev2 (cdr x)) (list (car x)))))


#| 

;; 4. A revv'er-upper

Conjecture 4:
(implies (boolp (boolp (boolp x))) (boolp (boolp x)))

...

QED

;; 5. Prove the following conjecture.

Conjecture 5:
(implies (tlp ls)
  (implies (endp ls)
    (equal (rev2 (rev2 ls)) ls)))
    
...

QED   

|# 

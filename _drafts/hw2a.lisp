#| 

In the first half of this homework, we will practice some skills you
will need throughout this course. You may even find some of these
functions helpful to you in the second half of this homework

These following commands are simplifying your interactions with
ACL2s. Do not remove them.

To learn more about what they do, see Ch2 found on the course readings
page

|#

(set-defunc-termination-strictp nil)
(set-defunc-function-contract-strictp nil)
(set-defunc-body-contracts-strictp nil)

#| 

To answer the following questions, write each function using definec.
Do not make use of any lisp functions that make the problems trivial.
So, for the most part these should be explicitly recursive functions
whose body is a cond. You should find yourself making freqent use of
cons, car, and cdr.

|#


;; 1. Define a function MY-MEMBER takes an element of the universe and
;; a true list and that behaves like Lisp's MEMBER function: 



;; 2. Define MY-SNOC, a version of lisp's SNOC function (it's like
;; cons, but backward). The first argument should be a true-list, the
;; second any element of the acl2 universe.


(check= (my-snoc '(a b c d) 'e) '(a b c d e)) 

;; 3. Define a function RAC that returns the last element of a
;; non-empty true-list. (RAC, because it's like CAR, but the other
;; way.)


(check= (rac '(a)) 'a)
(check= (rac '(a b c)) 'c)

;; 4. Define a function RDC that takes a non-empty true-list and
;; returns a list of all but the last element of its input. (RDC,
;; because it's like CDR, but the other way.)


(test? (implies (ne-tlp l) (equal (snoc (rdc l) (rac l)) l)))

;; 5. Define NAT-TO-BINARY, that takes a nat and returns a
;; little-endian binary number. You can decide how you want to
;; describe the output.

(check= (nat-to-binary 0) '())
(check= (nat-to-binary 1) '(t))
(check= (nat-to-binary 6) '(nil t t))

;; 6. Define MY-REMOVE, a function like lisp's REMOVE that takes an
;; element of the universe x and a true-list l, and removes each
;; occurrence of x from l. Does not recur deeply.


#| 

A predicate is a name for functions with codomain {t, nil}. We use the
word predicate, because we can think of such a function as defining a
way to separate the function's domain into the elements that have the
property, and those elements that do not have the property. 

|# 

;; 7. Define LIST-SET, a predicate on true-lists that returns t when
;; the true-list is free of duplicates, and nil otherwise.


(check (list-set '(a b c d f (a b c))))
(check (list-set '()))
(check (list-set '(a (a) ((a)))))
(check (not (list-set '(a x b x c d))))
(check (not (list-set '(a (x) b (x) d))))

#| 

As you will remember from discrete, a set is a collection of elements
without order or multiplicity. We will represent our sets with
lists. As we know, the order of elements in a list matters. This
implies that for functions like LIST-SET-DIFFERENCE, we will need a
test more generous than mere list equality to determine whether an
implementation is correct. Consider and bear this in mind.

|# 

;; 8. Define LIST-SET-DIFFERENCE, a function that takes two list-sets
;; s1 and s2 and returns a list-set containing all the elements of s1
;; that are not elements of s2. 


;; 9. Define PALINDROME-ME, a function that takes a true-list and
;; returning the result of appending the input to the reverse of the
;; input. That returns a palindromed version of the original input.
;; You should feel free to use append and reverse functions in your
;; answer to this problem.

(check= (palindrome-me '(a b c d e)) '(a b c d e e d c b a))


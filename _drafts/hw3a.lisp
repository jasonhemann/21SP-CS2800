#|

Homework 3a.

 Make sure you are in ACL2s mode. 

 Note that you can only change the mode when the session is not
 running, so set the correct mode before starting the session.

 Please try and get started on this before your lab section.

|#

(set-defunc-function-contract-strictp nil)
(set-defunc-body-contracts-strictp nil)

;; Part 1: Reviewing Function Definitions 

; Fill in the blanks below. This mostly involves adding check= and
; test? forms and a few definitions

; Make sure that the check='s and test?'s below pass. Provide
; sufficient check='s and test?'s of your own.

; 1. subst-in-list: All x All x TL -> TL
; (subst-in-list: old new l) replaces every element of l
; which is equal to old by new. 


(check= (subst-in-list '(2 1) '(1 2) '(1 2 (1 2) (2 1) ((2 1))))
        '(1 2 (1 2) (1 2) ((2 1))))

(check= (subst-in-list 'x 'd '(c o d e x)) '(c o d e d))
(check= (subst-in-list 'a 'b '()) '())
(test?  (equal (subst-in-list b c (subst-in-list a b x))
           (subst-in-list b c (subst-in-list (a c x)))))
(test? (equal (subst-in-list b a (subst-in-list a b x))
          (subst-in-list b a x)))

(defdata lor (listof rational))

; 2. orderedp: lor -> Boolean
; Returns t if the input is ordered with respect to <=, ie,
; is non-decreasing and is nil otherwise.

(definec orderedp (l :lor) :bool
  (or (endp (cdr l))
      (and (<= (first l) (second l))
           (orderedp (rest l)))))

; Make sure you understand why the above definition works, e.g.,
; consider what happens when l is empty and when it has 1 element. In
; this case, why does contract checking work? Complete the following
; check=s, ofc add your own. 

(check= (orderedp '())   )
(check= (orderedp '(44)) )
(check= (orderedp '(1 1 3/2 8/5)) )
(check= (orderedp '(1 3/2 26/20)) )
(test? (implies (and (ne-tlp x) (lorp x))
        (implies (orderedp x)
             (orderedp (cons (1- (car x)) x)))))
(test? (implies (and (ne-tlp x) (lorp x))
        (implies (orderedp x)
             (orderedp (cdr x)))))

; 3. my-remove: All x TL  -> TL
; (my-remove e l) will recur (but not deeply) down l removing every
; occurrence of e. cf. Lisp's REMOVE






;; Part 2: Properties and Conjectures

#| 

For each of the following conjectures (hypotheses):

Next to the (defconst ...) for the conjecture you should write "t"
or "nil" (not, btw, in the double quotation marks, that is to
distinguish [use from
mention](https://en.wikipedia.org/wiki/Use-mention). 

Then, in the definition of the test-or-counterexample form, 

If you marked "t": you should write a (test? ...)  with a
formalization of that property in ACL2. There are often many
propositionally-equivalent ways to express these properties; any will
be accepted. If in doubt, it is good practice to hew closer to form of
the property expressed by the English.

If you marked "nil": you should write a let binding giving values to
each of the variables; the body of that let should be a formalization
of the property in question, and the let expression should evaluate to
nil. Hint: ACL2s counterxexamples can be very helpful here. 

|# 

; 4. Conjecture: If a list is ordered, then deleting from it yields an ordered list
(defconst *conjecture-4* )
(defconst *conjecture-4-test-or-counterexample* )


; 5. Conjecture: If we delete a rational from an lor, the length decreases.
(defconst *conjecture-5* )
(defconst *conjecture-5-test-or-counterexample* )


; 6. Conjecture: The order of deletions does not matter.
(defconst *conjecture-6* )
(defconst *conjecture-6-test-or-counterexample* )


; 7. Conjecture: my-remove does not increase the length of the input list.
(defconst *conjecture-7* )
(defconst *conjecture-7-test-or-counterexample* )


; 8. Conjecture: subst-in-list does not change the length of l 
(defconst *conjecture-8* )
(defconst *conjecture-8-test-or-counterexample* )


; 9. Conjecture: if old=new then subst-in-list returns l
(defconst *conjecture-9* )
(defconst *conjecture-9-test-or-counterexample* )


; 10. Conjecture: if l is an lor, then (subst-in-list old new l) is an lor. 
(defconst *conjecture-10* )
(defconst *conjecture-10-test-or-counterexample* )

; 11. Conjecture: if l is an lor and new is a rational, then
; (subst-in-list old new l) is an lor. 
(defconst *conjecture-11* )
(defconst *conjecture-11-test-or-counterexample* )


;; Part 3: Reverse Polish Notation
#|

What do you know about [RPN and RPN
calculators](https://hansklav.home.xs4all.nl/rpn/)? Soon you will know
lots!

Back in the day, because computers' had such limited memory HP and
other companies produced calculators in which you would input the
operators not infix, not [prefix like god
intended](https://imgs.xkcd.com/comics/lisp.jpg), but postfix. This
was also known as reverse-Polish notation (RPN), where the Polish
refers to the nationality of [Jan
Łukasiewicz](https://en.wikipedia.org/wiki/Jan_%C5%81ukasiewicz), a
famous logician. Such calculators were in fairly common use because
pound-for-pound, bit-for-bit they really *were* more powerful. Three
tangential points: 

  1. [companies still today produce such
  calculators](https://web.archive.org/web/20150316030830/https://www.wsj.com/articles/SB10001424052748703841904576257440326458056),

  2. [some people are real devotees](https://www.hpmuseum.org/), and 
  
  3. [having to change fundamental facets of your field to work around
  the vagaries of
  computers](https://www.vice.com/en_us/article/z3eg54/scientists-rename-human-genes-so-microsoft-excel-spreadsheets-dont-get-confused)
  sure seems antequated and silly, right?

We will use ACL2s to define the syntax and semantics of two simple
languages: RPNEL (Reverse Polish Notation Expression Language) and SRPNEL
(Simple Reverse Polish Notation Expression Language). 

We will use a formal language: ACL2s! In using ACL2s, we will have to
be explicit about everything, as the use of ACL2s will force us to
provide formal, unambiguous, executable definitions of languages. For
this price, however, we get the assurances that precise formal
unambiguous definitions offer.

In lab, we will focus on SRPNEL.  You will define RPNEL in HW3.  Make sure
you fully understand this lab as it is intended to prepare you for
HW3: RPNEL is an extension of SRPNEL.

Notice that, because we are formalizing (S)RPNEL in ACL2s and all we
have access to in ACL2s are the elements of the ACL2s
universe, (S)RPNEL expressions *must* be elements of the ACL2s
universe.

|#

#|

A binary operation, boper, is one of +, -, or * 

|#

;; 12. Use defdata to define boper the binary operators. This really
;; means that you should define the symbol names of the SEAL binary
;; operators, which are shown above.



(check= (boperp '*) t)
(check= (boperp '^) nil)

#|

A simplified reverse polish notation expression, srpnexpr, is one of
the following:

- a rational number (we use the builtin type rational)

- a list of the form 
  
  (<srpnexpr> -)

  where <srpnexpr> is a simplified arithmetic expression

- a list of the form

  (<srpnexpr> <srpnexpr> <boper>)

  where <boper> is a binary operation and both <srpnexpr>s are
  simplified arithmetic expressions.

|#

;; 13. Use defdata to define srpnexpr.



#|

 We have now defined SRPNEL expressions in ACL2s. That is, we have
 defined the syntax of a language. In fact, the use of defdata made
 this pretty easy and gave us a recognizer, srpnexprp, for SRPNEL
 expressions.

|# 

(check= (srpnexprp '45/3) t)
(check= (srpnexprp '((1 2 +) (3 -) -)) t)
(check= (srpnexprp '(1 2 3 +)) nil)
(check= (srpnexprp '(1 2 + 3 +)) nil)

;; 14. To what should the following check= forms evaluate? Make sure you
;; understand what argument sexprp is given as input for each form.

(check= (srpnexprp 12) )
(check= (srpnexprp '12) )
(check= (srpnexprp ''12) )
(check= (srpnexprp '(45 -)) )
(check= (srpnexprp ''(45 -)) )
(check= (srpnexprp (+ 1/2 45)) )
(check= (srpnexprp '(+ 1/2 45)) )
(check= (srpnexprp '(1/2 45 +)) )
(check= (srpnexprp ''(1/2 45 +)) )


#| 

 SRPNEL expressions are not a subset of ACL2s expressions.

|#

;; 15. Provide an example of a SRPNEL expression that is not an ACL2s
;; expression.



#|

 We will now define the semantics of simplified arithmetic expressions
 in the formal language of ACL2s. Now that we have ACL2s, we can be
 much more precise and unambiguous with out definitions. In fact, the
 definitions we come up with will be subject to formal analysis and
 verification in ACL2s.

|#

#|

 Recall the design recipe you've learned. Data definitions give rise
 to a template that you can use to define functions operating on
 objects of that datatype. See Section 2.13 of the Manolios
 textbook. Use this template to define functions such as srpneval.

|#

;; 16. Define srpneval, a function that evaluates SRPNEL
;; expressions. Notice that the evaluation of a SRPNEL expression
;; always produces a rational number. Use case-match in your
;; definition.

              

(check= (srpneval 1) 1)
(check= (srpneval '((1 2 +) (3 -) -)) 6)
(check= (srpneval '((6 -) (-6 -) *)) -36)
(check= (srpneval (cons 6 '(2 +))) 8)

;; Part 4: Claims About Form and Meaning

#|

 Mentally, travel back in time to grade school. Specifically, to your
 favorite class, algebra.

 The teacher writes on the board:

 x+y = y+x

 What does this mean? Something like: for all numbers x and y, we have
 that x+y = y+x. (otherwise written: ∀xy.x+y = y+x)

 They clearly do not mean that the expressions "x+y" and "y+x" are
 equal; they aren't equal sequences of glyphs. Likewise "2+2" contains
 3 ascii characters whereas "4" contains 1 ascii character. We now
 understand the distinction between syntax and semantics. For example,
 the (grade-school arithmetic) expression "x+y" is syntax. Whereas the
 *meaning* of x+y, the semantics, comes from the meanings of the
 numerals and the meaning of (the definition of) addition. 

 Your teacher did not fib to you, per se. What your teacher *intended*
 to say is that, for all numbers x and y, that *the meaning of* the
 expression "x+y" is the same as *the meaning of* the
 expression "y+x".
 
 In our business, most of the things we write down—things like
 programs—are syntax. It would be odd to have to quote almost
 everything. So instead by default, when we write something down we
 are speaking about the syntax, and we use some extra notation when we
 want to refer to the meaning of an expression.  

 This is the [[ ... ]] notation (called "semantic brackets").

 We are going to, for SRPNEL and using ACL2s, explore how to formally
 make statements like those your grade school teacher made
 informally. 

 We can reason about both the syntax and semantics of your grade
 school math. Consider formalizing the following statement:

 For all rationals x,y  (x + y) = (y + x) 

 Notice that the following test? forms are ACL2s statements.

 The (false!) claim that the expressions themselves are equal—the
 claim about syntax—can be formalized as:

 (test? (equal `(,x + ,y) `(,y + ,x)))

 The (true!) claim that the expressions are semantically equal—that
 they have the same meaning in SRPNEL—can be formalized as:

 (test? (implies (and (rationalp x)
                      (rationalp y))
                 (= (grade-school-eval `(,x + ,y))
                    (grade-school-eval `(,y + ,x)))))

 Again, the above is a claim about semantics—about meaning.  It starts
 by saying that for any rationals, x and y (these are rationals in
 ACL2s as well as in grade school). We continue. For any rationals, x
 and y we have (after expanding the quasiquote/unquote syntax to the
 more familiar list notation):

 (= (grade-school-eval (list x '+ y)) (grade-school-eval (list y '+ x)))

 This is an equality—numeric equality in ACL2s—between the grade
 school meaning, of the expressions (list x '+ y) and (list y '+ x)

 The above claim can be thought of as an infinite number of claims,
 with one claim per pairs of rational numbers:

 (= (srpneval '(0 + 1)) (srpneval '(1 + 0)))
 (= (srpneval '(1/2 + 1)) (srpneval '(1 + 1/2)))
 ... 

 Notice how we used the ACL2s language to express a statement about
 grade-school expressions. The x, y ACL2s symbols are used as
 grade-school variables!

 You may have initially tried formalizing the above claim as follows.

 (test? (implies (and (rationalp x)
                      (rationalp y))
                 (= (grade-school-eval '(x + y))
                    (grade-school-eval '(y + x)))))

 This is not an appropriate formalization, and fails. The above is a
 statement about the symbols x and y, not arbitrary rationals! And,
 since SRPNEL does not have symbols, the above does not even pass
 contract checking. Remember that if you write a property that does
 not pass contract checking, then any test? or thm forms are
 meaningless. To see that contract checking fails, use the unifying
 observation to generate a definec form.

 (definec foo (x :rational y :rational) :bool
   (= (grade-school-eval '(x + y)) 
      (grade-school-eval '(y + x))))

 The above shows that x and y are not even used in the body! Make sure
 you understand this. If we remove them as arguments, notice the
 contract violations we get by trying to admit the following
 definition.

 (definec foo () :bool
   (= (grade-school-eval '(x + y)) 
      (grade-school-eval '(y + x))))

|#

;; Formalize the claims of this section. For each claim, answer
;; whether the claim is true. For the true claims, write a (test?
;; ...) expression that will test the property. For the false claims,
;; write a (let ...) binding that demonstrates the claim to be false.
;; Like the above similar exercise.


;; Hint: the quasiquote/unquote combo, while not necessary, will make
;; your life way easier.

;; Hint 2: In your mind, be terminologically careful here. For
;; instance in the following, `(,x * ,y) is an ACL2s expressions that,
;; for given rationals x and y, evaluates to an SRPNEL expression. Why
;; do we evaluate it in ACL2s first? We have implemented our srpneval
;; and SRPNEL language in ACL2s, and that's how ACL2s
;; works. Technically speaking, ACL2s is a [strict programming
;; language](https://en.wikipedia.org/wiki/Strict_programming_language).

;; 17. (x y +) = (x (y -) -), in SRPNEL, for rationals x, y
(defconst *conjecture-17* )
(defconst *conjecture-17-test-or-counterexample* )


;; 18. (x y -) = (x (y -) +), in SRPNEL, for srpnexpr's x, y
(defconst *conjecture-18* )
(defconst *conjecture-18-test-or-counterexample* )


;; 19. (x (y z *) +) = ((x y +) (x z +) *), in SRPNEL, for srpnexpr's x, y, z
(defconst *conjecture-19* )
(defconst *conjecture-19-test-or-counterexample* )

           

;; 20. (x (y z *) +) = ((x y +) (x z +) *), in SRPNEL, for rationals x, y, z
(defconst *conjecture-20* )
(defconst *conjecture-20-test-or-counterexample* )


;; 21. (x (y z +) *) = ((x y *) (x z *) +), in SRPNEL, for srpnexpr's x, y, z
(defconst *conjecture-21* )
(defconst *conjecture-21-test-or-counterexample* )

                   

;; 22. (x (y z +) *) = ((x y *) (x z *) +), in SRPNEL, for rationals x, y, z
(defconst *conjecture-22* )
(defconst *conjecture-22-test-or-counterexample* )


#| 

In our language, negation is
[involutive](https://en.wikipedia.org/wiki/Involution_(mathematics)).  (If
you don't know what "involutive" means, I trust you to be able to read
about it and find out, then express it as a theorem. There is a lot of
fancy or arcane vocabulary, and eventually you just have to dive in.)

|# 

;; 23. Express as a theorem that in our language, negation is involutive.

(defthm negation-is-involutive )


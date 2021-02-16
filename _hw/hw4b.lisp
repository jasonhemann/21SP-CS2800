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
  - These commands are simplifying your interactions with ACL2s

  - Do not remove them.

  - To learn more about what they do, see Ch2 found on the course
	readings page
|#

(set-defunc-termination-strictp nil)
(set-defunc-function-contract-strictp nil)
(set-defunc-body-contracts-strictp nil)

;; This directive forces ACL2s to generate contract theorems that
;; correspond to what we describe in the lecture notes.
(set-defunc-generalize-contract-thm nil)

#| 

This part of your hw picks up where the last part of your hw left
off. In hw4a, we used ACL2s to define the syntax and semantics of
SRPNEL (Simple Reverse Polish Notation Expression Language). In this
homework, we will use ACL2s to define the syntax and semantics of two
RPNELs (Reverse Polish Notation Expression Languages), so make sure
you have done hw4a already. 

|# 

#| 

Part I Defining a Syntax

|# 

#| 

 A (general) reverse-polish notation expression---an rpnexpr---extends
 srpnexpr with variables. It is one of the following:

- a rational number (we use the builtin type rational)

- a variable (we use the builtin type var)

- a list of the form 
    (<rpnexpr> -)
  where <rpnexpr> is an arithmetic expression

- a list of the form
    (<rpnexpr> <rpnexpr> <boper>)
  where <boper> is one of +, -, or * (the same as SRPNEL)
  and both <rpnexpr>'s are arithmetic expressions.

|#

;; Vars are just a restricted set of the symbols. 
;; The specifics of which symbols are vars is not all that important
;; for our purposes. All we need to know is that vars are symbols and
;; they do not include any of the RPNEL operators. Examples of vars include
;; symbols such as x, y, z, etc.

(check= (varp '-) nil)
(check= (varp '+) nil)
(check= (varp '*) nil)
(check= (varp '/) nil)
(check= (varp 'x) t)
(check= (varp 'x1) t)
(check= (varp 'y) t)

;; The defdata-subtype-strict form, below, is used to check that one
;; type is a subtype of another type. This is a proof of the claim that
;; var is a subtype of symbol.

(defdata-subtype-strict var symbol)

;; To see that symbol is not a subtype of var, we can use test? to
;; obtain a counterexample to the claim. The must-fail form succeeds
;; iff the test? form fails, i.e., it finds a counterexample.

(must-fail (test? (implies (symbolp x)
			   (varp x))))

;; We use defdata to define boper the binary operators (same as
;; earlier in your hw):

(defdata boper (enum '(+ - *)))

(check= (boperp '*) t)
(check= (boperp '^) nil)

;; 1. Use defdata to define rpnexpr with an inductive data
;; definition. That is, rpnexpr should appear inside of its own data
;; definition.

;; As an example, the following true-list data definition is
;; inductive. The data structure should be self-referential.

(defdata true-list (oneof (cons all true-list) nil)))

(check= (rpnexprp '45/3) t)
(check= (rpnexprp '((x y +) (z -) -)) t)
(check= (rpnexprp '(x y z +)) nil)
(check= (rpnexprp '(x + y + z)) nil)

;; 2. What should the following check= forms evaluate to? Make sure
;; you understand each argument that rpnexprp gets for input. 

(check= (rpnexprp  12) )
(check= (rpnexprp '12) )
(check= (rpnexprp ''12) )

(check= (rpnexprp  (- 45))  )
(check= (rpnexprp '(45 -))  )
(check= (rpnexprp ''(45 -)) )

(check= (rpnexprp  (+ 1/2 45)) )
(check= (rpnexprp '(+ 1/2 45)) )
(check= (rpnexprp ''(1/2 45 +)) )

(check= (rpnexprp  (expt 2 3)) )
(check= (rpnexprp '(expt 2 3)) )
(check= (rpnexprp '(2 3 expt)) )
(check= (rpnexprp '(2 expt 3)) )
(check= (rpnexprp ''(expt 2 3)) )

(check= (rpnexprp (car (cons 1 "hi there"))) )
(check= (rpnexprp `(,(car (cons 1 "hi there")) 12 +)) )
(check= (rpnexprp '(+ (car (cons 1 "hi there")) 12)) )
(check= (rpnexprp '((car (cons 1 "hi there")) + 12)) )
(check= (rpnexprp `(,(car (cons 1 "hi there")) + 12)) )

#|

 We have now defined RPNEL expressions in ACL2s. In fact, the use of
 defdata made this pretty easy and gave us a recognizer, rpnexprp, for
 RPNEL expressions. The RPNEL expressions are /still/ not a subset of
 ACL2s expressions. I am not going to ask you to provide an example of
 an RPNEL expression that is not a legal ACL2s expression.

|#

;; 3. Why am I not going to bother asking you to provide an example?
 

#| 

Part II Semantics

|# 

#| 

Next, we will define the semantics of reverse-polish notation
expressions. The main complication here is extending the earlier
language to deal with vars. The idea is that to evaluate a var, we
have to know what value it has. We will use an /environment/ to track
and hold the values of variables.

We will represent an environment as an alist from vars to
rationals. When looking up a variable in an alist, we take the
first binding we find with x as the car, as we scan the list from
left to right. For a variable x and an environment ρ (for
"enviρnment"), we say that the value of x in ρ is v (otherwise
written ρ(x) = v) if (x . v) is the first binding that begins with
x.

|#

(defdata env (alistof var rational))

(check= (envp '((x . 1) (y . 1/2))) t)

;; This is nil because (1) and (1/2) are lists, not rationals.
(check= (envp '((x 1) (y 1/2))) nil)

;; 4. Define lookup, a function that given a variable x and an
;; environment ρ, returns the value of x in ρ. Use case-match in your
;; definition.




#| 

If we look up a var that is not the left-hand side of a binding in
the environment, then we will, by default right now, say that
variable has the value 1. This is a bit like saying that all
variables are pre-initialized to 1.

Remember to use the "template" that defdatas give rise to as per
Section 2.13 of the lecture notes. 

|# 



(check= (lookup 'x '((x . 0) (y . 2))) 0)
(check= (lookup 'y '((x . 0) (y . 2))) 2)
(check= (lookup 'z '((x . 0) (y . 2))) 1)

;; 5. Define rpneval, a function that given an rpnexpr and an
;; environment evaluates the expression, using the environment to
;; determine the values of vars appearing in the expression. Use a
;; case-match in your definition. This should be similar enough to
;; what you did earlier in your hw that you /could/ consider copying
;; some of that in and editing it. Remember to use the "template" that
;; defdatas give rise to as per Section 2.13 of the Manolios
;; textbook. We provide an example usage below.








(check= (rpneval '((x y +) (z -) -) '((y . 3/2) (z . 1/2)))
        3)

#|
 
 Let us unpack the above check= form.

 A: The first argument to rpneval is '((x y +) (z -) -), which is an
 ACL2s expression.

 B: That evaluates to ((x y +) (z -) -), which is not an ACL2s
 expression, but *is* an RPNEL expression; this is what rpneval gets
 as input.

 The function rpneval given us the meaning of this latter
 expression. In fact, the meaning of this latter expression depends on
 the meanings of x, y and z, which are provided by the environment. In
 this instance, the meanings are: 1, 3/2 and 1/2, respectively.
 
|#


                    
#|

 Congratulations! You have formally defined the syntax and semantics
 of RPNEL. The data definition for rpnexpr defines the syntax: it tells
 us what objects in the ACL2s universe are legal rpnexps.
 
|#


#| 

Part III Specifying properties of this language w/ rpneval.

|# 

#| 
For each of the problems in this section:

In the first (defconst ...) form, write a quoted acl2 property that is
your translation of that question's English-language conjecture. Make
sure and quote this property---that is you are definining a quoted
list that *looks* like a property, and that absent the quote, we could
run it in (test? ...) as a property. Remember to include all necessary
preconditions. There are often many propositionally-equivalent ways to
express these properties; any will be accepted. If in doubt, it is
good practice to hew closer to form of the property expressed by the
English.

Then, decide if that property is *valid* in the acl2s universe. 

If you think the property is valid, then define the
t-or-counterexample form to be "t" (not, btw, in the double quotation
marks, that is to distinguish [use from
mention](https://en.wikipedia.org/wiki/Use-mention).

If on the other hand you think the property is falsifiable, then write
a quoted list representing a counter-example. This counter-example
will look like the bindings of a let form that would give admissible
values to each of the variables in your property, and for which your
property will not hold.

|# 

;; 6. A = ((A -) -), in RPNEL, for any rational A.
(defconst *conjecture-6* )
(defconst *conjecture-6-t-or-counterexample* )

;; 7. (A B -) = (A (B -) +), in RPNEL, for any rationals A and B.
(defconst *conjecture-7* )
(defconst *conjecture-7-t-or-counterexample* )

;; 8. (A (B C +) *) = ((A B *) (A C *) +), in RPNEL, for any rationals A, B & C.
(defconst *conjecture-8* )
(defconst *conjecture-8-t-or-counterexample* )

;; 9. (E1 E2 -) = (E1 (E2 -) +), in RPNEL, for any rpnexprs E1 and E2.
(defconst *conjecture-9* )
(defconst *conjecture-9-t-or-counterexample* )

;; 10. (E1 (E2 E3 +) *) = ((E1 E2 *) (E1 E3 *) +), in RPNEL,
;;    for any rpnexpr's E1, E2, E3.
(defconst *conjecture-10* )
(defconst *conjecture-10-t-or-counterexample* )

;;; RPNPRGM Langauge 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#| 

Part VI : Another Language

|# 

#|

I believe I heard someone mutter ".. I'm tired of all these
parentheses!" Rejoice! We can fix that! Unlike prefix notation or
infix notation, this post-fix notation behaves in a stack-like
discipline. This means that by using an ancillary stack sort
of "under-the-hood", we can process programs in these languages
without (m)any parentheses. Because for well-formed programs you can
sort of "squint at" the program and put the parentheses back where
they should be. But with our less rigorous syntax, it's now harder to
syntactically distinguish good programs from bad.

|# 

;; 11. Why is it now harder to syntactically distinguish good programs
;; from bad?

#|

We have grouped the operators variables and rationals together as
just "data". [Unary](https://en.wikipedia.org/wiki/Unary_function)
negation is now written "0-".

|# 

(defdata data (oneof (enum '(+ - * 0-)) rational)) ;; var

;; 12. Why is unary "-" (negation) now written "0-"?



;; We use defdata to define rpnpgm with an inductive data definition:
(defdata rpnprgm (listof data))

(check= (rpnprgmp '(2 3 +)) t)
(check= (rpnprgmp '(2 3 2 + -)) t)
(check= (rpnprgmp '(2 3 2 + -)) t)

(check= (rpnprgmp '(2 3 4 5 6)) t)

;; !?

;; This is believe it or not okay. We can have extra numbers. This is
;; just the result of a partially completed calculation. If this were
;; a calculator, you could imagine someone adding the next couple of
;; characters at the base of the program, so that the full program is
;; (2 3 4 5 6 - + - -).

;; The answer in that case is 2.

;; Why?

;; put 2 on the stack
;; put 3 on the stack
;; put 4 on the stack
;; put 5 on the stack
;; put 6 on the stack
;; - ... subtraction!
;; pop the most recent number from the stack, and start filling in operations
;; (- ... 6)
;; pop the most recent number from the stack, and fill the next one (you'd keep going in principle, until you had all n arguments for an n-ary operator.)
;; (- 5 6)
;; When the operation is "saturated", you can evaluate it, and place the value on the stack. 
;; look at the next operator, +, and do the same kind of thing
;; (+ 4 -1)
;; (- 3 3)
;; (- 2 0)
;; 2

;; Et voila!


;; For the same reason, (2 3 + 2 + 9 *) gets you 63

;; You can try [the following
;; tool](https://abakbot.com/en/net-en/rpn-en) for more examples


;; But! A new issue has arisen!

(check= (rpnprgmp '(+ + + + +)) t)

#|

It was okay when we had values left over on the stack. But we now also
permit programs for which there aren't *enough* values.

We handle this by introducing a special error value.

We'll now say that an RPNPRGM expression can evaluate to a rational
(if everything goes well), or to the error value (if an error
occurs anywhere in the evaluation of the expression).

|#


(defdata er 'error)
(defdata rat-or-err (oneof rational er))


;; 13. See following the check='s to see examples the idea; add a few
;; of your own.

(check= (rat-or-errp 5)      t)
(check= (rat-or-errp 'error) t)
(check= (rat-or-errp "five") nil)

(check= (rat-or-errp  (+ 2 3))   t)
(check= (rat-or-errp '(+ 2 3)) nil)
(check= (rat-or-errp '(2 + 3)) nil)
(check= (rat-or-errp '(2 3 +)) nil)


;; 14. Complete the impementation of rpnprgmeval-help. You can omit
;; variables in your implementation, or not. If you add them, remember
;; to reintroduce them into your data definition.

(definec rpnprgmeval-help (pgm :rpnprgm stk :???) :rat-or-err
  (case-match pgm
    ('()
     (case-match stk
       ((s1 . &) s1) ;; & serves as a ["wildcard" value](https://en.wikipedia.org/wiki/Wildcard_character).
       (& 'error)))
      


    

    
    ((n . res) (rpnprgmeval-help res `(,n . ,stk)))))

(definec rpnprgmeval (pgm :rpnprgm) :rat-or-err
  (rpnprgmeval-help pgm '()))

(check= (rpnprgmeval '(*)) 'error)
(check= (rpnprgmeval '(3 5 * * 2 3 + - *)) 'error)
(check= (rpnprgmeval '(2 3 4 5 6 - + - -)) 2)
(check= (rpnprgmeval '(3 5 2 3 + - *)) 0)

#| 

Now, you should trace the execution of a reasonably large program in
the language of your interpreter (i.e. the `srpneval` evaluator) from
hw4a. (Your program is "reasonably large" if it's sufficient for you
to notice the important diffenece we are looking to point out to
you. (If you want, you can probably think of a way to generate such
programs.))

Then, for that same program, convert any unary "-"s into the
corresponding form for this language, remove the inner parentheses,
and then trace the run of that program in rpnprgmeval.

|#

;; 15. What important difference do you see? Speculate as to why this
;; is.



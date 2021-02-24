---
title: Definition and Termination
date: 2021-02-22
---

# Let's check on our reading! How's this going?

`PollEverywhere`

# Exam Prep / Practice Exam / HW6a. 

## Discussion of test taking strategy, format, and co.

### Time: From Sat @ 1pm to Sun @ 10pm.
    - Enough time so that I can close the 6a Dropbox @ the usual time
    - A usual-enough time for people to be submitting things
    - B/c close 6a before opening Exam Dropbox, no one will upload to the wrong place.
    - People will have 33 hours to complete and upload
      -- (24 < 33 < 36) hours
      -- Enough to give all time zones a fair shake.

### Individual v. Group

>  You should feel free to ask your homework groups for clarification
>  on how to understand the questions, or the techniques through which
>  to work them. Of course, you aren't limited to learning from your
>  homework group, but its good to have these established
>  relationships. You should not receive the solutions from your
>  homework group, or others. But if this is a forcing function
>  through which to teach each other this material, then that's
>  great. Don't work /these/ problems with one another, but you may
>  instead work analogous problems together.

    - Came down to splitting the difference a bit
    - Based on desired learning outcomes
      - Do your own solutions, solve each problem yourself
      - Don't compare/correct each others' solutions
      - Don't collaborate on answering the questions on the exam
      - Do explain background concepts to one another
      - Do teach someone else the general principle behind solving such questions
      - Do work through example problems together (either existing or ones you come up w/)

#### "... if this is a forcing function for learning ..." ¯\_(ツ)_/¯

Basically, the goal is to induce learning and assess how it's
going. This is useful not just for metrics, but these are also
learning opportunities themselves. If it ends up being that during the
exam itself student A /teaches/ student B the material to know how to
solve a problem so that student B can then solve it, then that feels
more "mission accomplished" than "nefarious activity".

If you have concerns or questions about this wording, general overall
principles, etc, please do reach out. We have time and ability to
correct/improve/clarify wording. 

## How does this fit in w/upcoming HW?

  - The class and learning is going forward
  - I /do/ understand that you all are doing exam prep/practice
  - I have set aside lab on Friday for exam practice/prep
  - Many of you will not have used gradescope for exam submission before
  - I want you to also practice uploading exam
  - I have made successfully uploading your practice exam *the* 6a hw.
  - We still have something about the following material as a 6b, b/c I don't want to stand still.
  - We will grade this upload based on completion, not on your answers/correctness.
  - 6b still due at the usual place and time. 

# Definition Principle

We already understood this. Right? We had for every accepted
definition the axioms that `ic => oc` and `ic => (equal (f x) = body)`

```lisp
(definec f (a :dt1) :dt2
  :ic exp1
  :oc exp2
  (cons a a))
```lisp

## BTW: order matters in contracts b/c and, etc are "short-circuiting"

```lisp
(defunc f (a)
  :ic (and (dt1p a) exp1)
  :oc (and exp2 (dt2p (f a)))

  ...)
```

## The general defunc
 `< .. >` are metavariable names

But we know and we've seen that `definec` is a shallow wrapper over
`defunc`, and since not every predicate has a corresponding datatype,
we could go great with that. 

```lisp
(defunc <name (<args ...>)
  :ic <in-contract-expr>
  :oc <out-contract-exrp>
  <body>)
```  

###  Contract Theorem
`(implies <in-contract-expr> <out-contract-expr)`

### Definition Theorem
`(implies <in-contract-expr> (equal (f <inputs>) body[<inputs>/<args>]))`

### BTW: One more additional piece of syntax to introduce.

`[<inputs>/<args>]` is syntax that people use to discuss substitution.
  
  1. This is substitution for free variables (the complicated substitution notion)
  2. This is a simultaneous substitution

We're substituting the inputs to the function (AKA actual parameters)
for the formal parameters (the variable names in the function definition)
in the body.

```lisp
;; actual parameters
(f 12) 
(f 'cat)
(f 17)
```

```lisp
<body>[17/a]
(cons a a)[17/a] ;; stick in 17 for every (free) a in that expression
```

## Because ...

Computer scientists and mathematicians are nerds; thus multiply by
fractions "substitution" analogy.

`5*[3/5] = 3`

Our substitutions are by analogy w/let bindings. `((a 17))`

## There are many complications 

## We must not derive `nil`

### Function bodies that return `nil` allow `nil`.

`(definec foo (x :all) :all (equal 0 1))`

### (Some) non-terminating functions let us derive `nil`

`(definec foo (x :all) :all (1- (f x)))`

(cf. `(definec foo (x :all) :all (f (1- x)))`)

### Non-terminating functions let us derive `nil`.

`(definec foo (x :all) :all y)`

### Functions with free variable occurrences let us derive `nil`. 

`(definec foo (x :all) :all y)`	

## Admissible definitions

So we need a bouncer at the door. ACL2 doesn't let us introduce
functions willy-nilly. ACL2 saves us from accidentally allowing
non-termination.

### No decision procedures for termination!

  - Almost all programs you write, you *want* to terminate
  - Non-terminating programs are almost always a bug
  
  So we would expect something to check that for us. 
  
  But you can't write an algorithm for this. 
  
  (I'll show you that later. Really exciting result) 
  
### A definition is /admissible/ when

   1. `f` is a new function symbol (so that we aren't possibly adding
      an axiom about something that already exists) 
	  You can't add a new, 2nd meaning to `cdr`.
   2. The variables `x_i` in the definition of the body are unique (else, what happens when we subst in?)
   3. `body` is a term, possibly using `f` recursively as a function symbol, with no free variable occurrences other than the `x_i` 
      The word "term", here means that it is a legal expression in the current history.
   4. This means that if you evaluate the function on any inputs that
   satisfy the input contract, the function will terminate. A *total*,
   computable function. Terminates on (acceptable) input.
  
  5. `ic ⇒ oc` is a theorem. (note this is just describing the *contracts*.
  
  6. If the `ic` holds, all the body contracts hold too.

### We have seen what we get as a result

   - /Contract Theorem/ for `f`: `ic => oc`
   - /Definitional Axiom/ for `f`: `ic => (f x1 .. xn) = body`

## But! How to prove termination?

  We *can't* prove termination with a general decision procedure. So
  it sounds like we are in trouble, and we cannot define
  functions. Yet, we have done so. What shall we do? 
  

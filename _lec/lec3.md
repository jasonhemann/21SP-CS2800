---
title: Starting ACL2 in Earnest
date: 2021-01-25
---

## PollEverywhere Q

How many dots would we see in the "maximal dot" representation of the
following expression:

`(a (b c d) . e)`


# What questions do we have? 

  - Question from the reading?
  - Question from the HW? 
  - Questions from lecture, things that don't make sense now?
  - Question from here?
  - Question from the audience at home?

## Tests

I *have* just been writing programs at the REPL. As we know that's a
*terrible* testing strategy!

### `check=` tests

### property-based testing 

`(test? (equal (app (list x y) (list)) (list x y)))`

`(test? (implies (natp n) (equal (even-integerp n) (even-natp n))))`

## More bits of syntax

### PollEverywhere `atom` question. 

#### The recognizer `atom`, which is not like the others

### `let`, `let*` 

### Simultaneous vs structured

## Datatypes

# Defining datatypes 

## `defdata`

### Union types 

`(defdata fullname (list string string))`

Union types let us take the union of existing types. E.g.,

`(defdata intstr (oneof integer string))`

## Recursive type expressions 

Recursive type expressions involve the `oneof` combinator (noun: a
thing that combines. Like "regulators" regulate) and product
combinations, where additionally there is a (potentially-recursive)
reference to the type being defined.

`(defdata loi (listof integer))`

This defines the type consisting of lists of integers.

For example,here is another way of defining a list of integers.

`(defdata loi (oneof nil (cons integer loi)))`

# All functions must terminate! (You gotta make'em!)

`countdown`, fails to terminate. Contracts!

### constructing `enum` data 

#### range data

`(defdata probability (range rational (0 <= _ <= 1)))`

`(defdata big-nat (range integer ((expt 2 64) < _))`

## What about more complicated properties? 

### `defunc` functions

`definec` short-hand version of `defunc`

More powerful, because these permit us to define arbitrary contracts.

contracts: A simple and useful class of invariants about inputs and outputs

```lisp
:input-contract ...
:output-contract ...
```

## NEW! in ACL2s / this course

In Fundies 1 these were specified as comments

Here: integrated as part of the language => can be checked statically by the compiler!

## Invariants 

   An instantaneous property that is always satisfied in all
   executions of the program, at a certain location in the program

   ```c
   k := 0 ; // assign 0 to k
   // k=0 is an invariant here

   // say “I love you” ten times:
   while (k < 10) {
	 // k<10 is an invariant here
	 // 0<=k<10 is another (stronger) invariant
	 printf(“I love you\n”) ;
	 k++ ;
	 // k<=10 is invariant here
	 assert(k<=10);  // assertion statement
   }
	```

	```lisp
	(definec len (l :tl) :nat
	  (if (endp {(tlp l)} l) 
		0
		(+ 1 (len (rest l)))))
	```

## Natural Recursion


```lisp
(defun plus (x :nat y :nat) :nat
  (cond
    ((zp y) x)
	(t (1+ (plus x (1- y))))))
```lisp

## Contract Checking

This will be a topic of the whole course---we will revisit this. This
is a prelude.

```lisp
(definec tapp (x :tl y :tl) :tl
  (declare (xargs :mode :program))
  (if (lendp x)
    y
	(lcons (head x) (tapp (tail x) y))))
```

1. evaluating `f`'s input contract on any (well-formed) inputs whatsover
   will not lead to any contract violations, and

2. evaluating the body of f on any inputs that satisfy `f`'s input
   contract will never leadto a contract violation for any function
   that may be called during thisevaluation,including functions that
   are called directly or indirectly, and

3. the evaluation of `f`'s body on any inputs that satisfy `f`'s input
   contract will terminate,and

4. the evaluation of `f`'s body on any inputs that satisfy `f`'s input
   contract will yield avalue that satisfies `f`'s output
   contract.Therefore, for logic mode definitions, ACL2s only needs to
   check input contract for "top-level" forms. 
   
   

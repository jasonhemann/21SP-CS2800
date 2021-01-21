---
title: A first dose of ACL2
date: 2021-01-21
---

# Topics 

  - [Lisp v. Racket](#lisp-v-racket)
  - The ACL2s Environment
  - data, datatypes, and functions
  - "All the Lisp you need to know"

# The end goal, and how we get there.

    | In order to ...                        | we must first ...                                         | 
    | ... reason about our programs          | have a precise understanding of what they do.             | 
    | ... precisely say what our programs do | give an interpretation for each program in the language   | 
    | ... give every p ∈ L an interpretation | precisely specify the language L – the syntax of programs | 

Thus, we start by discussing the programming language in which we're
working. Not because *this* language is soooo much better than any
other language (IMHO, all programming languages are bad), but because
it's just important to fix *some* programming language, to have a
concrete discussion.

## Lisp History

  - John McCarthy. 
  - Wasn't intended as a PL (happens in a couple places!)
  - (One of) the oldest high-level languages.
  - "ACL2" - A Computational Logic for Applicative Common Lisp
  -  ACL2 Lisp is a language with history.  Poor conventions so some
     of the things aren't as first principles (`car`, `cdr`)
  - e.g. "... of Lisp on the IBM 704." Obviously. ಠ_ಠ

# The data, the universe

## The atoms 

Including: 

- rationals 
- symbols
- chars
- strings
- booleans

## Our booleans are different.

  - you need to know that `()` is `nil`, and that's your false you're used to.
  - no `else`, `t` for the fall-through case. 

## The data structures - `cons` `car` `cdr`

### Lisp v. *SL

- `cons` arbitrary binary trees vs. tl. (`cons`tructor)
- how to read, expand the tl into full bt.

### Four different views of data structures:

1. `(cons ... (cons  ...))`

2. Maximal dots 

3. boxes and arrows 

4. Suppressed dots

## = The Universe 

(The stuff about which we can write ACL2 programs.)

We'll say "All" when we allow anything in the universe.

#### Exercises:
   Your objectives are to express these in each of the above. 

   1. `()`
   1. `(a . d)`
   1. `(d g)`
   1. `((c ()) a ())`
   1. `(() . ((b . ()) . ()))`

## Expressions

- Expressions are things you can evaluate. 
- Some expressions are self-evaluating.
- One of the things we can talk about are programs *in* the language.

# Two built-in functions: 


## `(equal a b)` 

`‌‌[[(equal x y)]]` is `t` if `[[x]]` = `[[y]]`, and `nil` otherwise. 

## `if` 

What's the signature of `if`? 

### Not every list beginning w/ `(if ...)`  is an expression!

#### !! Not Racket!

   ```lisp 
   (if () 'b 'c)
   ```
#### Q

A - The list `(if () 'b 'c)`
B - `b`
C - `c` 
D - does not evaluate, signals an error

Why? 

#### Q 

Why is it important that acl2 evaluate `if` _lazily_?

# non-built-in Functions

## A reasonable question to ask here: 

- Why are those two built-in functions different than these other ones that are, "built into" (predefined in) acl2s? 

The distinction is the theory, the axioms for reasoning. With those
first two, we could construct, by writing programs for, all the other
ones that will follow. But we need to have those two in order to write
the rest. So these other functions don't add any "power" to the
language we didn't already have in principle.

## how-to. Different syntax 

 - `?` in Scheme and Racket, `p` for "predicate"
 - `check-expect`, now `check=`
 - `define` becomes ... 

Not `define` the way you did. 

# defun

# defunc 

`defun` w/ *contracts* 

```lisp
(defunc name (args) 
  :input-contract 
  :output-contract 
  body)
```

"Keyword arguments"

```lisp
:input-contract ...
:output-contract ...
```

`definec` short-hand version

```lisp
(definec name (args : pred) :out-pred 
;;  :input-contract (optional but allowed)
;;  :output-contract (optional, but allowed)
  body)
```

# Primitive types and predicates

The primitive types include: 

- rational,
- nat,
- integer,
- and pos

whose recognizers are
- rationalp,
- natp,
- integerp,
- and posp,


---
title: (Remainder) of the Dirty Dozen
date: 2021-09-16
---

## More bits of syntax

### PollEverywhere `atom` question. 

#### The recognizer `atom`, which is not like the others

### `let`, `let*` 

### Simultaneous vs sequential `let`

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

### constructing `enum` data 

#### range data

`(defdata probability (range rational (0 <= _ <= 1)))`

`(defdata big-nat (range integer ((expt 2 64) < _))`


# `not` v. `endp` v. `lendp` 

`cond` cases

# `quasiquote` and `unquote`

## Just the facts ma'am.

## 3D code examples.

# `t`, `nil` are symbols!? Yeah. 

## Not so weird, you know languages where 0 and 1 are the boolean values, so ....

# More macros

## Macros: a way to write down shorthand that expands into a long-form.

## We will use them; we shall not construct our own. 

## I went and checked: `first` is just `car`, etc

## `caddar` `cdddr`

## `definec` vs. `defunc`: when it gets complicated

 ```lisp
 (defunc lsd (s1 s2)
   :ic (and (tlp s1) (tlp s2) (list-set s1) (list-set s2))
   :oc (and (tlp (lsd s1 s2)) (list-set (lsd s1 s2)))
   (cond
     ((lendp s2) s1)
     (t (remove (car s2) (lsd s1 (cdr s2)) :test 'equal))))
 ```

# Program mode, logic mode, tracing.

## Tracing programs with

`trace$`

`untrace$`

## Try tracing executions of the following:

```lisp
(definec tapp (x :tl y :tl) :tl
  (declare (xargs :mode :program))
  (cond 
   ((lendp x) y)
   (t (lcons (head x) (tapp (tail x) y)))))
```
vs. 

```lisp
(definec tapp (x :tl y :tl) :tl
  (cond 
   ((lendp x) y)
   (t (lcons (head x) (tapp (tail x) y)))))
```

static contract checking! What a win!

## Tests and theorems

I *have* just been writing programs at the REPL. As we know that's a
 *terrible* testing strategy!

### `check=` tests

### property-based testing 

`(test? (equal (app (list x y) (list)) (list x y)))`

`(test? (implies (natp n) (equal (even-integerp n) (even-natp n))))`


# `thm`

  E.g. 

  ```lisp
  (thm (implies 
         (implies a 
           (implies b c))
             (implies (implies a b) 
                      (implies a c))))
  ```

  Like, you too could reason through that, but it's not trivial. 


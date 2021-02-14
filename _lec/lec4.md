---
title: (Remainder) of the Dirty Dozen
date: 2021-01-27
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

I'll see if I can do this one. 




`'(a b c)`




makes a list of data. This is convenient when you want to make a list of a bunch of data. You can tell this is a list of data, and the quote makes the difference.



```lisp
> '(+ 3 4)
(+ 3 4)
> (+ 3 4)
7
```



This is a rather nice way of making a list of data, if you already know all of the pieces you want to put in that list. But what if I want a list of data where I still need to compute one of the elements? Or if one of the things I want to put in that list is the value of a variable? Well,




`(list 'a 'b 'c 'd '(e f g))`





makes a list of data. So we could write 




`(list 'a 'b (+ 3 4) 'c 'd '(e f g))`


and that works. This is inconvenient, though! As soon as we wanted to put the value of *one* computation into a data structure---even if that was just looking up the value of a variable---we had to suddenly go and quote all the other pieces. So that ACL2 knows we aren't trying to use 'a' as a variable. This was cruddy. Furthermore, now it's more difficult to visually _see_ the structure of the data we are building. 


So quasiquote comes to the rescue.

This lets us write out the data we *wish* we'd been able to write to start with, and the only one part we have to change is the part that's now different.

```

`(a b ,(+ 3 4) c d (e f g))

```

With quasiquote, much like a reporter's interstitial remarks in the middle of a quotation in a newspaper, lets us do some non-literal, non-quote pieces in the middle of a quote. 

So these two are the same

```
`(a b c)
'(a b c)
```

Well, if quasiquote is so powerful and all-fired awesome, then why do we still have `quote`?

Well, suppose you *wanted* to put a comma in a list, for instance?

```
'(my favorite piece of punctuation , an awesome little thing)
```

This lets us do that. There is more and it goes much deeper, but this is all the information you need to know. 

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

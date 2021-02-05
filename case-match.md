---
title: Case-match Documentation
layout: single
toc: true
classes: wide
---

# `case-match`

Pattern matching is a mechanism for checking a variable symbol against a pattern. It can also deconstruct the variable into its constituent parts. We can think it as a more powerful version of `cond` in acl2. 

## General Form

```lisp
(case-match x
  (pat1 body1)
  (pat2 body2)
  ...
  (patk bodyk))
```
Where x is the variable symbol that we are checking against. The pati are structural patterns, and the bodyi are terms. case-match returns the value of the bodyi that corresponds to the first pati matching of x, or nil if none of the pati matches x. However, it is in general a good practice to have a full coverage of cases. case-match is  similar to `cond` except the conditions are replaced by structural patterns.

## Pattern Language:
* With a few special exceptions described in the **Exceptions** section below, matching requires that the `cons` structure of x be isomorphic to that of the pattern (for example, the structure of `(cons 2 nil)` is isomorphic to the pattern `(cons a b)`), down to the atoms in the pattern.
* Non-symbol atoms in the pattern match only themselves (e.g. `(cons ‘a nil)` does not match the pattern `(cons B nil)`. Because ’a does not match B, it only matches A).
* Symbols in the pattern denote variables which match anything and which are bounded by a successful match to the corresponding substructure of x.
* Variables that occur more than once must match the same (`equal`) structure in every occurrence (e.g. `(A 2 2)` matches the pattern `(y z z`), but `(A 2 3)` does not).

### Exceptions
```lisp
&               Matches anything (like a wild card).  & is not bounded, 
                  and repeated occurrences of & in a pattern may match 
                  different structures.
nil, t		    These symbols cannot be bound and match only their
                  global values.
!sym            where sym is a symbol that is already bound in the
                  context of the case-match, matches only the
                  current binding of sym.
'obj            Matches only itself.  This is the same as (QUOTE obj).
```

### Sample Patterns and Matches
Below are some sample patterns and examples of their matches and non-matches.
```lisp
pattern       matches          non-matches
(y z z)       (ABC 3 3)        (ABC 3 4)    ; 3 is not 4
(a . rst)     (A . B)          nil          ; NIL is not in the form (a . rst)
(‘a (G x) 3)  (A (H 4) 3)      (B (G X) 3)  ; ‘a matches only itself. 
(& t & !x)    ((A) T (B) (C))               ; provided x is '(C)
(list a b c)  (list 1 2 1)     (list 1 2)   ; (list 1 2) is not isomorphic to (list a b c)
```

## `case-match` example
Consider a math comparison expressions (compexpr) that can take the form of a rational number or a list with two rational numbers and a comparison operator (one of ‘>, ‘<, or ‘=). To put this in acl2 language, we have 
```lisp
(defdata compop (enum ‘(> < =)))
(defdata compexpr (oneof rational
                        (list rational rational compop)))
```
Suppose we want to recognize a math comparison expression x and evaluate it. The following function with `case-match` would do:
```lisp
(definec evalcomp (x :compexpr) :bool
  (case-match x
     (a t)                  
     ((list a b ‘<) (< a b)) 
     ((list a b ‘=) (== a b))
     ((list a b ‘>) (> a b))
     (& nil)))               ;; fall-through case (it is actually not neccessary here 
                             ;; since the 4 patterns above fully cover all the possible patterns)
                             ;; but it may come in handy if you want to collect all the cases
                             ;; you did not cover and handle them together.
```

_Note: If you are not going to use a variable in your body term, do not give it a name. Otherwise ACL2S will complain about it. For example, if I only care about the first in the pattern (cons first rest), I can structure my pattern as (cons first &)._

Bad example:
```lisp
(case-match tl
    ((first . rest) first) ;; ACL2S will complain because rest is not used in the body term
    (& nil))
```
Another bad example:
```lisp
(case-match tl
    ((list a b c d) (+ a b)) ;; ACL2S will complain because c and d are not used in the body term
    (& 0))
```
Use `&` to stop ACL2S from complaining about unused variables:
```lisp
(case-match tl
    ((list a b & &) (+ a b)) ;; The pattern still matches list with 4 elements, but only the first 2 elements are named and used
    (& 0))
```

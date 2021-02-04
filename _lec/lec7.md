---
title: Substitutions, `case-match`, reductions
date: 2021-02-03
---

# Valid formulae =_df tautologies

For propositional logic, the valid formulae are also called
"tautologies." You might know these from your other logic classes.

# Substitution, etc. 

  - `acl2::subst` vs *a* substitution

Separate the idea of a _substitution_ from it's representation. We can
represent a substitution many different ways. Same with a mathematical
function. E.g. as a mapping, etc. 

_We_ (in this class) will represent a substitution as a list of
bindings (under restrictions). Here is an example:

   ```lisp
   ((X T) (Y NIL) (W T))
   ```

This is a particularly convenient and clever choice of notation for we
ACL2ers! As we will soon see. 

# Notation: |_σ

Firstly, you should know that "σ" is for substitution what "x" is for
variable and "f" is for functions. Just the common choice of variable
name. No one taught me that, you just get it by acculturation. 

## Restriction of (a function) to it's domain. 

This notion of restriction is [common in
mathematics](https://en.wikipedia.org/wiki/Restriction_(mathematics))

You might want to restrict a function's domain for any number of
reasons. One common example is trying to construct an inverse function
`f(x)^{-1}`. Often the inverse is not a function.

## `f : X -> Bool iff { x ∈ X s.t. f(x) } ⊆ X`

  That is all to say that we can use the function from `X` into `Bool`
  to pick out a particular subset of `X`. That's what a Boolean
  function is: it's a way to pick out a subset of all the inputs.

## So finally: instantiation.

We can see a formula `(ϕ -> (ψ -> ϕ))` as a general form describing
all the particular instances of formulae of that shape.

  - `(a -> (b -> a))` 
  - `((d ≡ b) -> (g -> (d ≡ b)))` 
  - `((p -> q) -> ((p -> q) -> (p -> q)))` 
  - `(d -> (d -> d))` 
  
  In fact we can see this formula schema `(ϕ -> (ψ -> ϕ))` as
  describing the _set_ of all its instances. And partially
  instantiating `(ϕ -> ((d ≡ b) -> ϕ))` means that a whole bunch of
  formulae no longer fit this scheme. We've shrunk the set of
  instances that it represents. Instantiation is a kind of
  restriction. 

### Q: Why is the inverse not a function? 

So, after we appropriately restrict the domain of the function `f` to
the inputs on which its defined (call the resulting function `f'`)
then we *can* produce an inverse of `f'`

# Bringing this back to ACL2.

## We can use ACL2 to verify propositional logic equivalences for us. 

	```lisp
	(thm 
	  (implies (and (boolp p) (boolp q))
		(iff (not (or p q)) (and (not p) (not q)))))
	```

## We can use ACL2 to search for counterexamples!

	```lisp
	(test?
	  (implies (and (boolp p) (boolp q))
		(iff (not (or p q)) (and p (not q)))))
	```

  And ACL2 will give them to us. How? As a substitution! 
  And it is so convenient to have them as a substitution? 
  
  Because a substitution is *exactly* the same as a LISP `let`
  binding.
  
  ```lisp
  (let 
    (iff (not (or p q)) (and p (not q))))
  ```

  You can even look at the two sides independently and watch them
  evaluate to different answers.
  
  ```lisp
  (let 
    (not (or p q)))
  ```

  ```lisp
  (let 
    (and p (not q)))
  ```

## So how does this notion of `restriction` relate to `instantiation`?

So how does this notion of `restriction` relate to `instantiation` of
a formula by a substitution. 

## `case-match` in ACL2

How it is that we can understand a substitution, we can see what
happens in a pattern matching expression.

We can round-trip these acl2s expressions, matching on things as we go. 

```lisp
(definec try (ls :tl) :tl
  (case-match ls
    (() '(it-is-empty))
    ((a . d) (list 'it-is-a-cons 'with 'a 'as a 'and 'd 'as d))))
```

See the `:doc case-match` documentation for the best detailed
explanation.

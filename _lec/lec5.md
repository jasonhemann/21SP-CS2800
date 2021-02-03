---
title: Property-based testing, and the utility. 
liquid: enabled
date: 2021-01-28
---

## Discussion 
 
 - Property-based testing: a kind of testing you likely haven't seen before 
 - PBT vs. other kinds of testing
 - How to do it, how to think about them generally
 - A common pitfall 
 - How to do it, how to think about them in acl2s

### `check=` tests, unit tests. 

## Tests and Theorems

I *have* just been writing programs at the REPL. As we know that's a
 *terrible* testing strategy!

### property-based testing 

 - Somewhat of a "middle ground" between unit testing and theorem proving
 - A different style of testing than the kind you know. 


`(test? (equal (app (list x y) (list)) (list x y)))`

`(test? (implies (natp n) (equal (even-integerp n) (even-natp n))))`

### Theorem proving

## `thm`

  E.g. 

  ```lisp
  (thm (implies 
         (implies a 
           (implies b c))
             (implies (implies a b) 
                      (implies a c))))
  ```

  Like, you too could reason through that, but it's not trivial. 

## `defthm` 

### `thm` and `test?` -- better together!


# Don't just take my word for it! 

[Specifying Properties on Pure Functions](https://www.youtube.com/watch?v=zvRAyq5wj38) 

## Used in Industry 

## You already have access to these tools!!

## Writing some programs



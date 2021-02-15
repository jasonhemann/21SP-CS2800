---
title: Homework 5
layout: single
due: 
---

# Objectives 
 
 - Practice the subtleties of substitution in ACL2S
 - Cement understanding of normal forms and normalization
 - Practice writing proofs over straight-line code. 


## TL;DR Instructions: 

 * Submit [hw5a.lisp]({{ site.baseurl }}/hw/hw5a.lisp) and
   [hw5b.lisp]({{ site.baseurl }}/hw/hw5b.lisp) (the hw files) on
   gradescope in the corresponding drop box.

 * We have now deliberately removed our earlier allowances that let us
   elide checking function and body contracts. You now know what these
   are and understand them, so we will use this HW to get more
   comfortable.

 * Insert your solutions into this file where indicated.

 * Only add to the file. Do not remove or comment out anything pre-existing.

 * For each function definition, you must provide both contracts and a body.

 * We have now deliberately removed our earlier allowances that let us
   elide checking function and body contracts. You now know what these
   are and understand them, so we will use this HW to get more
   comfortable.

 * Insert your solutions into this file where indicated.

 * Only add to the file. Do not remove or comment out anything pre-existing.

 * Make sure ACL2s accepts your file, at least up to any portions with
   the pen-and-paper proofs (those that you might have mechanically
   checked via [Drew's page](http://checker.atwalter.com)).

## Further technical and pedantic instructions: 

 - For this homework you will need to use ACL2s.

 - Make sure you are in ACL2s mode. This is essential! Note that you can
   only change the mode when the session is not running, so set the correct
   mode before starting the session.

 - If you don't finish all problems, comment the unfinished ones
   out. You must not leave any uncommented `...` in the code. Files we
   cannot immediately run we will mark a 0. 
   
 - Do not submit the `.lisp.a2s` file, only the `.lisp` file. We call
   the `.lisp.a2s` file a "session file"; this just shows your
   interaction with the theorem prover, it is not part of your
   solution.

 - Appropriately testing means writing sufficient example-based and/or
   property-based tests to properly check all the possible scenarios
   according to the data definitions of the involved types. 

 - The number of tests should reflect the difficulty of the
   function. For complex functions with numerical output, you want to
   test whether it behaves according to spec in different ways on a
   sufficient number of inputs.

 - Beyond that, use good judgment and write a clearly sufficient
   variety of tests, so you aren't even close to the line. For what we
   deem insufficient test case coverage we will deduct points.

 - You can use any types, functions and macros listed on the [ACL2s
   Language
   Reference]({{ site.baseurl }}/acl2s-reference/)


---
title:  "Homework 2"
layout: single
due: 
---

[hw2a]({{ site.baseurl }}/hw/hw2a.lisp)
[hw2b]({{ site.baseurl }}/hw/hw2b.lisp)

# Objectives 
 
 - Write some ACL2s functions against given requirements
 - Practice using and understanding ACL2s' contract system
 - Form groups and working relationships 
 - Practice writing naturally-recursive functions

> Computer science is the continuation of logic by other means. 
> -- Georg Gottlob

## TL;DR Instructions: 

 * Submit your homework files in the corresponding drop box.

 * Insert your solutions into this file where indicated. 

 * Only add to the file. Do not remove or comment out anything pre-existing.

 * For each function definition, you must provide both contracts and a body.
 
 * You must provide adequate tests. We further elaborate on the
   definition of "adequate tests" below.
   
 * Solutions must be naturally recursive (whenever possible), or
   credit will not be given.
 
 * You may not use built-in procedures that handle the bulk of the
   work. For example, you may not use the append function from. But
   once you have defined your own append feel free to use it for later
   problems.
 
## Further technical and pedantic instructions: 

 - For this homework you will need to use ACL2s.

 - Make sure you are in ACL2s mode. This is essential! Note that you can
   only change the mode when the session is not running, so set the correct
   mode before starting the session.

 - Make sure ACL2s accepts the entire file. 

 - If you don't finish all problems, comment the unfinished ones
   out. You must not leave any uncommented `...` in the code. Files we
   cannot immediately run we will mark a 0. 
   
 - Use comments for any English text that you add. You will see
   exemplary comments in the starting file.

 - Do not submit the `.lisp.a2s` file, only the `.lisp` file. We call
   the `.lisp.a2s` file a "session file"; this just shows your
   interaction with the theorem prover, it is not part of your
   solution.

 - At least for right now, appropriately testing means writing
   example-based tests that cover at least all the possible scenarios
   according to the data definitions of the involved types. For
   example, a function taking two lists should have at least 4 tests:
   all combinations of each list being empty and non-empty.

 - Beyond that, the number of tests should reflect the difficulty of
   the function. For very simple ones, the above coverage of the data
   definition cases may be sufficient. For complex functions with
   numerical output, you want to test whether it produces the correct
   output on a reasonable number of inputs.

 - Use good judgment. For what we deem unreasonably few test cases we
   will deduct points. Write a clearly sufficient variety of tests,
   so you aren't even close to the line. 

 - You can use any types, functions and macros listed on the [ACL2s
   Language
   Reference](http://pages.github.ccs.neu.edu/jhemann/21SP-CS2800/acl2s-reference/)

 - Since this is our first real programming exercise, we have
   simplified your interaction with ACL2s somewhat. Right now, we will
   not require ACL2s to prove termination and contracts. Instead we
   shall allow ACL2s to proceed even if a proof fails. If ACL2s finds
   a counterexample, however, ACL2s will still report that to you.


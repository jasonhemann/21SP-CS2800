---
title: Homework 4
layout: single
due: 
---

> To prove a thing is not enough; you have to seduce people to accept it. 
> -- Friedrich Nietzsche

## TL;DR Instructions: 

 * Find the starting files in the usual place. 
 
 * Submit `hw4a.lisp` and `hw4b.lisp` (the hw files) on gradescope in the
   corresponding drop box.

 * Insert your solutions into this file where indicated (usually as `...`)

 * Only add to the file. Do not remove or comment out anything pre-existing.

 * For each function definition, you must provide both contracts and a body.
 
 * You must provide adequate tests. For this homework we will write
   example-based tests using `check=`. These are like the tests you'd
   have written in Fundies I. We further elaborate on the definition of 
   "adequate tests" below. 

 * We use the following ASCII character combinations to represent the
   Boolean connectives:

	 - NOT     !
	 - AND     &
	 - OR      v
	 - IMPLIES =>
	 - EQUIV   ==
	 - XOR     ><

 * We have not defined an operator precedence or a default
   associativity. Unless otherwise noted, just write the
   disambiguating parentheses.

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
   Reference](http://pages.github.ccs.neu.edu/jhemann/21SP-CS2800/acl2s-reference/)

 - We have simplified your interaction with ACL2s somewhat. Right
   now, we will not require ACL2s to prove termination and
   contracts. Instead we shall allow ACL2s to proceed even if a proof
   fails. If ACL2s finds a counterexample, however, ACL2s will still
   report that to you.

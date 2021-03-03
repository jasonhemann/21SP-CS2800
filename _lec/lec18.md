---
title: More Halting, More Undecidability Results
date: 2021-03-03
---

# Continuation 

## To start out. 

## What is it that's undecidable? 

  We cannot have a general algorithm that will tell you whether, for
  any string (data), whether it is in this set, or not in this set.

  Steps. Assume (towards contradiction) that the set *is* decidable. 

  - We have shown that TM can run any program, including an encoding
    of some other TM, on it. We could install VirtualBox on our
    VirtualBox image, right?

  - Therefore, then (toward contradiction), there must be a TM that
    computes that function (1,0) that recognizes the set.
	
  - Call H the algorithm that reads <M,w>, and outputs "Accept" if M
    accepts w, Right before it would reject, we go into an infinite
    loop. "Reject" if M rejects w.

####  Q1. Under what conditions will M accept w? 
When the set M computes includes w. 

####  Q2. Under what conditions will M reject w? 
When the set M computes does not include w. 

####  Q3. How does that manifest on the machine?
	Either the machine would have rejected w or it go into an
     infinite loop. We ensured that it does.)
  
  (Again, it doesn't exist. Assume it does.)
  
  Let's construct a machine. 
  
  D. (for "diagonalization", in honest. 
  
  Let's build the machine D. D is a contrarian kind of thing. 
  
  Input to D: description of a machine, "M". 
  
  D("M") 
   if (M["M"]) "REJECT"
   else "ACCEPT"
  
  We could very well do the ones that *do* accept themselves as input.
  
  That is: D is a collection of machine descriptions that do not
  accept themselves as input.
  
  Run D on itself. 
  
  D("D") : the collection of machines that don't accept themselves as
  input. 
  
  What does it mean to say that D doesn't accept itself as input? 
  
  D is one of the machines that accepts itself as input. 
  
  What does it mean to say that D accepts itself as input?
  
  D is one of the machines that *doesn't* accept itself as input.
    
  The theorem: that the halting problem is unsolvable.

## History 

  - In 1900 David Hilbert publishes his 23 problems
    - No. 2 is proving the consistency of the Peano axioms 
  - ~1930 Hilbert has rephrased the problem to 3 parts: 
    - Is mathematics _complete_? 
	- Is mathematics _consistent_?
	- Is mathematics _decidable_?
  - Turing is not the first. Alonzo Church w/λ-calculus proof. 
    - Turing submits his manuscript a month later, it's published a year later. 
	- However, before that people are working in recursive functions on number theory
	- Turing introduces the computer inter alia.
	  - Introducing a mechanizable computer wasn't the point of the
        work! Just incidental!
  - By 1940s, the British govt. is asking him to assist w/war-time
    decoding efforts, and they are actually *using* some of these
    kinds of machines that were before just theoretical inventions.
    - Bombes. [Great book on Colossus](http://www.colossus-computer.com/) 
  - Jailed. Homosexual. His work was secret, the govt. People got
    credit for his prior work, or for things that would have been
    consequences, that he could have done, but for government
    classification.

## Alan Turing 
  - Bletchley Park is now home of the big computer museum.
  - The highest prize in CS is called the "Turing award" in his honor.

# Left off

## `http://www.lel.ed.ac.uk/~gpullum/loopsnoop.html`

## A Third View:

  [Diagonalization
  POV](https://en.wikipedia.org/wiki/Halting_problem#Sketch_of_proof)

## So What? 
 
-- It gets worse! 

## What else can we not decide? 


#### Rice's theorem:
   
   Decision problem for any non-trivial extensional property of Turing
   machines is undecidable.

It states that for any non-trivial property, there is no general
decision procedure that, for all programs, decides whether the
partial function implemented by the input program has that
property.

   - Non-trivial – not "all of them" or "none of them"
   - Extensional – not about "does it have 3 "0"s in its source code,
     about how it *behaves*
   
   You can't tell much of anything about _every_ program. 




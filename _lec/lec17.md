---
title: Undecidability Halting Problem Results
date: 2021-03-01
---

# Exam: How'd you all go? 

`PollE` 

# Termination Results, Problems

  I have asserted at you that ACL2 cannot prove termination, and that
  it's not just ACL2. 
  
# Today, the Proof

## Turing Machines 

  Based upon results, I don't need to explain to you all what a Turing
  machine is.
  
## It's a most-general model of computation (C-T thesis).

  If we want to understand 'algorithm,' we do it with respect to
  *some*, _particular_ model of computation. Analogy: play
  "poker". You can play the game under pretty much any "good enough"
  set of rules. You need to decide on *a* set of rules. Kind of like
  that for descriptions of Turing machines, or for models of
  computation generally.
  
  There are any number of good common choices. Which particular one we
  choose doesn't matter.
  
  It's not even my favorite. Point is that we have one.
  
## Why are they *useful* models. 

  Everyone else is doing applied mathematics and working on questions
  in recursive function theory. DYK: "computer" used to be a job
  they'd hire you for? Or advertise for in the newspaper? I want to
  find one, so that I have one, a picture of some newspaper article
  with an ad for a computer. Looking to hire one.
  
  Turing lies out in a field for a month or so, and thinks about,
  "What is it that mathematicians /do/?"
  
  1. Self-evidently the task of computation. 
  
  They are programmable. Early computers weren't utilizing some of the
  important capacities. 
  
  "Universal" Turing machine. 
  
  Without the stored program concept, you'd have one wired up to do
  the work of a toaster, and then re-wire it to do the logic of a
  washing machine. That kind of stuff. These early computers were
  missing the concept of "stored program". Data as code. Running
  data. Turing had that idea.
  
  It was essential to his work. 
  
  Minimalist models of computation. Shown though that they can, and
  are powerful enough to, /simulate/ other computers.  
  
###  Computers that can simulate other computers. 

  This was exciting and novel at the time! You all have it now, on
  your machines! The VM you're running in VirtualBox Any OS. And you
  could install software on it. We installed CodeTogether. So it's not
  nearly so foreign as it was. But would you have thought about
  virtual machines before you ever saw a computer? Before anyone had
  seen one?
  	
	
## "Codes for". There is a program on your hard drive.
   
   It's data. When you run it, it looks and acts just like a computer. 
   
   Your VM boots up to an OS. You could imagine it was a dedicated
   computer, though, that did just one thing. 

## "Codes for" 

   Things "code for." We know all about encoding systems. ASCII,
   etc. Programs as data are not foreign concepts. But you want to
   know that we aren't cheating or getting by with anything with these
   "string" things. We can encode strings as numbers. 
   
   ```
   54
    |\ 
   27 2
    |\ 
    9 3
    |\ 
	3 3
  
   2^1 3^3 5^0 ....
   ```

  Here is a unique way of encoding and decoding data, sequences of
  numbers, as numbers themselves. Not the only way. Not especially
  _short_. But it _works_ and that's all we need. Point being, we can
  discuss sets of strings, and you'll know I'm not cheating. 

## Decision Problem. How do you even phrase this as a decision problem? 

   A set. A set of strings. Which sets of strings? What we want is the
   set of strings, that /codes/ for, 
   
   H = {<M,w> | where M codes for a pair of a TM M and M-input w, and
   halts on input w.}

## We have the ability to put programs in an infinite loop:

	```
	10: goto 10
    ```

### To start out. Paradox questions. 
   – Autological
   – Heterological

## What is it that's undecidable? 

  We cannot have a general algorithm that will tell you whether, for
  any string (data), whether it is in this set, or not in this set.

  Steps. Assume (towards contradiction) that the set *is* decidable. 

  - We have shown that TM can run any program, including an encoding
    of some other TM, on it. We could install VirtualBox on our VirtualBox image, right?  

  - Therefore, then (toward contradiction), there must be a TM that
    computes that function (1,0) that recognizes the set.
	
  - Call H the algorithm that reads <M,w>, and outputs "Accept" if M
    accepts w, Right before it would reject, we go into an infinite
    loop. "Reject" if M rejects w.

  (Under what conditions will M accept w? 
   - When the set M computes includes w. 
   Under what conditions will M reject w? 
   - When the set M computes does not include w. 
   How does that manifest on the machine?
   - Either the machine would have rejected w or it go into an
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



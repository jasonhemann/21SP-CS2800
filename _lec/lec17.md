---
title: Halting Problem; Undecidability Results
date: 2021-03-01
---

# Exam: How'd you all go? 

`PollE` 

# Termination Results, Problems

  I have asserted at you that ACL2 cannot prove termination, and that
  it's not just ACL2. 

## ACL2 cannot do it for us! 

# Today, the Proof

  - BTW: If you want to see this recapitulated, Harry Porter @ PSU has
    a nice [video](https://www.youtube.com/watch?v=o7I-N6KlkTg)

## Poll: Turing Machines 

  Do you know what a Turing machine is?

## Turing Machines---Why do they matter? 

  A TM is a most-general model of computation (according to the
  /Church-Turing thesis/). 
	
  If we want to understand 'algorithm,' we do it with respect to
  *some*, _particular_ model of computation. Here's an analogy: what
  do you mean when you say "play poker"? You can play the game under
  pretty much any "good enough" set of rules. You need to decide on
  *a* set of rules. It's kind of like that for (descriptions of)
  Turing machines. Which particular one we choose matters in the
  details.
  
  The purpose is kind of like that. It's /an/ adequate, general model
  of computation. However you formally describe them (there's lots of
  different ways)---or for choosing your models of computation more
  generally---more important than the particulars of any one model is
  that we have *an* adequate model; the details matter less.
  
  By the way, there are any number of good common choices. Turing
  Machines are not even my favorite. Point is that we have such a
  model, and a description of it.    
  
## Why are they *useful* models. 
  
### Put yourself in this historical time and place. 

  DYK: "computer" used to be a job they'd hire you for? If you were
  looking for a computer, you'd put an ad up in the "help wanted"
  section of a newspaper: ["Looking to
  hire.."](https://chroniclingamerica.loc.gov/lccn/sn82015387/1907-08-03/ed-1/seq-1/print/image_681x648_from_3510%2C2255_to_4747%2C3433/)

  This is coming out of work in applied mathematics and areas of
  philosophy, really. There was no computer science, there was no
  computer. Everyone else is doing applied mathematics and working on
  questions in recursive function theory.

  Turing lies out in a field for a month or so, and thinks about,
  "What is it that mathematicians /do/?" 
  
  -- They make marks on paper. If you were going to mechanize a
  mathematician, what would the least job of it be? 
  
  1. This is self-evidently the task of computation.
  
  2. Minimalist models of computation. Importantly, they are also
     /programmable/. Shown though that they are powerful enough to
     /simulate/ other computers.
    
## "Universal" Turing machine. 
  
## /Stored-program/ concept
  
  Early computers weren't utilizing some of the important
  capacities. John Von Neumann gets credit for this at the time,
  IIRC. These early computers were missing the concept of "stored
  program". Data as code. Running data. Turing had that idea, with the
  "Universal" Turing machine.

### Code-as-data, data-as-code. 

  Without the stored program concept, you'd have one wired up to do
  the work of a toaster, and then re-wire it to do the logic of a
  washing machine. That kind of stuff. 
  
  This was essential to Turing's work. 
    
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
   computer, though, that did just one thing. Imagine if you lived in
   a world where you could run one program at a time, and that program
   was loaded on boot.

## "Codes for" 

   Things "code for." We know all about encoding systems. ASCII,
   etc. Programs as data are not foreign concepts. But you want to
   know that we aren't cheating or getting by with anything with these
   "string" things. We can encode strings as numbers. This is, by the
   way, an inessential detail, but it is also very cool and neat and
   fun to think about how /mathematicians/ would think of "encoding"
   data. 
   
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

## The Decision Problem

### How do you even phrase halting on input as a decision problem? 

   A set. A set of strings. Which sets of strings? What we want is the
   set of strings, that /codes/ for,
   
   `H` `=` `{` `<M,w>` `|` where `M` codes for a pair of a TM `M` and M-input `w`, and
   halts on input `w`.`}`

## We have the ability to put programs in an infinite loop:

	```
	10: goto 10
    ```

### Paradox questions. 
   – Autological
   – Heterological


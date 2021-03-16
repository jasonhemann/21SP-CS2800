---
title: Accumulators, Examples of proving w/accumulators
date: 2021-03-17
---

# Plans 

## Rest of Sem. Sched.

## Outline next project, due dates

## Project presentations

## Next class? Next, next class!

Discussed earlier plans. Those plans upon us. I can't imagine enjoying
a final exam more. 

So meeting w/folk re: these. 

# Accumulators + Reasoning w/Accumulators

## Recap what we saw w/accumulators on Last Mon.

## Defining functions w/accs

  1. Start with a function `f`.
  2. Define `ft`, a tail-recursive version of `f` with an accumulator.
  3. Define `f*`, a non-recursive function that calls `ft` and is
     logically equivalent to `f`, i.e., the following is a theorem
     `hyps ⇒ (f* ...) = (f ...)`

## Proofs w/, wrt/ accumulator fns.
 
 4. Identify a lemma that relates `ft` to `f`. It should have the
    following form: `hyps ⇒ (ft ... acc) = ... (f ...) ...` Remember
    that you have to generalize, so all arguments to `ft` should be
    variables (no constants). The RHS should include `acc`.

 5. Assuming that the lemma in 4 is true, and using only equational
    reasoning, prove the main theorem `hyps ⇒ (f* ...) = (f ... )`

    If you have to prove lemmas, prove them later. (Like an adult!)

 6. Prove the lemma in 4. Use the induction scheme of `ft`.
 
 7. Prove any remaining lemmas. (Like an adult!)


## Example

```lisp
(definec powof3 (b)
  (if (zp b) 1
    (* 3 (powof3 (1- b)))))

(definec powof32h (b)
  (powof32h b 1))

(definec powof32h (b c)
  (if (zp b) c
   (powof32h (1- b) (* 3 c))))
```

## The lemma of 4
`(pow32 b c) = (* (pow3 b) c)`

## The main theorem


# Time permitting: review more proof Qs from HW 


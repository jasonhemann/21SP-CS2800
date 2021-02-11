---
title: 
date: 2021-02-11
---

# Where we last left. 

  - We had a more powerful technique. Program /equivalences/ rather than just boolean expressions. 

# Booleans, we had counterexamples. So far our claims that we've seen also give us some counterexamples. 

   |                  | Just Booleans | Expressions          |
   | Counterexamples  | Generated     | Can test if supplied |
   | Power            | Lesser        | Greater              |
   | Decidable?       | Yes           | No                   |
   | Automated Proofs | Trivially yes | Can be verified      |

## With greater power comes fewer freebies.

## Machine checkable.

  - Which is great! 
  - Important to have a standardized format. 

## Proof Setup

   - Starting w/a given conjecture
   - Check the contracts
   - Contract completion (if necessary)
   - Exportation (if necessary)
   - Write out the /context/
   - Write out the /derived context/

 These last two are new for us today!

## Context
   - The initial setup that we have. Presumptions. 

### Contexts vs. Theorem

How do we think about a context vs. a theorem?

We can _instantiate_ a theorem, since it holds generally. The _context_ is fixed in any particular situation.

   ``` 
	 (implies (endp x) (endp y))
   ```

We had better not confuse `(endp x)` with something that holds for *all* values of `x`!

### Derived Context.
 These are the obvious things that follow from the context + axioms/theorems

### Common patterns:

  - (endp x), (tlp x): (== x nil)
  - (tlp x), (consp x): (tlp (rest x))
  - φ1 ∧ … ∧ φn -> ψ: Derive φ1,…,φn and use MP to derive ψ

## HW: Examples of such conjectures

### The pen-and-paper prover isn't fully battle-ready.
 Upgrades coming, plus more documentation

### Come back together and work together




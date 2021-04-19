---
title: Evaluator again, and project notes
date: 2021-04-19
---

Project is due† the 21st, presentations also due† the 21st.

† Yes, but you all do have the free unpenalized extensions, should you
choose to take advantage of them.


Q: How should I 'lint' my prose? 

I use some of: 

  - `codespell`
  - [`anorak`](http://jwilk.net/software/anorack)
  - `aspell` and `hunspell` via [`enchant-2`](https://en.wikipedia.org/wiki/Enchant_(software))
  - [`write-good`](https://github.com/btford/write-good) 
  - [GNU `style` and `diction`](https://www.gnu.org/software/diction/)
  - `atd` 
  - [`retext`](https://github.com/retextjs/retext)
  - `proselint`, via [`vale`](https://docs.errata.ai/vale/about)
  - [`textlint`](https://textlint.github.io/)
  
  Also, if I can't find the right word but I'd know it when I see it,
  `powerthesaurus` (has a website and command-line query interface). 
  
Q: Where do I/we sign up for a review date/time?
Waitwhile again, I'll post a link again to the front page for you

Q: What kind of common errors did people make on drafts? 

You should eliminate `to be` verbs, unnecessary prepositional phrases,
and passive voice. To a first approximation, remove all uses of the
passive voice, past or future tenses. Try and remove prepositional
phrases in favor of possessive adjectives. Check for s/v disagreement,
singular/plural.

Watch exophoric references. "It" and "its" are especially problematic
for you all.

Avoid "group", "homework" etc; treat it like real writing.

Look for sentences devoid of content. Find places where you can cut
extraneous words or sentences for which the converse is obviously
false. This was common. 

Be consistent with your capitalization, e.g. python -> Python

Use it or math font `$\frac{4}{5}$` $$\frac{4}{5}$$ for variables that
are mathematical, and tt for code (verbatim/minted, etc). I'm happy to
give you my prelude if you want, but I won't support it.

ACL2/proof people, *please* be aware you need to prove this in
ACL2. Not proof checker. Not Tarski's world. Not Virginia Klenk's
books proof system. Not Scala's type checker. ACL2(s).

Skip the grandiloquent words, e.g. Utilize -> use.

Go on a "which" hunt---do not use "which" when you mean "that."

If you are stuck at a proof, and think you have the right lemmata, use
skip-proof to ensure your proof goes through with them. Note, this
does not guarantee that your lemmata are valid, just that they would
be sufficient to show your main result.

When you get a contract failure, take the `(progn ....)` expression,
pick it up and run it. remove the `(with-output ...)` in favor of the
sub-expression, change '`undefined`' to '`undefined2`' uniformly, and
then look at the output.

```
(set-gag-mode nil) 
debug-guards debug-measure/debug-termination 

> (PROGN (ENCAPSULATE ... (WITH-OUTPUT :OFF :ON (UNDEFINED2 ...))
(ENCAPSULATE ... (WITH-OUTPUT :OFF :ON ....))
(ENCAPSULATE ... (FOO A B))
```

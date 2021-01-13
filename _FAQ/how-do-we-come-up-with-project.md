---
title: How do we come up with a project idea?
---

Some students had expressed trouble coming up with project ideas. This
is perfectly reasonable. In practice, proofs often come up "by need."
This is to say the motivation for *doing* some formal proof or
mechanization (e.g [CompCert](https://en.wikipedia.org/wiki/CompCert))
is extrinsic. We have a purpose in mind, and thus we do a proof.

One bit of advice is to think about some function or functions from
[fundies I](https://course.ccs.neu.edu/cs2500/) to the first half
maybe of [fundies II](https://course.ccs.neu.edu/cs2510/), with some
complicated interaction behavior. Think some specification of behavior
more complex than it's type in Java. Something than even in Java you'd
*have* to put in the comments because you couldn't express it in Java
even with Java's types. That's often good fodder.

You can think much more about plain ol functions over lists and
trees---inductively defined kinds of data structures. Maybe how they
work in tandem. No need to drift into the kind of complex,
number-theoretic sort of stuff. As we all know, [the natural numbers
are a really an inductively defined data
structure](https://github.com/idris-lang/Idris-dev/blob/a59c371b5dfe1bbeebc0ecccbfd094a6ae27628f/libs/prelude/Prelude/Nat.idr#L15-L21),
and our grade school teachers were all holding out on us in some grand
conspiracy. But the relationships are often murkier than with other
kinds of examples.

Another way to try and think of these is thinking of the *properties*
you want functions to have, with respect to other calls, or perhaps
how functions interact. The kinds of things for which you'd've used
test? or quickcheck way back yon. Remember not wanting/having
to/needing to write tests? If you prove it, then you can rely on
it. [Here's a video on the kinds of
properties](https://www.youtube.com/watch?v=zvRAyq5wj38) we'd be
thinking about. They're using them for writing randomized tests, but
let's prove them true.

*Another* place you could go / thing you could do is try to prove
formally some of those properties we've spelled out on the last couple
of homework assignments. The "big ones." IMHO, `(defthm
make-ACL2-accept-me (implies (lorp x) (permp x (rev x))))` is
*already* a beastly sort of proof. You could try and draw inspiration
from some of the earlier sample middle lemmas that Shankar had to come
up with in our recent lab. You could try and re-prove one of his
smaller lemmas, for instance. I think working over lambda calculus
expressions might be more than you want to deal with. But that could
be fun, if you're already into that stuff.

Starting with some function you want to write could be fun. Because
termination is something we have to guarantee, and that might itself
require some non-trivial proof. But it might not be super exciting for
you at the end of the day to get "ta da---we have this function that
we already knew how to write in like 3m in Racket!!!" I was *hoping*
that folk would have imagined or remembered something in the distant
past where like an incorrect F1, F2 assignment meant that functions
didn't always work together like you'd wanted them to, and so we ended
up getting points off or had to write scores of tests to find the
mistake, and oh, goodess if we'd just forced ourselves to show it
correct, then we'd've found that bug way early.

I've been talking about pure functions, but we can simulate ADTs and
state-changes and mutation. ACL2 can as we know do some quantified
reasoning, but these are beyond what we've done in class so just know
that's extra.

I was hoping that you all would be able to push it through with our
own definitions (e.g. our `rev2`, not the ACL2 `rev`) so as not to
piggyback on top of
["magic"](https://en.wikipedia.org/wiki/Clarke%27s_three_laws#Clarke's_second_law:~:text=Any%20sufficiently%20advanced%20technology%20is%20indistinguishable%20from%20magic.).

As you know with this stuff, there's often a reeeeeally narrow and
deep chasm between the [trivial and the mighty
tough](https://xkcd.com/1425/). So bear that in mind. Make sure it's
not trivial (ACL2 proves it right away for you) and we'll go from
there. So thinking about [how you'll use ACL2]({{
site.baseurl}}/_FAQ/how-do-I-steer.md) can help you in selecting a
problem.

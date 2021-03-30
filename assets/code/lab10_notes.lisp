;; We'll start with some of the definitions we've seen before.
(definec app2 (x :tl y :tl) :tl
  (if (endp x)
	  y
    (cons (first x) (app2 (rest x) y))))

(definec rev2 (x :tl) :tl
   (if (endp x)
	   x
     (app2 (rev2 (cdr x)) (list (car x)))))

(definec len2 (x :tl) :nat
  (if (endp x)
      0
    (+ 1 (len2 (rest x)))))

(definec in2 (a :all X :tl) :bool
  (and (consp X)
       (or (equal a (first X))
           (in2 a (rest X)))))

(definec del (a :all X :tl) :tl
  (cond ((endp X) nil)
        ((equal a (car X)) (del a (cdr X)))
        (t (cons (car X) (del a (cdr X))))))

;; We'll talk about doc topics a few times in this file.

;; You can read them either by typing :doc <topic> in ACL2s (replacing
;; <topic> with the topic you're interested in seeing), or by looking
;; at the online XDoc documentation site.

;; Here's a link to that site:
;; https://www.cs.utexas.edu/users/moore/acl2/current/combined-manual/?topic=ACL2____TOP

;; I'll denote doc topics by square brackets. e.g. [history] means the
;; doc topic "history", which can be seen either by typing :doc
;; history in ACL2s, or by typing "history" in the "Jump to" box in
;; the online XDoc documentation viewer linked above.
;; If the content in the square brackets contains "acl2::", you should
;; not type that if you're using the online viewer. You will need to
;; type it if you're viewing the documentation inside of eclipse though.

;; First, some discussion of the history commands.
;; see [history] for a full list.
;; We'll mainly be using :pl here. see more information in [pl].
;; :pl will show you all of the rules that are applicable to a particular statement.
;; We'll talk more about the details later, but here's an example.
:pl (len2 x)
;; This prints out 4 rules:
;; (I elide some details with ellipses)
#|
(:DEFINITION LEN2-DEFINITION-RULE)
  New term: (IF (CONSP X) (+ 1 (LEN2 (CDR X))) 0)
  Hypotheses: ((FORCE (TLP X)))
  Equiv: EQUAL
  Substitution: ((X X))

(:DEFINITION LEN2) (disabled)
  New term: ...
  Hypotheses: <none>
  Equiv: EQUAL
  Substitution: ((X X))

(:TYPE-PRESCRIPTION LEN2-CONTRACT-TP)
  ...

(:TYPE-PRESCRIPTION LEN2)
  ...
|#
;; We see two :DEFINITION rules. The second one is disabled, which
;; means that ACL2s will not use it unless we specifically ask it to.
;; The first says that when we see (len2 x), we can replace it with
;; (IF (CONSP X) (+ 1 (LEN2 (CDR X))) 0) if we can prove that (TLP X) holds.
;; (ignore the FORCE for now).

;; This is true under the EQUAL (equality) equivalence relation. We'll
;; see some non-EQUAL equivalence relations soon, but just know that
;; we can always replace a term with something it is equal to.

;; Ignore the :TYPE-PRESCRIPTION rules, they're not relevant to our
;; discussion today.

;; The rules are ordered by "age" in increasing order;
;; (:DEFINITION LEN2-DEFINITION-RULE) was the most recently defined
;; rule, and will be the first one that ACL2s tries to use (as we'll
;; see)

;; Here's a nice little lemma about len2 and rev2.
(defthm len2-rev2
  (implies (tlp x)
           (equal (len2 (rev2 x)) (len2 x))))

;; Now, say we want to prove this.
(thm (implies (tlp b)
     (equal (len2 (rev2 (cons a b)))
            (+ (len2 b) 1))))

;; I imagine that I'll be able to prove this in the following way:
;; (note that "hints" I give are just a textual description of the
;; justification for the step)
#|
(equal (len2 (rev2 (cons a b))) (+ (len2 b) 1))
= { use len2-rev2 with (x (cons a b)) }
(equal (len2 (cons a b)) (+ (len2 b) 1))
= { expand len2 }
(equal (+ 1 (len2 b)) (+ (len2 b) 1))
= { by arithmetic }
t
|#

;; However, the proof ends up requiring a doubly-nested induction and
;; takes a while.

;; Note that ACL2s prints out the rules it used to prove the thm.

;; defthm by default creates a :REWRITE rule; therefore if len2-rev2
;; was used, we'd expect to see (:REWRITE LEN2-REV2) in the list of
;; used rules. But we don't!

;; Why isn't it used? It seems obvious that it is applicable...
;; Let's take a look at what the rewriter does...
;; First, note that it moves through terms "inside out", or "left-most
;; innermost first"

;; That is, if we have a statement (foo (bar a b) c (baz d e)), the
;; rewriter will try to perform rewriting in the following order:
;; a, b, (bar a b), c, d, e, (baz d e), (foo (bar a b) c (baz d e))

;; This can be problematic if you want to apply a particular rule, but
;; a term inside of it gets rewritten before your rule is considered.

;; See [introduction-to-rewrite-rules-part-1],
;; [introduction-to-rewrite-rules-part-2], and
;; [further-information-on-rewriting] for more information about
;; rewriting.

;; Let's now dig into how the rewriting occurs in our thm above.
;; I'm just going to focus on the second argument of the (implies ...)
;; since it is the most interesting.

#|
(equal (len2 (rev2 (cons a b)))
       (+ (len2 b) 1))
;; first we try to rewrite a and b, neither is rewritten (variables "rewrite to themselves")
;; next we try to rewrite (cons a b).
;; what rules will ACL2s consider for (cons a b)? run :pl (cons a b).
;; This prints out two rules. Note that both of them have 
;; "Equiv: STR::STRING-LIST-EQUIV$INLINE"
;; This means that the rules are only relevant when rewriting something that is
;; directly inside of a call to string-list-equiv$inline.
;; We're not inside of such a call, so neither rule is applicable.

;; next we try to rewrite (rev2 (cons a b)). :pl (rev2 (cons a b))
;; prints out 4 rules. We start from the top and will use the first
;; one that is applicable.

;; The first rule, (:DEFINITION REV2-DEFINITION-RULE), has "Equiv:
;; EQUAL", so that is OK. We then check to see if the hypotheses of
;; the rule are provable given what we know. Since we know (tlp b), it
;; is certainly true that (TLP (CONS A B)) is true. Thus, we use this
;; rule.
;; This means we replace (rev2 (cons a b)) with the "New term:" shown in :pl
(equal (len2 (IF (CONSP (CONS A B))
                (APP2 (REV2 (CDR (CONS A B)))
                      (LIST (CAR (CONS A B))))
                (CONS A B)))
       (+ (len2 b) 1))

;; When we rewrite something, we immediately try rewriting the new
;; term (the IF statement in this case)
;; first we try to rewrite a and b in the if test, neither is rewritten.
;; next we try to rewrite (cons a b). no applicable rules, as before.
;; next we try to rewrite (consp (cons a b)).
;; :pl (consp (cons a b)) shows many rewrite rules. As before, we start from the top.

;; The first two rules are very similar. Let's look at the
;; first, (:REWRITE ACL2::CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP . 2)
;; Note that the output for this rule includes "Free variable: ACL2::X".
;; We haven't seen this yet. To explain this, we'll take a look at the
;; theorem that lead to this rule using:
:pf ACL2::CONSP-WHEN-MEMBER-EQUAL-OF-SYMBOL-SYMBOL-ALISTP
;; This produces:
(IMPLIES (AND (ACL2::SYMBOL-SYMBOL-ALISTP ACL2::X)
              (MEMBER-EQUAL ACL2::A ACL2::X))
         (CONSP ACL2::A))
;; You can ignore the ACL2::... business, that just relates to packages.
;; This theorem says that, if you know that X is a SYMBOL-SYMBOL-ALIST,
;; and that A is a member of X, then (CONSP A) is true.
;; This will create a rewrite rule that tells ACL2s it can replace 
;; (CONSP A) with T if it can show that
;; (AND (ACL2::SYMBOL-SYMBOL-ALISTP ACL2::X) (MEMBER-EQUAL ACL2::A ACL2::X))
;; Note that because X appears in the hypotheses but not on the left
;; hand side of the body, it is a "free variable" and ACL2s will need
;; to guess what to replace X with whenever it uses this rule.
;; See [acl2::free-variables] for more info.
;; In the end, to use this rule we need to have some X for which we know
;; both (ACL2::SYMBOL-SYMBOL-ALISTP ACL2::X) and (MEMBER-EQUAL ACL2::A ACL2::X).
;; We don't, so we can't use either of the first two rules.
;; The next two rules are disabled, so we skip them.
;; We next get to (:REWRITE ACL2::DEF=>CONS). This rule has no hypotheses
;; and has EQUAL as its "Equiv" relationship, so we can use it.
;; We replace (CONSP (CONS A B)) with T.
(equal (len2 (IF T
                (APP2 (REV2 (CDR (CONS A B)))
                      (LIST (CAR (CONS A B))))
                (CONS A B)))
       (+ (len2 b) 1))
;; if statements are treated specially; if we can show that the test
;; is either T or NIL, we can replace the whole if statement with
;; the appropriate branch and rewrite that branch.
;; So we get:
(equal (len2 (APP2 (REV2 (CDR (CONS A B)))
                   (LIST (CAR (CONS A B)))))
       (+ (len2 b) 1))
;; And now we try to rewrite the APP2 call.

;; As before, we first try to rewrite A, B, and (CONS A B) on the
;; first line. None can be rewritten.
;; We then try to rewrite (CDR (CONS A B)). :pl (CDR (CONS A B))
;; shows 3 rules; the first can't be used because it doesn't hold
;; under EQUAL, and the second can't be used because it only is
;; true if (NOT (CONSP (CONS A B))).
;; Therefore ACL2s uses the third, (:REWRITE CDR-CONS) and rewrites
;; (CDR (CONS A B)) to B.
;; ACL2s will go through a similar process to rewrite (CAR (CONS A B))
;; to A using (:REWRITE CAR-CONS).
;; We arrive at the following:
(equal (len2 (APP2 (REV2 B) (LIST A)))
       (+ (len2 b) 1))
|#
;; If we scroll back to the top of the output from the THM, we can see
;; that this is exactly Goal'!

;; The real problem with the above is that ACL2s decided to rewrite
;; the rev2 call, leaving us with an expression that it couldn't apply
;; len2-rev2 on (because the statement no longer contained (len2 (rev2 ...))).
;; So how can we tell the theorem prover that we don't want to expand rev2?

;; There are a few ways.
;; First, we could tell it "hands off of rev2"
;; This means that any call to rev2 will not be rewritten.
;; Arguments may be rewritten though.
(thm (implies (tlp b)
              (equal (len2 (rev2 (cons a b)))
                     (+ (len2 b) 1)))
  :hints (("Goal" :hands-off (rev2))))

;; we could also say "forget about REV2-DEFINITION-RULE for the duration of this proof"
(thm (implies (tlp b)
              (equal (len2 (rev2 (cons a b)))
                     (+ (len2 b) 1)))
  :hints (("Goal" :in-theory (disable (:DEFINITION REV2-DEFINITION-RULE)))))

;; we'll ask ACL2s to print out more information for this next thm
(set-gag-mode nil)
;; (set-gag-mode nil) is often useful when debugging a proof;
;; it results in ACL2s printing out each goal, plus some extra info.
;; we could tell ACL2s "throw an instance of this lemma into the hypotheses"
(thm (implies (tlp b)
              (equal (len2 (rev2 (cons a b)))
                     (+ (len2 b) 1)))
  :hints (("Goal" :use ((:instance len2-rev2 (x (cons a b)))))))

;; here's a good overview of useful hints
;; [https://www.cs.utexas.edu/users/moore/acl2/current/combined-manual/index.html?topic=ACL2____INTRODUCTION-TO-HINTS]

;; Note that which statement is on the LHS and which is on the RHS of an equals in a defthm matters.
;; The LHS is what ACL2 will try to match with. The RHS is what ACL2 will replace a match with (under substitution)
;; We can see this with the following:
:pl (len2 (rev2 x))
:pl (len2 x)
;; note that len2-rev2 was only listed for the first :pl, and not for the second.

;; Sometimes you want ACL2s to prove something and remember it, but not create a rewrite rule.
;; This is because some rewrite rules are bad - they make a statement more difficult to prove than the original.
;; See [https://www.cs.utexas.edu/users/moore/acl2/current/combined-manual/?topic=ACL2____EQUIVALENT-FORMULAS-DIFFERENT-REWRITE-RULES] for some examples.
;; You can actually get the rewriter into an infinite loop pretty easily!
;; For example,
#|
(defthm rev2-len2
  (implies (tlp x)
           (equal (len2 x) (len2 (rev2 x)))))
(thm (implies (tlp b)
              (equal (len2 (rev2 (cons a b)))
                     (+ (len2 b) 1))))
|#
;; will result in a "HARD ACL2 ERROR in ACL2::REWRITE"
;; this is because it will keep rewriting (len2 (rev2 (cons a b)))
;; into (len2 (cons a b)) using len2-rev2, and then it will rewrite it
;; back into (len2 (rev2 (cons a b))) using rev2-len2.
;; We shouldn't have len2-rev2 and rev2-len2 enabled at the same time.
;; However, rev2-len2 is not a very good rule: it makes a statement
;; that only contains len2 more complicated by adding a rev2.
;; Therefore we probably don't want it anyways.

;; We've mainly talked about rewrite rules here (definition rules are
;; similar).
;; There are other classes of rules, but I'm not going to talk about
;; them today.

;; Now, we'll look at a few sample exercises. These are from
;; [acl2::introduction-to-the-theorem-prover].
;; I recommend reading through at least the above doc topic,
;; and trying to get these defthms to go through by yourself
;; first.

;; =======================
;;       Example 1
;; =======================
(defthm triple-rev2
  (implies (tlp x)
           (equal (rev2 (rev2 (rev2 x)))
                  (rev2 x))))
;; Trying to run this results in counterexamples being found.
;; Note that each of the counterexamples has a note printed after it
;; that says "The above counterexample is not consistent with the top-level form."
;; This means that it is a counterexample to some subgoal that ACL2s
;; ran into during the proof, but not the original statement.
;; This can happen if ACL2s tries to generalize too much during a proof,
;; and does not indicate that the statement is false.

;; Above that, you can see the following:
#|
*** Key checkpoint under a top-level induction: ***

Subgoal *1/3''
(IMPLIES (AND (TLP (CDR X))
              (CONSP X)
              (EQUAL (REV2 (REV2 (REV2 (CDR X))))
                     (REV2 (CDR X))))
         (EQUAL (REV2 (REV2 (APP2 (REV2 (CDR X)) (LIST (CAR X)))))
                (APP2 (REV2 (CDR X)) (LIST (CAR X)))))
|#
;; ACL2s tried to do a proof by induction, and got stuck here.
;; A good strategy when ACL2s can't prove a statement is to
;; look at these key checkpoints under induction and try to figure
;; out what lemma ACL2s needs to be able to prove the checkpoint.
;; The bit that sticks out to me here is that ACL2s needs to show
;; that (EQUAL (REV2 (REV2 <stuff>)) <stuff>).
;; Maybe the proof will go through if ACL2s knows that
;; (rev2 (rev2 x)) == x ?
(defthm double-rev2
  (implies (tlp x)
           (equal (rev2 (rev2 x))
                  x)))

(defthm triple-rev2
  (implies (tlp x)
           (equal (rev2 (rev2 (rev2 x)))
                  (rev2 x))))
;; This time the proof goes through.

;; =======================
;;       Example 2
;; =======================
;; Here is a function that determines whether all elements in
;; x are also in y.
(definec subset2 (x :tl y :tl) :bool
  (cond ((endp x) t)
        ((in2 (car x) y)
         (subset2 (cdr x) y))
        (t nil)))

;; We want to show that it is reflexive.
(defthm subset2-reflexive
  (implies (tlp x)
           (subset2 x x)))
;; Again, we get a failure.
;; Here, the we get the following key checkpoint under induction
;; in our output:
#|
*** Key checkpoint under a top-level induction: ***

Subgoal *1/3''
(IMPLIES (AND (TLP (CDR X))
              (CONSP X)
              (SUBSET2 (CDR X) (CDR X)))
         (SUBSET2 (CDR X) X))
|#
;; No matter what induction scheme we choose, because the original
;; statement has (subset2 x x), we'll always have to deal with
;; something like (subset2 (cdr x) (cdr x))
;; (e.g. the first and second arguments will be identical)
;; Intuitively, we know that
;; (subset2 z w) => (subset2 z (w plus more elements))
;; That would be helpful here, because X is just ((CDR X) plus more elements)
;; Let's try the following:
(defthm subset2-cons
  (implies (and (tlp x) (tlp y) (subset2 x y))
           (subset2 x (cons a y))))

(defthm subset2-reflexive
  (implies (tlp x)
           (subset2 x x)))
;; Again, this goes through now.

;; =======================
;;       Example 3
;; =======================
;; Finally, we have this function, that determines whether
;; any element of X occurs more than once.
(definec dupsp (x :tl) :bool
  (cond ((endp x) nil)
        ((in2 (car x) (cdr x)) t)
        (t (dupsp (cdr x)))))

;; Here's what we would like to prove.
(defthm dupsp-rev2
  (implies (tlp x)
           (equal (dupsp (rev2 x))
                  (dupsp x))))
;; This time, we get the following:
#|
*** Key checkpoint under a top-level induction: ***

Subgoal *1/4''
(IMPLIES (AND (TLP (CDR X))
              (CONSP X)
              (NOT (IN2 (CAR X) (CDR X)))
              (EQUAL (DUPSP (REV2 (CDR X)))
                     (DUPSP (CDR X))))
         (EQUAL (DUPSP (APP2 (REV2 (CDR X)) (LIST (CAR X))))
                (DUPSP (CDR X))))
|#
;; To be able to use the induction hypothesis, we need to turn
;; (DUPSP (APP2 (REV2 (CDR X)) (LIST (CAR X)))) into
;; (DUPSP (REV2 (CDR X)))
;; To do this, ACL2s needs to know that:
;; (not (in2 a z)) => (equal (dupsp (app2 z (list a))) (dupsp z))

(defthm in2-dupsp-redundant
  (implies (and (tlp x)
                (not (in2 a x)))
           (equal (dupsp (app2 x (list a)))
                  (dupsp x))))

(defthm dupsp-rev2
  (implies (tlp x)
           (equal (dupsp (rev2 x))
                  (dupsp x))))
;; This fails with the same key checkpoint!
;; Why can't acl2s use this proof?
;; Well, the instantiation of in2-dupsp-redundant that we want is
;; in2-dupsp-redundant|((x (rev2 (cdr x))) (a (car x)))
;; This has two hypotheses, one of which is:
;; (not (in2 (car x) (rev2 (cdr x))))
;; We know in Subgoal *1/4'' that (NOT (IN2 (CAR X) (CDR X))),
;; but we need to tell ACL2s that (in2 a x) iff (in2 a (rev2 x)).

(defthm in2-rev2
  (implies (tlp x)
           (equal (in2 a (rev2 x))
                  (in2 a x))))

(defthm in2-dupsp-redundant
  (implies (and (tlp x)
                (not (in2 a x)))
           (equal (dupsp (app2 x (list a)))
                  (dupsp x))))

(defthm dupsp-rev2
  (implies (tlp x)
           (equal (dupsp (rev2 x))
                  (dupsp x))))

;; It goes through!
;; Note that it needs a triply-nested induction though.
;; (you can tell because the last line says "That completes the proofs of *1.1.1.1, *1.1.1, *1.1 and *1.")
;; If one wanted to, you could try creating lemmas for some of these
;; nested inductions. For example, if gag-mode is set to nil, you
;; can scroll back to the line "So we now return to *1.1.1.1, which is"
#|
(IMPLIES (AND (TLP A4)
              (NOT (DUPSP A4))
              (IN2 A3 A4)
              (NOT (IN2 A3 (APP2 A4 (LIST X3)))))
         (DUPSP (APP2 A4 (LIST X3)))).
|#
;; What exactly is going on here?
;; Well one of the hypotheses is (NOT (IN2 A3 (APP2 A4 (LIST X3))))
;; This is clearly always false. Maybe ACL2s doesn't know that
;; (in2 a y) => (in2 a (app2 x y))
;; Let's provide a lemma like this and try the proof one more time,
;; just for fun.
;; First we need to undo the last defthm so that we can try it again
;; after defining our new lemma. That's what :u does.
:u

(defthm in2-app2
  (implies (and (tlp x) (tlp y) (in2 a x))
           (in2 a (app2 x y))))

(defthm in2-app2-y
  (implies (and (tlp x) (tlp y) (in2 a y))
           (in2 a (app2 x y))))

(defthm dupsp-rev2
  (implies (tlp x)
           (equal (dupsp (rev2 x))
                  (dupsp x))))

;; Well, we got rid of one level of nested induction.
;; I'll leave getting rid of the other nested inductions
;; as an exercise for the reader.

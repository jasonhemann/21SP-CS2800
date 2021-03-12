;; If we don't set do-not-alias, the proof checker gets a little unhappy later on.
(defdata lor (listof rational) :do-not-alias t)

;; Given some functions:
(definec insert (e :rational L :lor) :lor
  (cond ((endp L) (list e))
        ((<= e (car L)) (cons e L))
        (t (cons (car L) (insert e (cdr L))))))
        
(definec isort (L :lor) :lor
  (if (endp L)
      L
    (insert (car L) (isort (cdr L)))))

(definec orderedp (L :lor) :bool
  (or (endp (cdr L))
      (and (<= (car L) (second L))
           (orderedp (cdr L)))))

(defthm insert-orderedp
  (implies (and (rationalp e) (lorp l) (orderedp l))
           (orderedp (insert e l))))

(defthm orderedp-isort
  (implies (lorp l)
           (orderedp (isort l))))

(definec del (a :rational L :lor) :lor
  (cond ((endp L) L)
        ((equal a (car L)) (cdr L))
        (t (cons (car L) (del a (cdr L))))))

(definec permp (x :lor y :lor) :bool
  (if (endp x)
      (endp y)
    (and (in (car x) y)
         (permp (cdr x) (del (car x) y)))))


;; Lemma l4
;; (permp x y) => (permp (insert e x) (cons e y))
;; Contract completion:
;; (lorp x) ^ (lorp y) ^ (rationalp e) ^ (permp x y)
;; => (permp (insert e x) (cons e y))

;; We do a skip-proofs here so that we can double-check that our
;; below proof for isort-is-perm works first.
;; If it didn't work, we would be better off fixing it rather than
;; potentially proving something we don't need to.
;; Note that skip-proofs can possibly put you in a bad state, because
;; you can make ACL2s agree that something false is true. But it is OK
;; for the purposes of checking whether our below proof is good.
(skip-proofs
 (defthm l4
   (implies (and (lorp x) (lorp y) (rationalp e) (permp x y))
            (permp (insert e x) (cons e y)))))
           
;; since the below proof works, I would now go off
;; and prove l4 by hand

Conjecture isort-is-perm:
(implies (lorp l)
         (permp (isort l) l))

Proof by: Induction on (tlp l)

;; (not (consp l)) => P
Induction Case isort-is-perm-base:
(implies (not (consp l))
         (implies (lorp l)
                  (permp (isort l) l)))

Exportation:
(implies (and (not (consp l))
              (lorp l))
         (permp (isort l) l))

Context:
C1. (not (consp l))
C2. (lorp l)

Derived Context:
D1. (equal l nil) { C1, C2, def lorp }

Goal: (permp (isort l) l)

Proof:
(permp (isort l) l)
= { D1 }
(permp (isort nil) nil)
= { Evaluation }
t
QED

;; (consp l) ^ P|((l (cdr l))) => P
Induction Case isort-is-perm-ind:
(implies (and (consp l)
              (implies (lorp (cdr l))
                       (permp (isort (cdr l)) (cdr l))))
         (implies (lorp l)
                  (permp (isort l) l)))
                  
Exportation:
(implies (and (consp l)
              (implies (lorp (cdr l))
                       (permp (isort (cdr l)) (cdr l)))
              (lorp l))
         (permp (isort l) l))

Context:
C1. (consp l)
C2. (implies (lorp (cdr l))
             (permp (isort (cdr l)) (cdr l)))
C3. (lorp l)

Derived Context:
D1. (lorp (cdr l)) { C1, C3, def lorp }
D2. (permp (isort (cdr l)) (cdr l)) { C2, D1, MP }
;; recall MP = modus ponens is:
;; ((A => B) ^ A) => B

Goal:
(permp (isort l) l)

Proof:
(permp (isort l) l)
= { C1, C3, def isort }
(permp (insert (car l) (isort (cdr l))) l)
= { C1, car-cdr axioms }
(permp (insert (car l) (isort (cdr l)))
       (cons (car l) (cdr l)))
;; Let's assume we're doing a proof by induction on (tlp l)
;; We get to assume:
;; (implies (lorp (cdr l)) (permp (isort (cdr l)) (cdr l)))
;; We also know that 
;; (lorp (cdr l))
;; Therefore we know that:
;; (permp (isort (cdr l)) (cdr l))

;; (permp x y) => (permp (insert e x) (cons e y))
;; Let's call this lemma l4
= { D2,
    lemma l4 ((e (car l)) (x (isort (cdr l))) (y (cdr l))) }
t
QED

QED

;; Here's some notes from the professional method:

#|
Conjecture isort-is-perm:
(implies (lorp l)
         (permp (isort l) l))
...

;; We know that proving (permp (isort l) l) is easy if l is empty.
;; Therefore we'll focus on the hard case: l is not empty.

Proof:
(permp (isort l) l)
= { assume (consp l), def isort }
(permp (insert (car l) (isort (cdr l))) l)
= { assume (consp l), car-cdr axioms }
(permp (insert (car l) (isort (cdr l)))
       (cons (car l) (cdr l)))
;; Let's assume we're doing a proof by induction on (tlp l)
;; We get to assume (via the induction hypothesis, IH):
;; (implies (lorp (cdr l)) (permp (isort (cdr l)) (cdr l)))
;; We also know that 
;; (lorp (cdr l))
;; Therefore we know that:
;; (permp (isort (cdr l)) (cdr l))

;; (permp x y) => (permp (insert e x) (cons e y))
;; Let's call this lemma l4
= { from IH: (permp (isort (cdr l)) (cdr l)),
    lemma l4 ((e (car l)) (x (isort (cdr l))) (y (cdr l))) }
t

QED
;; Why does this last step work?
;; Applying this substitution to l4 gives us:

(permp (isort (cdr l)) (cdr l))
=>
(permp (insert (car l) (isort (cdr l)))
       (cons (car l) (cdr l)))

The LHS of that implication is exactly what we got from the IH, and
the RHS is what we started with in that last step. Therefore, IH +
that instantiation of l4 allows us to get to t.

At this point, we kinda know what the "hard case" of isort-is-perm
looks like, and that we probably want to induct on (tlp l).

We can then go through and write out the full proofs of the two
obligations for isort-is-perm when inducting on (tlp l). Finally, we
tell the proof checker to assume that l4 holds and then check that the
proof works.

If it does work, we then need to prove l4 by hand, possibly using
induction there too.

|#

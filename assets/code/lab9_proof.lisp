(definec app2 (x :tl y :tl) :tl
  (if (endp x)
      y
    (cons (car x) (app2 (cdr x) y))))

(definec rev2 (x :tl) :tl
  (if (endp x)
      x
    (app2 (rev2 (cdr x)) (list (car x)))))

(definec revt (x :tl acc :tl) :tl
  (if (endp x)
      acc
    (revt (cdr x) (cons (car x) acc))))

(definec rev* (x :tl) :tl
  (revt x nil))

(defthm app2-associative
  (implies (and (tlp x) (tlp y) (tlp z))
           (equal (app2 (app2 x y) z)
                  (app2 x (app2 y z)))))

(defthm app2-nil
  (implies (tlp x)
           (equal (app2 x nil)
                  x)))

;; We first need to prove a lemma that relates the accumulator value
;; to the version of the function that doesn't use an accumulator.
Lemma revt-rev2-equiv:
(implies (and (tlp x) (tlp acc))
         (equal (revt x acc) 
                (app2 (rev2 x) acc)))

Proof by: Induction on (revt x acc)

Induction Case revt-rev2-equiv-contract:
(implies (not (and (tlp x) (tlp acc)))
         (implies (and (tlp x) (tlp acc))
                  (equal (revt x acc) (app2 (rev2 x) acc))))

Exportation:
(implies (and (not (and (tlp x) (tlp acc)))
              (tlp x)
              (tlp acc))
         (equal (revt x acc) (app2 (rev2 x) acc)))

Context:
C1. (not (and (tlp x) (tlp acc)))
C2. (tlp x)
C3. (tlp acc)

Derived Context:
D1. nil { C1, C2, C3, PL }

QED

Induction Case revt-rev2-equiv-base:
(implies (and (tlp x) (tlp acc) (endp x))
         (implies (and (tlp x) (tlp acc))
                  (equal (revt x acc) (app2 (rev2 x) acc))))

Exportation:
(implies (and (tlp x) (tlp acc) (endp x))
         (equal (revt x acc) (app2 (rev2 x) acc)))

Context:
C1. (tlp x)
C2. (tlp acc)
C3. (endp x)

Derived Context:
D1. (equal x nil) { C1, C3 }
D2. (equal (rev2 x) nil) { D1, Eval }

Goal:
(equal (revt x acc) (app2 (rev2 x) acc))

Proof:
(revt x acc)
= { def revt, C1, C2, C3 }
acc
= { def app2, D2 }
(app2 (rev2 x) acc)

QED

Induction Case revt-rev2-equiv-ind:
(implies (and (tlp x) (tlp acc) (not (endp x))
              (implies (and (tlp (cdr x)) (tlp (cons (car x) acc)))
                       (equal (revt (cdr x) (cons (car x) acc))
                              (app2 (rev2 (cdr x)) (cons (car x) acc)))))
         (implies (and (tlp x) (tlp acc))
                  (equal (revt x acc) (app2 (rev2 x) acc))))

Exportation:
(implies (and (tlp x) (tlp acc) (not (endp x))
              (implies (and (tlp (cdr x)) (tlp (cons (car x) acc)))
                       (equal (revt (cdr x) (cons (car x) acc))
                              (app2 (rev2 (cdr x)) (cons (car x) acc)))))
         (equal (revt x acc) (app2 (rev2 x) acc)))

Context:
C1. (tlp x)
C2. (tlp acc)
C3. (not (endp x))
C4. (implies (and (tlp (cdr x)) (tlp (cons (car x) acc)))
             (equal (revt (cdr x) (cons (car x) acc))
                    (app2 (rev2 (cdr x)) (cons (car x) acc))))

Derived Context:
D1. (tlp (cdr x)) { C1, C3 }
D2. (tlp (cons (car x) acc)) { C1, C2, C3, car-cdr axioms }
D3. (equal (revt (cdr x) (cons (car x) acc))
           (app2 (rev2 (cdr x)) (cons (car x) acc))) { D1, D2, C4, MP }

Goal:
(equal (revt x acc) (app2 (rev2 x) acc))

Proof:
(revt x acc)
= { def revt, C1, C2, C3 }
(revt (cdr x) (cons (car x) acc))
= { D3 }
(app2 (rev2 (cdr x)) (cons (car x) acc))
= { def app2, car-cdr axioms, C1, C3 }
(app2 (rev2 (cdr x)) (app2 (list (car x)) acc))
= { lemma app2-associative ((x (rev2 (cdr x))) (y (list (car x))) (z acc)) }
(app2 (app2 (rev2 (cdr x)) (list (car x))) acc)
= { C1, C2, C3, def rev2 }
(app2 (rev2 x) acc)

QED

QED

Conjecture rev*-revt-equiv:
(implies (tlp x)
         (equal (rev* x) (rev2 x)))

Context:
C1. (tlp x)

Goal:
(equal (rev* x) (rev2 x))

Proof:
(rev* x)
= { def rev*, C1 }
(revt x nil)
= { C1, lemma revt-rev2-equiv ((acc nil)) }
(app2 (rev2 x) nil)
= { C1, lemma app2-nil ((x (rev2 x))) }
(rev2 x)

QED



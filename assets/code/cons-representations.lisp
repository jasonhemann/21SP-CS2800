#|
  Some functions for printing various representations of cons structures
  Andrew Walter - 2021-01-21 - Northeastern CS2800 Spring 2021
|#

;; A maximal cons representation uses infix . instead of prefix cons
;; to represent conses. It also doesn't suppress conses like 
(definec cons-maximal (x :all) :all
  (if (not (consp x))
      x ;; If it's not a cons, don't do any conversion.
    ;; If it is a cons, recursively convert the car and cdr and
    ;; produce a list with a dot between them.
    (list (cons-maximal (car x))
          '|.| ;; note the use of the bars to represent a symbol for the literal .
          (cons-maximal (cdr x)))))

(check= (cons-maximal '(1 2 3))
        '(1 |.| (2 |.| (3 |.| nil))))
(check= (cons-maximal (cons (cons 1 2) (list 1 2 3 (cons 3 4))))
        '((1 |.| 2) |.| (1 |.| (2 |.| (3 |.| ((3 |.| 4) |.| NIL))))))

(cons-maximal '(1 2 3))
(cons-maximal (cons (cons 1 2) (list 1 2 3 (cons 3 4))))

;; A suppressed cons representation uses infix . instead of prefix
;; cons to represent conses. Unlike a maximal cons representation,
;; some conses are not printed explicitly.
;; We don't produce a . when:
;; - the cdr of a cons is equal to nil (in this case we also don't include the cdr)
;; - the cdr of a cons is another cons
(definec cons-suppressed (x :all) :all
  (cond ((not (consp x)) x)
        ;; We want to transform (cons a nil) into '(a)
        ((and (consp x)
              (equal nil (cdr x)))
         (list (cons-suppressed (car x))))
        ;; We want to not print . if the cdr of this is also a cons
        ((and (consp x)
              (consp (cdr x)))
         (cons (cons-suppressed (car x))
               (cons-suppressed (cdr x))))
        ;; Otherwise, just do the same thing as cons-maximal
        (t (list (cons-suppressed (car x))
                 '|.|
                 (cons-suppressed (cdr x))))))

(check= (cons-suppressed nil) nil)
(check= (cons-suppressed '(1 2 3))
        '(1 2 3))
(check= (cons-suppressed (cons 1 (cons 2 (cons 3 4))))
        '(1 2 3 |.| 4))

(cons-suppressed '(1 2 3))
(cons-suppressed '(1 . (2 . (3 . 4))))

;; This is basically equivalent to cons-maximal, except it uses prefix
;; cons instead of . to represent conses.
(definec cons-maximal-cons (x :all) :all
  (if (not (consp x)) x
    (list 'cons
          (cons-maximal-cons (car x))
          (cons-maximal-cons (cdr x)))))

(check= (cons-maximal-cons nil) nil)
(check= (cons-maximal-cons '(1 2 3))
        '(cons 1 (cons 2 (cons 3 nil))))
(check= (cons-maximal-cons '((1 . 2) . (1 2 3 (3 . 4))))
        '(cons (cons 1 2)
               (cons 1 (cons 2 (cons 3 (cons (cons 3 4) nil))))))


(cons-maximal-cons '(1 2 3))
(cons-maximal-cons '(1 . (2 . (3 . 4))))
(cons-maximal-cons '((1 . 2) . 3))

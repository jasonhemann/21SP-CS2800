#lang racket
;; Note that we're using this alternate miniKanren package
(require minikanren-ee)

;; Here is an example that you should have seen in lecture.

;; Here's a version of append, written in Racket. It's pretty similar
;; to what we would have written in ACL2s.
(define (append l s)
  (cond
    ((empty? l) s)
    ((cons? l)
     (cons (car l) (append (cdr l) s)))))

;; We want to convert this `append` function into a miniKanren
;; relation. To do this, we'll first write this alternate definition
;; of the non-relational version of append. This will help us write
;; the relational version.

;; We do two main things:

;; First, if we see `(car l)` or `(cdr l)` anywhere, we add a
;; `match-let` statement. Effectively this lets us refer to `(car l)`
;; and `(cdr l)` with their own variables.
;; `(match-let (((cons a d) l)) ...)` effectively means "let `a` and
;; `d` be variables such that `(equal? (cons a d) l)` is true"

;; Second, if the end result contains any recursive calls or calls to
;; relations that you've written before that aren't at the top level,
;; you should create a new variable that stores the result of each
;; call. In this case, there is one recursive call in the output and
;; it isn't at the top level (e.g. it is inside another call), so we
;; create a new variable `res` and set it equal to the value of that
;; recursive call. We then replace that recursive call with `res` in
;; the original statement.

;; After doing this, we get the following:

#|
(define (append l s)
  (cond
    ((empty? l) s)
    ((cons? l)
     (match-let (((cons a d) l))
       (let ((res (append d s)))
         (cons a res))))))
|#

;; Now, we can write our relational version of append.
;; Conventionally, we add an 'o' to the end of the name of the
;; non-relational function to get the name of the relational version.
;; This relational function takes in the same arguments as the
;; non-relational function, plus 'o' (for "output").

;; Essentially, we are writing a function that, instead of taking in 2
;; arguments and returning the result of appending them, takes in 3
;; arguments and returns true if the third argument is equal to the
;; result of appending the first two arguments.

;; We replace `cond` with `conde`, and rewrite `(empty? l)` as `(== l '())`.
;; We also can replace a `((cons? l) (match-let (((cons a d) l)) ...))`
;; cond branch with `[(fresh (a d) (== (cons a d) l) ...)]`, which
;; essentially will only be true if l is a cons.

;; There is one more complication below - we almost always want any
;; recursive calls to be last. This affects the performance of
;; miniKanren, though it is effectively logically equivalent.

(define-relation (appendo l s o)
  (conde
    [(== l '()) (== s o)]
    [(fresh (a d)
       (== (cons a d) l) ;; handles both (cons? l) and (match-let (((cons a d) l)) ...)
       (fresh (res)
         (== (cons a res) o) ;; !!!!!! 
         (appendo d s res)))]))
;;(run 1 (o) (appendo '(1 2 3) '(3 4 5) o))
;;(run 10 (x y) (appendo x y '(1 2 3 4 5)))
;;(run* (x y) (appendo x y '(1 2 3 4 5)))
;;(run 1 (x) (appendo x '(1 2 3) '(5 6 7)))

;; Quick sidebar on conde: it is how one expresses disjunction in miniKanren
#|
(conde [A B C]
       [C D E])
==
(or (and A B C)
    (and C D E))
|#
;; Quick sidebar on cond: remember that in a cond, you get to assume that the
;; tests for the previous cond branches are false.
;; You don't get to assume that with conde - you need to explictly state that
;; a previous condition was false if that is relevant to you.
#|
(cond [(equal? x '()) ...]
      [else ...])
(conde
 [(== x '()) ...]
 [(=/= x '()) ...])
|#

;; We discussed this last time.
(define-relation (revo ls o)
  (conde
   [(== ls '()) (== ls o)]
   [(fresh (a d)
           (== (cons a d) ls) ;; implicitly states "ls is a cons"
           (fresh (sl)
                  (appendo sl (cons a '()) o)
                  (revo d sl)))]))

;;(run 1 (o) (revo '(1 2 3) o))
;;(run 2 (o) (revo '(1 2 3) o))
;;(run 2 (o) (revo o '(1 2 3)))

;; Now, we'll convert this function `del-first`, which deletes
;; the first occurrence of `elt` from `ls`.
(define (del-first elt ls)
  (cond
    [(empty? ls) ls]
    [(cons? ls)
     (cond
       [(equal? (car ls) elt) (cdr ls)]
       [else (cons (car ls) (del-first elt (cdr ls)))])]))
#|
(define (del-first elt ls)
  (cond
    [(empty? ls) ls]
    [(cons? ls)
     (match-let (((cons a d) ls))
       (cond
         [(equal? a elt) d]
         [else (let ((res (del-first elt d)))
                 (cons a res))]))]))
|#
(define-relation (del-firsto elt ls o)
  (conde
   [(== ls '()) (== o ls)]
   [(fresh (a d)
           (== (cons a d) ls)
           (conde [(== a elt) (== o d)]
                  [(=/= a elt) ;; we need to explicitly state that the previous cond
                               ;; case in the original program is not true
                   (fresh (res)
                          (== (cons a res) o)
                          (del-firsto elt d res))]))]))
;; (run 1 (o) (del-firsto 'a '(a b c a b c) o))
;; (run 1 (elt) (del-firsto elt '(a b c a b c) '(a c a b c)))
;; (run 1 (elt o) (del-firsto elt '(a b c a b c) o))
;; (run 1 (elt) (del-firsto elt '(a b c a b c) '(c d)))

;; Something that will be relevant for this next function: remember
;; that `and` and `or` can be written as `if` statements.
;; (and a b) = (if a b #f)
;; (or a b) = (if a #t b)

;; Now, we want to convert `in`.
;; This is one way that one might write `in`:
(define (in elt ls)
  (and (cons? ls)
       (or (equal? elt (car ls))
           (in elt (cdr ls)))))
;; We're actually going to go through a few rounds of translation here.
#|
;; First, let's eliminate all of the `and`s and `or`s.
(define (in elt ls)
  (cond
    [(cons? ls) (or (equal? elt (car ls))
                    (in elt (cdr ls)))]
    [else #f]))
(define (in elt ls)
  (cond
    [(cons? ls)
     (cond [(equal? elt (car ls)) #t]
           [else (in elt (cdr ls))])]
    [else #f]))
;; Now, we'll get rid of `(car ls)`s and `(cdr ls)`s using match-let.
(define (in elt ls)
  (cond
    [(cons? ls)
     (match-let (((cons a d) ls))
       (cond [(equal? elt a) #t]
             [else (in elt d)]))]
    [else #f]))

;; We're going to make an assumption that `ls` is a list.
;; If this is true, we can simplify a little bit more from the above.
(define (in elt ls)
  (cond
    [(empty? ls) #f]
    [(cons? ls)
     (match-let (((cons a d) ls))
       (cond [(equal? elt a) #t]
             [else (in elt d)]))]))
|#

(define-relation (ino elt ls o)
  (conde
   ;; note that I'm using 'f and 't to represent false and true.
   ;; you can use whatever values you want to represent true and false, as long as you're consistent.
   ;; you can't use the Racket false and true (#f and #t) inside of relations.
   [(== ls '()) (== o 'f)]
   [(fresh (a d)
           (== (cons a d) ls)
           (conde [(== elt a) (== o 't)]
                  [(=/= elt a)
                   (ino elt d o)]))]))
;;(run 5 (ls) (ino 'a ls 't))
;;(run 5 (ls) (ino 'a ls 'f))
;;(run 5 (elt ls) (ino elt ls 't))
;;(run 5 (elt ls) (ino elt ls 'f))

;; Finally, we're gonna translate subset.
;; As before, we'll have a few stages of translation, where we first get rid of `or`s and `and`s.
(define (subset l1 l2)
  (or (empty? l1)
      (and (in (car l1) l2)
           (subset (cdr l1) (del-first (car l1) l2)))))
#|
(define (subset l1 l2)
  (cond
    [(empty? l1) #t]
    [else
      (and (in (car l1) l2)
           (subset (cdr l1) (del-first (car l1) l2)))]))

(define (subset l1 l2)
  (cond
    [(empty? l1) #t]
    [else
     (cond [(in (car l1) l2)
            (subset (cdr l1) (del-first (car l1) l2))]
           [else #f])]))

(define (subset l1 l2)
  (cond
    [(empty? l1) #t]
    [else
     (match-let (((cons a1 d1) l1))
       (cond [(in a1 l2)
              (subset d1 (del-first a1 l2))]
             [else #f]))]))

(define (subset l1 l2)
  (cond
    [(empty? l1) #t]
    [else
     (match-let (((cons a1 d1) l1))
       (cond [(in a1 l2)
              (let ((res (del-first a1 l2)))
                (subset d1 res))]
             [else #f]))]))
|#
(define-relation (subseto l1 l2 o)
  (conde
   [(== l1 '()) (== o 't)]
   [(fresh (a1 d1)
           (== (cons a1 d1) l1)
           (conde
            [(ino a1 l2 't) ;; a1 is in l2
             (fresh (res)
                    (del-firsto a1 l2 res)
                    (subseto d1 res o))]
            [(ino a1 l2 'f) ;; a1 is not in l2
             (== o 'f)]))]))

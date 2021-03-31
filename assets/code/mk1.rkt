#lang racket
(require minikanren)

(define (membero x ls o)
  (fresh (a d)
    (== ls (cons a d))
    (conde
      [(== x a) (== ls o)]
      [(=/= x a) (membero x d o)])))

(define (direct-flight from to)
  (conde
    ((== from 'miami) (== to 'portland))
    ((== from 'chicago) (== to 'boston))
    ((== from 'houston) (== to 'miami))
    ((== from 'dallas) (== to 'chicago))
    ((== from 'los-angles) (== to 'las-vegas))
    ((== from 'houston) (== to 'dallas))))

(define (flight-path from to route)
  (conde
    ((direct-flight from to) (== route (list from to)))
    ((fresh (layover mid-route)
       (direct-flight from layover)
       (== route (cons from mid-route))
       (flight-path layover to mid-route)))))

(define (app l s)
  (cond
    ((empty? l) s)
    ((pair? l)
     (cons (car l) (app (cdr l) s)))))

(define (appendo l s o)
   (conde
     [(== l '()) (== s o)]
     [(fresh (a d)
        (== l (cons a d))
        (fresh (res)
          (== o (cons a res))
          (appendo d s res)))]))


(run* (q) (fresh (y x)
            (== q (cons x y))
            (conde
              [(=/= x y) (symbolo x) (numbero y)]
              [(== x 'cat)]
              [(== y 'zeplin)
               (fresh (t)
                      (conde
                       ((== t #t))
                       ((== t #f))))])))

#lang racket

(define (var? n) (number? n))
(define (var n) n)

#| 

Term := Symbol 
      | Boolean
      | Empty
      | cons Term Term 

|# 

#| Term × Subst → Term |# 
(define (walk x sub)
  (let ((pr (assv x sub)))
    (if pr (walk (cdr pr) sub) x)))

#| Var × Term × Subst → Boolean |# 
(define (occurs? x t sub)
  (cond
    ((var? t) (eqv? x t))
    ((cons? t) (or (occurs? x (walk (car t) sub) sub)
                   (occurs? x (walk (cdr t) sub) sub)))
    (else #f)))

#| Var × Term × Subst → Maybe Subst |# 
(define (extend-sub x t sub)
  (cond
    ((occurs? x t sub) #f)
    (else `((,x . ,t) . ,sub))))

#| Term × Term × Subst → Maybe Subst |# 
(define (unify t u sub)
  (let ([t (walk t sub)]
        [u (walk u sub)])
    (cond
      ((eqv? t u) sub)
      ((var? t) (extend-sub t u sub))
      ((var? u) (extend-sub u t sub))
      ((and (cons? t) (cons? u))
       (let ([subc (unify (car t) (car u) sub)])
         (and subc (unify (cdr t) (cdr u) subc))))
      (else #f))))


#| Goal :: State → Listof State |# 

#| Term × Term → Goal |# 
(define ((== t u) st)
  (let ((subu (unify t u (car st))))
    (if subu (list (cons subu (cdr st))) '())))

(define (empty-state)
  '(() . 0))

'([([0 . 1]) . 2])

;; call this an f
#| (Var → Goal) |# 
(lambda (var1) (== var1 'turtle))

#| (Var → Goal) → Goal |# 
(define ((bind-var f) st)
  (let ((ct (cdr st)))
    ((f (var ct)) (cons (car st) (+ 1 ct)))))

#| Goal × Goal → Goal |# 
(define ((disj g1 g2) st) ($app (g1 st) (g2 st)))

#| Goal × Goal → Goal |# 
(define ((conj g1 g2) st) ($app-map (g1 st) g2))

(define ($app $1 $2)
  (cond
    ((empty? $1) $2)
    ((procedure? $1) (lambda () ($app $2 ($1))))
    ((cons? $1) (cons (car $1) ($app (cdr $1) $2)))))

(define ($app-map $ g)
  (cond 
    ((empty? $) '())
    ((procedure? $) (lambda () ($app-map ($) g)))
    ((cons? $) ($app (g (car $)) ($app-map (cdr $) g)))))


;; define vs. define-relation

(define ((nevero x) st)
  (lambda ()
    ((nevero x) st)))

(define ((turtles x) st)
  (lambda ()
    ((disj
      (turtles x)
      (== x 'turtle))
     st)))

(define ((cats x) st)
  (lambda ()
    ((disj
      (== x 'cat)
      (cats x))
     st)))

(define ((dogs x) st)
  (lambda ()
    ((disj
      (disj (== x 'dog)
            (== x 'hound))
      (dogs x))
     st)))

(define (pull $) (if (procedure? $) (pull ($)) $))

(define (take n $)
  (cond
    ((zero? n) '())
    (else (cond
            ((empty? $) '())
            ((procedure? $) (take n ($)))
            ((cons? $) (cons (car $) (take (sub1 n) (cdr $))))))))

(define (run n g)
  (take n (g (empty-state))))

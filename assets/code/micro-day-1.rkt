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

#| Term × Term → (State → Listof State) |# 
(define ((== t u) st)
  (let ((subu (unify t u (car st))))
    (if subu (list (cons subu (cdr st))) '())))

(define (empty-state)
  '(() . 0))

;; call this an f
;; (lambda (var1) (== var1 'turtle))

(define ((bind-var f) st)
  (let ((ct (cdr st)))
    ((f (var ct)) (cons (car st) (+ 1 ct)))))

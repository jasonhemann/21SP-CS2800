---
title: Implementing a language evaluator
date: 2021-04-08
---

# Preliminaries

## Additional Bonus - TRACE surveys.

If TRACE eval scores completion % >= 85%

I'll add 2 overall grade points to class grade average. 

# Implementing our own `lambda` calc language

## Implementing languages in terms of host languages.

```
#lang racket

(define (evaluate exp env) 
  (match exp
    [b #:when (boolean? b) b]
    [n #:when (number? n) n]
    [y #:when (symbol? y) (apply-env env y)]
    [`(zero? ,ne) (zero? (evaluate ne env))]
    [`(+ ,ne1 ,ne2) (+ (evaluate ne1 env) (evaluate ne2 env))]
    [`(sub1 ,ne) (sub1 (evaluate ne env))]
    [`(if ,t ,c ,a) (if (evaluate t env) 
                        (evaluate c env)
                        (evaluate a env))]
    [`(lambda (,x) ,b) `(closure ,x ,b ,env)]
    ;; fix 	
    [`(,rator ,rand)
     (match-let ((`(closure ,x ,b ,env^) (evaluate rator env))
                 (a (evaluate rand env)))
       (evaluate b `((,x . ,a) . ,env^)))]))
	
;; Notice we disallowed looking things up in an empty environment
(define (apply-env env y)
  (cdr (assv env y))
  
;; point free definition
(define apply-env (compose cdr assv))

(define (apply-env env y)
  (match-let ((`((,x . ,v) . ,d) env))
    (cond
      ((equal? x y) v)
      ((not (equal? x y)) (apply-env d y)))))

(define (empty-env) 
  `())

(evaluate '(((lambda (y) (lambda (x) y)) 5) 6) (empty-env))
```

## Compositionality

We build complicated expressions' meanings from their immediate sub-pieces.

## First-order implementation. 

Not using `lambdas` for `lambdas`

## Definition is a meaning, is a semantics.

## What about "broken" programs

We don't allow it. 

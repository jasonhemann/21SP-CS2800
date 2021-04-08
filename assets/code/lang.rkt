#lang racket

;;;;;;;;;;;;;;
;; Graham Hutton
;; Easy as 1-2-3
;; (Enrichment stuff)

'(2 3 + 4 *)

;; cf case-match in ACL2 
;; (match <input>
;;   [<pattern-for-possible-shape-of-input> <result expresssion>]
;;   [<next-pattern-for-possible-shape-of-input> <result expresssion>]
;;   [<third-pattern-for-possible-shape-of-input> <result expresssion>])

;; A List is one-of nil or cons a List 

(define (app2 l1 l2)
  (match l1
    [`() l2]
    [`(,a . ,d) `(,a . ,(app2 d l2))]
    [else (error "sadness not in datatype")]))

;; An Env is one-of
;; ()
;; (cons (cons Var Val) Env)

;; A Closure is a listof 'closure! Var Expr Env

;; An Expr is one-of 
;; Nat
;; Var 
;; (+ Expr Expr)
;; (sub1 Expr)
;; (let ((Var Expr)) Expr)

;; Lookup : Var × Env → Val
(define (lookup y env)
  (cdr (assoc y env)))

(define (valof expr env)
  (match expr
    [`,n #:when (number? n) n]
    [`,y #:when (symbol? y) (lookup y env)]
    [`(+ ,nexp1 ,nexp2) (+ (valof nexp1 env) (valof nexp2 env)) ]
    [`(* ,nexp1 ,nexp2) (* (valof nexp1 env) (valof nexp2 env)) ]
    [`(sub1 ,nexp) (sub1 (valof nexp env))]
    [`(let ((,x ,lexp)) ,bod) (let ((v (valof lexp env)))
                                (valof bod `((,x . ,v) . ,env)))]
    [`(lambda (,x) ,bod) `(,x ,bod ,env)] ;; this RHS is called a closure in the biz'
    [`(,rator ,rand) (match-let ([`(,x ,bod ,old-env) (valof rator env)] ;; *** 
                                 [`,v (valof rand env)])
                       (valof bod `((,x . ,v) . ,old-env)))]))

;; (let ((x Expr)) Body)
;; ((lambda (x) Body) Expr)

;; (x ( x f) ___)

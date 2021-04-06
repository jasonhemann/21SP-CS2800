#lang racket
(require minikanren-ee)
(provide assoco reverseo stuttero)
#| 

HW10: Introduction to Logic Programming 

|# 

;; Mathematics is concerned only with the enumeration and comparison
;; of relations.
;; 
;; -- Carl Friedrich Gauss

;; You may find useful the notes on transforming Racket programs to
;; miniKanren on the schedule page. You might also want to consult the
;; [match-let*](http://docs.racket-lang.org/reference/match.html?q=match-let#%28form._%28%28lib._racket%2Fmatch..rkt%29._match-let%2A%29%29)
;; documentation.

#| PART I: EXPLAINING ANSWERS |# 

;; For each of the following queries, explain how miniKanren arrived
;; at its answer. We will grade you on the quality and completeness of
;; your explanation; each explanation will require several sentences.

;; 1. What is the value, and why? 

(run 2 (q)
  (== 5 q)
  (conde
    [(conde
       [(== 5 q) (== 6 q)]) (== 5 q)]
    [(== q 5)]))

#| 

Answer:


|# 

;; 2. What is the value, and why? 

(run* (q)
  (fresh (x y)
    (== `(,x ,y) q)
    (conde
      [(== 'cat 'dog) (== 'cat 'cat)]
      [(conde
         [(== 5 x) (== y x)]
         [(== `(,x) y)]
         [(== x y)])]
      [(== 'dog 'dog)])))

#| 

Answer:


|# 


;; 3. What is the value, and why? 

(run 1 (q) 
  (fresh (a b) 
    (== `(,a ,b) q)
    (absento 'tag q)
    (symbolo a)))

#| 

Answer:


|# 

#| PART II: MINIKANRENIZING |# 

;; Please find below the definitions of assoc, reverse, and
;; stutter. Translate them into their equivalent miniKanren equivalent
;; definitions, assoco, reverseo, and stuttero. We have included app
;; and appendo for you as an example.

;; Please note that in this relational, non-deterministic language, it
;; is possible that a correct implementation will produce answers in a
;; different order. So you may have a correct solution even if it does
;; not pass all of our tests. If you encounter this situation, try and
;; convince yourself that a query to your relation will produce all of
;; the answers we would produce, and no more.

(define (app l s)
  (cond
    [(equal? l '()) s]
    [else
     (match-let ([`(,a . ,d)])
       (let ([res (app d s)])
         `(,a . ,res)))]))

(define-relation (appendo l s o)
  (conde
    [(== l '()) (== s o)]
    [(fresh (a d)
       (== l `(,a . ,d))
       (fresh (res)
         (== o `(,a . ,res))
         (appendo d s res)))]))


(define (assoc x ls)
  (match-let ([`(,a . ,d) ls])
    (match-let ([`(,aa . ,da) a])
      (cond
        [(equal? aa x) a]
        [(not (equal? aa x)) (assoc x d)]))))

;; 4. define-relation assoco





(define (reverse ls)
  (cond
    [(equal? '() ls) '()]
    [else
     (match-let* ([`(,a . ,d) ls])
       (let ([res (reverse d)])
         (append res `(,a))))]))

;; 5. define-relation reverseo





(define (stutter ls)
  (cond
    [(equal? '() ls) '()]
    [else 
     (match-let ([`(,a . ,d) ls])
       (let ([res (stutter d)])
         `(,a ,a . ,res)))]))

;; 6. define-relation stuttero








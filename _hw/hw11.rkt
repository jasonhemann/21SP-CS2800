#lang racket
(require minikanren-ee)
(require rackunit)
(provide assoco reverseo stuttero)
#| 

HW11: Introduction to Logic Programming 

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
     (match-let ([`(,a . ,d) l])
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


;; Unlike Racket's built in version, our version of assoc does not
;; have a case for when x is absent from the list. This will be
;; advantageous when we move to the relational version.

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





;; The below tests are a guide. Your relations might not pass these
;; tests as such----listing goals in a different order may cause the
;; stream to return results in a different order. So it is possible
;; that your code is correct though the tests fail.

;; If you suspect this is the case, you could use some of the other
;; rackunit functionality to ensure, say, that all our answers are
;; found within in a slightly longer list of answers that your
;; relation produces

(test-equal? "stuttero-inverse-mode"
 (run 1 (q) (stuttero q '(1 1 2 2 3 3)))
 '((1 2 3)))

(test-equal? "stuttero-inverse-mode-complete"
 (run* (q) (stuttero q '(1 1 2 2 3 3)))
 '((1 2 3)))

(test-equal? "stuttero-fills-in-pieces"
 (run 1 (q)
   (fresh (a b c d)
     (== q `(,a ,b ,c ,d))
     (stuttero a `(1 ,b ,c 2 3 ,d))))
 '(((1 2 3) 1 2 3)))

(test-equal? "stuttero-fills-in-other-pieces-leaves-others-fresh"
 (run 1 (q)
   (fresh (a b c d)
     (== q `(,a ,b ,c ,d))
     (stuttero `(,b 1) `(,c . ,d))))
 '((_.0 _.1 _.1 (_.1 1 1))))

(test-equal? "stuttero-minimally-instantiates-first-answer"
 (run 1 (q)
   (fresh (e f g)
     (== q `(,e ,f ,g))
     (stuttero `(,e . ,f) g)))
 '((_.0 () (_.0 _.0))))

(test-equal? "stuttero-minimally-instantiates-first-two-answers"
 (run 2 (q)
   (fresh (e f g)
     (== q `(,e ,f ,g))
     (stuttero `(,e . ,f) g)))
 '((_.0 () (_.0 _.0)) (_.0 (_.1) (_.0 _.0 _.1 _.1))))

(test-equal? "assoco-requires-membership"
 (run* (q) (assoco 'x '() q))
 '())

(test-equal? "assoco-properly-looks-up"
 (run* (q) (assoco 'x '((x . 5)) q))
 '((x . 5)))

(test-equal? "assoco-recurs-appropriately"
 (run* (q) (assoco 'x '((y . 6) (x . 5)) q))
 '((x . 5)))

(test-equal? "assoco-returns-first-element-only"
 (run* (q) (assoco 'x '((x . 6) (x . 5)) q))
 '((x . 6)))

(test-equal? "assoco-runs-in-check-mode"
 (run* (q) (assoco 'x '((x . 5)) '(x . 5)))
 '(_.0))

(test-equal? "assoco-recurs-in-check-mode"
 (run* (q) (assoco 'x '((x . 6) (x . 5)) '(x . 6)))
 '(_.0))

(test-equal? "shadowing-makes-assoco-fail"
 (run* (q) (assoco 'x '((x . 6) (x . 5)) '(x . 5)))
 '())

(test-equal? "irrelevant-what-follows-successful-lookup"
 (run* (q) (assoco 'x '((x . 6) . ,q) '(x . 6)))
 '(_.0))

(test-equal? "generate-some-alists-with-a-given-pair"
 (run 4 (q) (assoco 'x q '(x . 5)))
 '(((x . 5) . _.0)
   (((_.0 . _.1) (x . 5) . _.2) (=/= ((_.0 x))))
   (((_.0 . _.1) (_.2 . _.3) (x . 5) . _.4) (=/= ((_.0 x)) ((_.2 x))))
   (((_.0 . _.1) (_.2 . _.3) (_.4 . _.5) (x . 5) . _.6)
    (=/= ((_.0 x)) ((_.2 x)) ((_.4 x))))))

(test-equal? "reverseo empty list"
 (run* (q) (reverseo '() q))
 '(()))

(test-equal? "reverseo singleton list"
 (run* (q) (reverseo '(a) q))
 '((a)))

(test-equal? "reverseo non-empty, non-singleton list"
 (run* (q) (reverseo '(a b c d) q))
 '((d c b a)))

(test-equal? "reverse with fresh variable element"
 (run* (q) (fresh (x) (reverseo `(a b ,x c d) q)))
 '((d c _.0 b a)))

(test-equal? "solve reverse with fresh element"
 (run* (x) (reverseo `(a b ,x d) '(d c b a)))
 '(c))

(test-equal? "solve reverse with tail fresh"
 (run* (x) (reverseo `(a b c d) `(d . ,x)))
 '((c b a)))

(test-equal? "reverseo knows about tree structure"
 (run* (q) (fresh (x) (reverseo `(a b c d) `(d . (,q . ,x)))))
 '(c))

(test-equal? "generating some reverseoed lists"
 (run 10 (q) (fresh (x y) (reverseo x y) (== `(,x ,y) q)))
 '((() ())
   ((_.0) (_.0)) ((_.0 _.1) (_.1 _.0))
   ((_.0 _.1 _.2) (_.2 _.1 _.0))
   ((_.0 _.1 _.2 _.3) (_.3 _.2 _.1 _.0))
   ((_.0 _.1 _.2 _.3 _.4) (_.4 _.3 _.2 _.1 _.0))
   ((_.0 _.1 _.2 _.3 _.4 _.5) (_.5 _.4 _.3 _.2 _.1 _.0))
   ((_.0 _.1 _.2 _.3 _.4 _.5 _.6)
    (_.6 _.5 _.4 _.3 _.2 _.1 _.0))
   ((_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7)
    (_.7 _.6 _.5 _.4 _.3 _.2 _.1 _.0))
   ((_.0 _.1 _.2 _.3 _.4 _.5 _.6 _.7 _.8)
    (_.8 _.7 _.6 _.5 _.4 _.3 _.2 _.1 _.0))))


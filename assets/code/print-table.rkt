#lang racket
(require left-pad)
(define bool-size 3)

;; A file to pretty-print list-of-list truth tables a la acl2

;; If this file does not work for you, make sure you are in the
;; regular Racket language, and that you have installed the left-pad
;; package via the package manager.

(define (build-header ls)
  (map (lambda (e)
         (max bool-size (string-length (format "~a" e))))
       ls))

(define (pprint-table lol)
  (let ((head (build-header (first lol))))
    (let ((rows (map (lambda (ls)
                       (let ((strs-row (map
                                        (lambda (e h)
                                          (left-pad (format "~a" e) h))
                                        ls
                                        head)))
                         (apply string-append (add-between strs-row '(" ")
                                                           #:before-first '("(")
                                                           #:after-last '(")")
                                                           #:splice? #t))))
                     lol)))
      (printf (apply string-append (add-between rows '("\n  ")
                                                #:before-first '("'(")
                                                #:after-last '(")")
                                                #:splice? #t))))))


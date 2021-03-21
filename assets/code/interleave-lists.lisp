(definec interleave1 (l1 :tl l2 :tl) :tl
  (if (endp l1)
      l2
    (cons (car l1) (interleave1 l2 (cdr l1)))))

(definec interleave2 (l1 :tl l2 :tl) :tl
  (if (endp l1)
      l2
    (if (endp l2)
	l1
    (cons (car l1) (cons (car l2) (interleave2 (cdr l1) (cdr l2)))))))


(thm (implies (and (tlp x) (tlp y))
       (equal (interleave1 x y) (interleave2 x y))))

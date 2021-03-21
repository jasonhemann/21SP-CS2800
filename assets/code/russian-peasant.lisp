(definec multiply2 (x :nat y :nat) :nat
  (if (zp x)
      0
    (if (evenp x)
	(* 2 (multiply2 (floor x 2) y))
      (+ y (* 2 (multiply2 (floor x 2) y))))))

(definec multiply (x :nat y :nat) :nat
   (if (zp x)
       0
     (if (evenp x)
	 (multiply (floor x 2) (* y 2))
       (+ y (multiply (floor x 2) (* y 2))))))

(defthm equivalent-algorithms2
 (implies (and (natp x) (natp y))
   (equal (multiply x y) (* x y))))

(defthm also-equivalent-algorithms2
 (implies (and (natp x) (natp y))
   (equal (multiply2 x y) (* x y))))

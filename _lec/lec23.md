---
title: Reasoning with accumulators
date: 2021-03-15
---

## With accumulators 

### Scylla and Charybdis

We have one definition that is simple, but inefficient. We also have
another function definition that is more complex, but efficient.

```lisp
(definec plus (n :nat m :nat) :nat
  (cond
    ((zp m) n)
	(t (1+ (plus n (1- m))))))
```

```lisp
(definec plus (n :nat m :nat) :nat
  (cond
    ((zp m) n)
	(t (plus (1+ n) (1- m)))))
```

## How should we proceed? 

## Where have we seen similar situations before? 


```lisp
 (definec app2 (x :tl y :tl) :tl
	(if (endp x)
		y
	  (cons (first x) (app2 (rest x) y))))

  (definec rev2 (x :tl) :tl
	(if (endp x)
		x
	  (app2 (rev2 (cdr x)) (list (car x)))

  
  (definec rev/acc (x :tl acc :tl) :tl
	(if (endp x) 
	    acc
	    (rev/acc (cdr x) (cons (car x) acc))))



  (definec rev (x :tl) :tl 
    (rev/acc x '()))
```




```lisp
(definec reverse (ls)
  (cond
	[(null? ls) '()] // endp
	[t
	 (if (null? (cdr ls))
	 ls
	 (let ((rd (reverse (cdr ls))))
	   (cons (car rd) (reverse (cons (car ls) (reverse (cdr rd)))))))]))
```


## Predef Functions

```lisp
(definec in (a :rational L :lor) :bool
  (and (consp L)
       (or (== a (car l))
	       (in a (cdr L)))))

(definec del (a :rational L :lor) :lor
  (cond 
    ((endp L) L)
    ((== a (car L)) (cdr L))
    (t (cons (car L) (del a (cdr L))))))
	
(definec permp (x :lor y :lor) :bool
  (if (endp x) (endp y)
	  (and (in (car x) y)
	       (permp (cdr x) 
		   (del (car x) y)))))

 (definec app2 (x :tl y :tl) :tl
	(if (endp x)
		y
	  (cons (first x) (app2 (rest x) y))))

  (definec rev2 (x :tl) :tl
	(if (endp x)
		x
	  (app2 (rev2 (cdr x)) (list (car x)))))
```

#lang eopl

(provide test-for-exer8.13)

(define test-for-exer8.13
  '(
    (represent-k-as-k*5+3
     "module myint
 interface
  [opaque t
   zero : t
   succ : (t -> t)
   pred : (t -> t)
   z? : (t -> bool)]
 body
  [type t = int
   zero = 3
   succ = proc (x : t) -(x, -5)
   pred = proc (x : t) -(x, 5)
   z? = proc (x : t) zero? -(x, 3)]
 let a = (from myint take succ from myint take zero)
 in (from myint take z? (from myint take pred a))
"
     bool #t)
    ))

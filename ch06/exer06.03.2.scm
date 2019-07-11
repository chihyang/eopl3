#lang eopl
(require rackunit)

;;; 2
(define f (lambda (v1 v2) (+ v1 v2)))
(define f/k (lambda (v1 v2 c) (c (+ v1 v2))))
(define g (lambda (v1 v2) (- v1 v2)))
(define g/k (lambda (v1 v2 c) (c (- v1 v2))))
(define u 1)
(define v 2)
(define x 3)
(define y 4)

(check-eqv?
 ((lambda (x y u v) (+ 1 (f (g x y) (+ u v)))) x y u v)
 ((lambda (x y u v cont)
    (g/k x y (lambda (v1)
               (f/k v1 (+ u v) (lambda (v2)
                                 (cont (+ 1 v2)))))))
  x
  y
  u
  v
  (lambda (v) v)))

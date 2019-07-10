#lang eopl
(require rackunit)

;;; 6
(define p (lambda (v1 v2) (+ v1 v2)))
(define p/k (lambda (v1 v2 c) (c (+ v1 v2))))
(define q (lambda (v1) (- v1 1)))
(define q/k (lambda (v1 c) (c (- v1 1))))
(define x 3)
(define y 4)

(check-eqv?
 ((lambda (x y) (p (+ 8 x) (q y))) x y)
 ((lambda (x y cont)
    (q/k y (lambda (v1)
             (p/k (+ 8 x) v1 cont))))
  x
  y
  (lambda (v) v)))

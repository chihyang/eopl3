#lang eopl
(require rackunit)

;;; 6
(define p (lambda (v) (+ v 1)))
(define p/k (lambda (v c) (c (+ v 1))))
(check-eqv?
 (let ((x (let ((y 8)) (p y)))) x)
 ((lambda (y)
    (p/k y (lambda (val)
             ((lambda (x) x) val))))
  8))

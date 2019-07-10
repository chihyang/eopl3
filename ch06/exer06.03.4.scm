#lang eopl
(require rackunit)

;;; 6
(define p (lambda (v) (+ v 1)))
(define p/k (lambda (v c) (c (+ v 1))))
(define a 1)
(define x -1)
(define y 1)
(check-eqv?
 (zero? (if a (p x) (p y)))
 (if a
     (p/k x (lambda (v) (zero? v)))
     (p/k y (lambda (v) (zero? v)))))

#lang eopl
(require rackunit)

;;; 7
(define p (lambda (v) (+ v 1)))
(define p/k (lambda (v c) (c (+ v 1))))
(define a #t)
(define x 2)
(define y 3)
(check-eqv?
 (let ((x (if a (p x) (p y)))) x)
 (if a
     (p/k x (lambda (val)
              ((lambda (x) x) val)))
     (p/k y (lambda (val)
              ((lambda (x) x) val)))))
(set! a #f)
(check-eqv?
 (let ((x (if a (p x) (p y)))) x)
 (if a
     (p/k x (lambda (val)
              ((lambda (x) x) val)))
     (p/k y (lambda (val)
              ((lambda (x) x) val)))))

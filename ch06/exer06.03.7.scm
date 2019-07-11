#lang eopl
(require rackunit)

;;; 7
;; tips:
;; (let ((x (if a (p x) (p y)))) x)
;; =>
;; ((lambda (x) x) (if a (p x) (p y)))
(define p (lambda (v) (+ v 1)))
(define p/k (lambda (v c) (c (+ v 1))))
(define a #t)
(define x 2)
(define y 3)
(check-eqv?
 (let ((x (if a (p x) (p y)))) x)
 (if a
    (p/k x
         (lambda (v)
           ((lambda (x cont) (cont x))
            v
            (lambda (v) v))))
    (p/k y
         (lambda (v)
           ((lambda (x cont) (cont x))
            v
            (lambda (v) v))))))
(set! a #f)
(check-eqv?
 (let ((x (if a (p x) (p y)))) x)
 (if a
    (p/k x
         (lambda (v)
           ((lambda (x cont) (cont x))
            v
            (lambda (v) v))))
    (p/k y
         (lambda (v)
           ((lambda (x cont) (cont x))
            v
            (lambda (v) v))))))

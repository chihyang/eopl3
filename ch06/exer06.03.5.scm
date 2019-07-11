#lang eopl
(require rackunit)

;;; 5
(define f (lambda (v) (- v 1)))
(define f/k (lambda (v c) (c (- v 1))))
(define p (lambda (v) (+ v 1)))
(define p/k (lambda (v c) (c (+ v 1))))
(define a 1)
(define x -1)
(define y 1)
(check-eqv?
 (zero? (if (f a) (p x) (p y)))
 (f/k a
      (lambda (v1)
        (if v1
            (p/k x (lambda (v2) (zero? v2)))
            (p/k y (lambda (v2) (zero? v2)))))))

(set! f (lambda (v) #f))
(set! f/k (lambda (v c) (c #f)))
(check-eqv?
 (zero? (if (f a) (p x) (p y)))
 (f/k a
      (lambda (v1)
        (if v1
            (p/k x (lambda (v2) (zero? v2)))
            (p/k y (lambda (v2) (zero? v2)))))))

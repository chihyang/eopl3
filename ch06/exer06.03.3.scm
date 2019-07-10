#lang eopl
(require rackunit)

;;; 6
(define f (lambda (v1 v2) (+ v1 v2)))
(define f/k (lambda (v1 v2 c) (c (+ v1 v2))))
(define g (lambda (v1 v2) (- v1 v2)))
(define g/k (lambda (v1 v2 c) (c (- v1 v2))))
(define h (lambda (v) (* v 1)))
(define h/k (lambda (v c) (c (* v 1))))
(define u 1)
(define v 2)
(define x 3)
(define y 4)

(check-eqv?
 (+ 1 (f (g x y) (+ u (h v))))
 (g/k x y (lambda (v1)
            (h/k v (lambda (v2)
                     (f/k v1 (+ u v2) (lambda (v3)
                                        (+ 1 v3))))))))

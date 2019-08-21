#lang eopl

(define mul (lambda (v1 v2) (* v1 v2)))

(define fact1
  (lambda (n cont)
    (if (zero? n)
        (cont 1)
        (fact1 (- n 1)
               (lambda (v)
                 (cont (mul n v)))))))

(define fact2
  (lambda (n cont)
    (if (zero? n)
        cont
        (fact2 (- n 1) (mul n cont)))))

(trace fact1 fact2 mul)

(fact1 3 (lambda (v) v))
(fact2 3 1)

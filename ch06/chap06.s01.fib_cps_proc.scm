#lang eopl

;;; cps with procedure inlined
(define fib
  (lambda (n)
    (fib/k n (end-cont))))

(define fib/k
  (lambda (n cont)
    (if (< n 2)
        (apply-cont cont 1)
        (fib/k (- n 1) (fib1-cont n cont)))))

(define apply-cont
  (lambda (cont val)
    (cont val)))

(define end-cont
  (lambda ()
    (lambda (val) val)))

(define fib1-cont
  (lambda (n saved-cont)
    (lambda (val)
      (fib/k (- n 2) (fib2-cont val saved-cont)))))

(define fib2-cont
  (lambda (saved-val saved-cont)
    (lambda (val)
      (apply-cont saved-cont (+ saved-val val)))))

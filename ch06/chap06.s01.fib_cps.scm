#lang eopl

;;; cps with procedure inlined
(define fib
  (lambda (n)
    (if (< n 2)
        1
        (+ (fib (- n 1))
           (fib (- n 2))))))

(define fib/k
  (lambda (n cont)
    (if (< n 2)
        (apply-cont cont 1)
        (fib/k (- n 1) (fib1-cont n cont)))))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
           (end-cont
            ()
            val)
           (fib1-cont
            (n saved-cont)
            (fib/k (- n 2) (fib2-cont val saved-cont)))
           (fib2-cont
            (saved-val saved-cont)
            (apply-cont saved-cont (+ saved-val val))))))

(define-datatype continuation continuation?
  (end-cont)
  (fib1-cont
   (n integer?)
   (saved-cont continuation?))
  (fib2-cont
   (saved-val integer?)
   (saved-cont continuation?)))

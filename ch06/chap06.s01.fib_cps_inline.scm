#lang eopl

;;; cps with procedure inlined
(define fib
  (lambda (n)
    (fib/k n (lambda (val) val))))

(define fib/k
  (lambda (n cont)
    (if (< n 2)
        (cont 1)
        (fib/k (- n 1)
               (lambda (saved-val)
                 (fib/k (- n 2)
                        (lambda (val)
                          (cont (+ saved-val val)))))))))

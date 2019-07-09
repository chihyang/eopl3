#lang eopl

;;; cps with data type
(define fact
  (lambda (n)
    (fact/k n (end-cont))))

(define fact/k
  (lambda (n cont)
    (if (zero? n)
        (apply-cont cont 1)
        (fact/k (- n 1) (fact1-cont n cont)))))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
           (end-cont
            ()
            val)
           (fact1-cont
            (n saved-cont)
            (apply-cont saved-cont (* n val))))))

(define-datatype continuation continuation?
  (end-cont)
  (fact1-cont
   (n integer?)
   (cont continuation?)))

#lang eopl

;;; cps with procedure
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
    (cont val)))

(define end-cont
  (lambda ()
    (lambda (val)
      val)))

(define fact1-cont
  (lambda (n cont)
    (lambda (val)
      (cont (* n val)))))

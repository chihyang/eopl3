#lang eopl

;;; trampoline
(define fact
  (lambda (n)
    (trampoline (fact/k n (end-cont)))))

(define fact/k
  (lambda (n cont)
    (if (zero? n)
        (lambda () (apply-cont cont 1))
        (lambda ()
          (fact/k (- n 1) (fact1-cont n cont))))))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
           (end-cont
            ()
            val)
           (fact1-cont
            (n saved-cont)
            (lambda () (apply-cont saved-cont (* n val)))))))

(define-datatype continuation continuation?
  (end-cont)
  (fact1-cont
   (n integer?)
   (cont continuation?)))

(define trampoline
  (lambda (bc)
    (if (integer? bc)
        bc
        (trampoline (bc)))))

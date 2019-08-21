#lang eopl

(define n 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define pc 'uninitialized)

(define fact
  (lambda (num)
    (set! cont (end-cont))
    (set! n num)
    (set! pc fact/k)
    (trampoline)
    val))

;;; without (set! pc fact/k) in fact/k and (set! pc apply-cont) in apply-cont
;;; the program still works because pc register has the value that could be used
;;; again
(define fact/k
  (lambda ()
    (if (zero? n)
        (begin
          (set! val 1)
          (set! pc apply-cont))
        (begin
          (set! cont (fact1-cont n cont))
          (set! n (- n 1))))))

(define apply-cont
  (lambda ()
    (cases continuation cont
           (end-cont
            ()
            (set! pc #f))
           (fact1-cont
            (n saved-cont)
            (set! cont saved-cont)
            (set! val (* n val))))))

(define-datatype continuation continuation?
  (end-cont)
  (fact1-cont
   (n integer?)
   (cont continuation?)))

(define trampoline
  (lambda ()
    (when (procedure? pc)
      (pc)
      (trampoline))))

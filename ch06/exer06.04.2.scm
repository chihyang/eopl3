#lang eopl

;; list-sum : Listof(Int) → Int
(define list-sum
  (lambda (loi k)
    (if (null? loi)
        (k 0)
        (list-sum (cdr loi)
                  (lambda (v)
                    (k (+ (car loi) v)))))))

(define end-cont
  (lambda ()
    (lambda (val)
      (eopl:printf "End of computation.~%")
      (eopl:printf "This sentence should appear only once.~%")
      val)))

(require rackunit)
(check-equal?
 (list-sum '() (end-cont))
 0)

(check-equal?
 (list-sum '(1 2 3 4 5) (end-cont))
 15)

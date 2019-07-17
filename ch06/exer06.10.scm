#lang eopl

;; list-sum : Listof(Int) → Int
(define list-sum
  (lambda (loi v)
    (if (null? loi)
        v
        (list-sum (cdr loi)
                  (+ (car loi) v)))))

(require rackunit)
(check-equal?
 (list-sum '() 0)
 0)

(check-equal?
 (list-sum '(1 2 3 4 5) 0)
 15)

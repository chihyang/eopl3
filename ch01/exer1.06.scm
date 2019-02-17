#lang eopl
(define nth-element-reversed-order
  (lambda (lst n)
    (if (eq? n 0)
        (report-list-too-short n)
        (if (null? lst)
            (car lst)
            (nth-element-reversed-order (cdr lst) (- n 1))))))
(define report-list-too-short
  (lambda (n)
    (eopl:error 'nth-element
           "List too short by ~s elements.~%" (+ n 1))))
(nth-element-reversed-order '(a b c d e) 8)

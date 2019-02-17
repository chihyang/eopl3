#lang eopl
(define nth-element-informative
  (lambda (lst n)
    (nth-element-informative-iter lst n '() 0)))

(define nth-element-informative-iter
  (lambda (lst n acc-lst acc-n)
    (if (null? lst)
        (report-list-too-short-informative (append acc-lst lst) (+ n acc-n))
        (if (eq? n 0)
            (car lst)
            (nth-element-informative-iter
             (cdr lst)
             (- n 1)
             (append acc-lst (list (car lst)))
             (+ acc-n 1))))))

(define report-list-too-short-informative
  (lambda (lst n)
    (eopl:error 'nth-element-informative
           "List ~a does not have ~s element(s).~%" lst (+ n 1))))

#lang eopl
;;; constructor
(define empty-stack
  (lambda () '()))
(define push
  (lambda (val stack)
    (cons val stack)))
(define pop
  (lambda (stack)
    (if (null? stack)
        (empty-stack)
        (cdr stack))))
;;; observer
(define top
  (lambda (stack)
    (if (null? stack)
        (empty-stack)
        (car stack))))
(define empty-stack?
  (lambda (stack)
    (null? stack)))

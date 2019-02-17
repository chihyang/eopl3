#lang eopl
;;; invert : Listof(2-List) -> Listof(2-List)
;;;
;;; usage: return a list of 2-lists with each 2-list reversed
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (invert-2-lst (car lst))
              (invert (cdr lst))))))

;;; invert-2-lst : 2-List -> 2-List
;;;
;;; usage: return a 2-list with elements in 2-lst reversed
(define invert-2-lst
  (lambda (2-lst)
    (list (cadr 2-lst) (car 2-lst))))

(invert '((a 1) (a 2) (1 b) (2 b)))
(invert '((a 1)))
(invert '())

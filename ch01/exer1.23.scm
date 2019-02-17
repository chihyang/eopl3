#lang eopl
;;; list-index : pred x list -> Int | #f
;;;
;;; usage: return the 0-based position of the first element of lst that
;;; satisfies the predicate pred. If no element of lst satisfies the predicate,
;;; then returns #f
(define list-index
  (lambda (pred lst)
    (list-index-iter pred lst 0)))
;;; list-index-iter : pred x list x Int -> Int | #f
;;;
;;; usage: return the n-based position of the first element of lst that
;;; satisfies the predicate pred. If no element of lst satisfies the predicate,
;;; then returns #f
(define list-index-iter
  (lambda (pred lst n)
    (if (null? lst)
        #f
        (if (pred (car lst))
            n
            (list-index-iter pred (cdr lst) (+ n 1))))))

(equal? (list-index number? '(a 2 (1 3) b 7))
        1)
(equal? (list-index symbol?  '(a (b c) 17 foo))
        0)
(equal? (list-index symbol?  '(1 2 (a b) 3))
        #f)

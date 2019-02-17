#lang eopl
;;; up : list -> list
;;;
;;; usage: removes a pair of parentheses from each top-level element of lst
(define up
  (lambda (lst)
    (if (null? lst)
        '()
        (if (list? (car lst))
            (append (car lst)
                    (up (cdr lst)))
            (cons (car lst)
                  (up (cdr lst)))))))

(equal? (up '((1 2) (3 4)))
        '(1 2 3 4))
(equal? (up '((x (y)) z))
        '(x (y) z))
(equal? (up '(2))
        '(2))
(equal? (up '(2 ()))
        '(2))

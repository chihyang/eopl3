#lang eopl
;;; flatten : slist -> slist
;;;
;;; usage: removes all of parentheses from inside of slist
(define flatten
  (lambda (slist)
    (if (null? slist)
        '()
        (if (list? (car slist))
            (append (flatten (car slist))
                    (flatten (cdr slist)))
            (cons (car slist)
                  (flatten (cdr slist)))))))

(equal? (flatten '(a b c))
        '(a b c))
(equal? (flatten '((a) () (b ()) () (c)))
        '(a b c))
(equal? (flatten '((a b) c (((d)) e)))
        '(a b c d e))
(equal? (flatten '((((((((((()))))))))) c (((d)) e)))
        '(c d e))
(equal? (flatten '((((((((((((((((((((())))))))))))))))))))))
        '())

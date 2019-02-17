#lang eopl
;;; every? : pred x list -> #t | #f
;;;
;;; usage: return #f if any element of lst fails to satisfy pred, and returns #t
;;; otherwise
(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (if (pred (car lst))
            (every? pred (cdr lst))
            #f))))

(equal? (every? number? '(a b c 3 e))
        #f)
(equal? (every? number? '(1 2 3 4 5))
        #t)
(equal? (every? null? '(1 2 (a b) 3))
        #f)

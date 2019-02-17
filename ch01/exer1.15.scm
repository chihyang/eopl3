#lang eopl
;;; duple : Int x SchemeVal -> Listof(SchemeVal)
;;;
;;; usage: return a list of n x
(define duple
  (lambda (n x)
    (if (eq? n 0)
        '()
        (cons x (duple (- n 1) x)))))
(duple 2 3)
(duple 4 '(ha ha))
(duple 0 '(blah))
(duple 1 '(blah))
(duple 3 '(blah (a b c)))
(duple 1 '())

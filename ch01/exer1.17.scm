#lang eopl
;;; down : Listof(SchemeVal) -> Listof(SchemeVal)
;;;
;;; usage: return a list that wraps parentheses around each top-level element of
;;; lst.
(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (car lst))
              (down (cdr lst))))))

(down '(1 2 3))
(down '((a) (fine) (idea)))
(down '(a (more (complicated)) object))

(down '((a 1) (a 2) (1 b) (2 b)))
(invert '((a 1)))
(invert '())

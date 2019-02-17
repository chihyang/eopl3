#lang eopl
;; remove-before-first : Sym x Listof(Sym) -> Listof(Sym)

;; usage: (remove-before-first s los) return a list with the same elements
;; arranged in the same order as los, except that the first occurrence of s and
;; the elements before it are removed, if s is not in los, empty list would be
;; returned

(define remove-before-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eq? s (car los))
            (cdr los)
            (remove-before-first s (cdr los))))))

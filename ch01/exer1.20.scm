#lang eopl
;;; count-occurrences : symbol x s-list -> Int
;;;
;;; usage: return the number of occurrence of s in slist
(define count-occurrences
  (lambda (s slist)
    (cond ((null? slist) 0)
          ((eq? s (car slist))
           (+ 1 (count-occurrences s (cdr slist))))
          ((symbol? (car slist))
           (count-occurrences s (cdr slist)))
          (else
           (+ (count-occurrences s (car slist))
              (count-occurrences s (cdr slist)))))))

(eq? (count-occurrences 'x '((f x) y (((x z) x))))
     3)
(eq? (count-occurrences 'x '((f x) y (((x z) () x))))
     3)
(eq? (count-occurrences 'w '((f x) y (((x z) x))))
     0)

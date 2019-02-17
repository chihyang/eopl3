#lang eopl
;;; swapper : s-list -> s-list
;;;
;;; usage: return a list with every occurrence of s1 in slist replaced by s2 and
;;; every occurrence of s2 in slist replaced by s1
(define swapper
  (lambda (s1 s2 slist)
    (cond ((null? slist) '())
          ((eq? s1 (car slist))
           (cons s2 (swapper s1 s2 (cdr slist))))
          ((eq? s2 (car slist))
           (cons s1 (swapper s1 s2 (cdr slist))))
          ((symbol? (car slist))
           (cons (car slist) (swapper s1 s2 (cdr slist))))
          (else
           (cons (swapper s1 s2 (car slist))
                 (swapper s1 s2 (cdr slist)))))))

(equal? (swapper 'a 'd '(a b c d))
        '(d b c a))
(equal? (swapper 'a 'd '(a d () c d))
        '(d a () c a))
(equal? (swapper 'x 'y '((x) y (z (x))))
        '((y) x (z (y))))

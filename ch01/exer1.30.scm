#lang eopl
;;; sort/predicate : pred x Listof(Int) -> Listof(Int)
;;;
;;; usage: return a list of the elements of loi sorted by pred
(define sort/predicate
  (lambda (pred loi)
    (sort-iter/predicate pred '() loi)))

;;; sort-iter : pred x Listof(Int) x Listof(Int) -> Listof(Int)
;;;
;;; usage: given a list loi1 sorted by predicate and any of a list of integers
;;; loi2, return a list of all the elements of loi1 and loi2 sorted by pred
(define sort-iter/predicate
  (lambda (pred loi1 loi2)
    (if (null? loi2)
        loi1
        (sort-iter/predicate
         pred
         (merge/predicate pred loi1 (list (car loi2)))
         (cdr loi2)))))

;;; merge : pred x Listof(Int) x Listof(Int) -> Listof(Int)
;;;
;;; usage: return a list of all integers in loi1 and loi2 sorted by pred
(define merge/predicate
  (lambda (pred loi1 loi2)
    (cond ((null? loi2)
           loi1)
          ((null? loi1)
           loi2)
          ((pred (car loi1) (car loi2))
           (cons (car loi1)
                 (merge/predicate pred (cdr loi1) loi2)))
          (else
           (cons (car loi2)
                 (merge/predicate pred loi1 (cdr loi2)))))))

(equal? (sort/predicate < '(8 2 5 2 3))
        '(2 2 3 5 8))
(equal? (sort/predicate > '(8 2 5 2 3))
        '(8 5 3 2 2))

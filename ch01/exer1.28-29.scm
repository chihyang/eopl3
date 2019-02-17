#lang eopl
;;; ------------------------------ exer1.28 ------------------------------
;; merge : Ordered Listof(Int) x Ordered Listof(Int) -> Ordered Listof(Int)
;;
;; usage: return a sorted list of all integers in loi1 and loi2
(define merge
  (lambda (loi1 loi2)
    (cond ((null? loi2)
           loi1)
          ((null? loi1)
           loi2)
          ((< (car loi1) (car loi2))
           (cons (car loi1)
                 (merge (cdr loi1) loi2)))
          (else
           (cons (car loi2)
                 (merge loi1 (cdr loi2)))))))

(equal? (merge '(1 4) '(2 8))
        '(1 2 4 8))
(equal? (merge '(35 62 81 90 91) '(3 83 85 90))
        '(3 35 62 81 83 85 90 90 91))
(equal? (merge '() '(1 2 3))
        '(1 2 3))

;;; ------------------------------ exer1.29 ------------------------------
;; sort : Listof(Int) -> Listof(Int)
;;
;; usage: return a list of the elements of loi in ascending order
(define sort
  (lambda (loi)
    (sort-iter '() loi)))

;; sort-iter : Listof(Int) x Listof(Int) -> Listof(Int)
;;
;; usage: given an ascending ordered list loi1 and a list of integers loi2,
;; return a list of all the elements of loi1 and loi2 in ascending order
(define sort-iter
  (lambda (loi1 loi2)
    (if (null? loi2)
        loi1
        (sort-iter (merge loi1 (list (car loi2)))
                   (cdr loi2)))))

(equal? (sort '(8 2 5 2 3))
        '(2 2 3 5 8))

;; way to think of sort-iter
;; (merge '() '(4)) '(2 1 3)
;; (merge '(4) '(2)) '(1 3)
;; (merge '(2 4) '(1)) '(3)
;; (merge '(1 2 4)) '(3)
;; (merge '(1 2 3 4))

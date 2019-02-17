#lang eopl
;;; product : s-list x s-list -> Listof(2-list)
;;;
;;; usage: return the Cartesian product of symbol list sos1 and sos2
(define product
  (lambda (sos1 sos2)
    (cond ((or (null? sos1) (null? sos2))
           '())
          (else
           (append (s-product (car sos1) sos2)
                   (product (cdr sos1) sos2))))))

;;; s-product : symbol x s-list -> Listof(2-list)
;;;
;;; usage: return the Cartesian product of symbol s and list sos
(define s-product
  (lambda (s sos)
    (if (null? sos)
        '()
        (cons (list s (car sos))
              (s-product s (cdr sos))))))

(equal? (product '(a b c) '(x y))
        '((a x) (a y) (b x) (b y) (c x) (c y)))
(equal? (product '(a b c) '())
        '())
(equal? (product '() '(x y))
        '())

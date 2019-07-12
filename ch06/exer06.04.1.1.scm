#lang eopl

(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eq? s (car los))
            (cdr los)
            (cons (car los)
                  (remove-first s (cdr los)))))))

(define remove-first/k
  (lambda (s los k)
    (cond ((null? los) (apply-cont k '()))
          ((= (car los) s) (apply-cont k (cdr los)))
          (else
           (remove-first/k s
                           (cdr los)
                           (remove-first1-cont (car los) k))))))

(define-datatype continuation continuation?
  (end-cont)
  (remove-first1-cont
   (saved-val integer?)
   (saved-cont continuation?)))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
           (end-cont
            ()
            (begin
              (eopl:printf "End of computation.~%")
              (eopl:printf "This sentence should appear only once.~%")
              val))
           (remove-first1-cont
            (v c)
            (apply-cont c (cons v val))))))

(require rackunit)
(check-equal?
 (remove-first/k 6 '() (end-cont))
 '())

(check-equal?
 (remove-first/k 6 '(1 2 3 4 5) (end-cont))
 '(1 2 3 4 5))

(check-equal?
 (remove-first/k 2 '(1 2 3 4 5) (end-cont))
 '(1 3 4 5))

(check-equal?
 (remove-first/k 2 '(1 2 3 2 4 5) (end-cont))
 '(1 3 2 4 5))

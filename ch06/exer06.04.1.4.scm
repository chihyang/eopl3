#lang eopl

(define remove-first
  (lambda (ele lst)
    (set! s ele)
    (set! los lst)
    (set! cont (end-cont))
    (remove-first/k)))

(define s 'uninitialized)
(define los 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define remove-first/k
  (lambda ()
    (cond ((null? los)
           (set! val '())
           (apply-cont))
          ((= (car los) s)
           (set! val (cdr los))
           (apply-cont))
          (else
           (set! cont (remove-first1-cont (car los) cont))
           (set! los (cdr los))
           (remove-first/k)))))

(define-datatype continuation continuation?
  (end-cont)
  (remove-first1-cont
   (saved-val integer?)
   (saved-cont continuation?)))

(define apply-cont
  (lambda ()
    (cases continuation cont
           (end-cont
            ()
            (begin
              (eopl:printf "End of computation.~%")
              (eopl:printf "This sentence should appear only once.~%")
              val))
           (remove-first1-cont
            (v c)
            (set! cont c)
            (set! val (cons v val))
            (apply-cont)))))

(require rackunit)
(check-equal?
 (remove-first 6 '())
 '())

(check-equal?
 (remove-first 6 '(1 2 3 4 5))
 '(1 2 3 4 5))

(check-equal?
 (remove-first 2 '(1 2 3 4 5))
 '(1 3 4 5))

(check-equal?
 (remove-first 2 '(1 2 3 2 4 5))
 '(1 3 2 4 5))

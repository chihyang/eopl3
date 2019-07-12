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
    (cond ((null? los) (k '()))
          ((= (car los) s) (k (cdr los)))
          (else
           (remove-first/k s
                           (cdr los)
                           (lambda (val)
                             (k (cons (car los) val))))))))

(define end-cont
  (lambda ()
    (lambda (val)
      (eopl:printf "End of computation.~%")
      (eopl:printf "This sentence should appear only once.~%")
      val)))

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

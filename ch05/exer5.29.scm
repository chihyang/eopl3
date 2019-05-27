#lang eopl

;;; tail-form
(define fact-iter
  (lambda (n)
    (fact-iter-acc n 1)))
(define fact-iter-acc
  (lambda (n a)
    (if (zero? n)
        a
        (fact-iter-acc (- n 1) (* n a)))))

;;; register
(define reg-n 'uninitialized)
(define reg-a 'uninitialized)
(define fact-iter-acc-reg
  (lambda ()
    (if (zero? reg-n)
        reg-a
        (begin
          (set! reg-a (* reg-n reg-a))
          (set! reg-n (- reg-n 1))
          (fact-iter-acc-reg)))))
(define fact-iter-reg
  (lambda (m)
    (set! reg-n m)
    (set! reg-a 1)
    (fact-iter-acc-reg)))

(eqv? (fact-iter 100)
      (fact-iter-reg 100))

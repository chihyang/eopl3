#lang eopl
(provide tests-for-run tests-for-parse)

(define the-test-suite
  '(
    (bogus-odd-even-via-self
     "
class oddeven extends object
  method initialize()1
  method even(n)if zero?(n) then 1 else send self odd (-(n,1))
  method odd(n) if zero?(n) then 0 else send self even (-(n,1))
class bogus-oddeven extends oddeven
  method even(n) 0

let o1 = new bogus-oddeven() in send o1 odd(13)"
     0)
    )
  )

(define tests-for-run the-test-suite)

(define tests-for-parse
  (let loop ((lst the-test-suite))
    (cond
     ((null? lst) '())
     ((> (length (car lst)) 3)
      (cons
       (list
        (list-ref (car lst) 0)
        (list-ref (car lst) 1)
        (list-ref (car lst) 3))
       (loop (cdr lst))))
     (else
      (cons
       (list
        (list-ref (car lst) 0)
        (list-ref (car lst) 1)
        #t)
       (loop (cdr lst)))))))

#lang eopl
(provide less?-test-list)
;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

(define less?-test-list
  '(
    (fib-anf-test
     "letrec fib(x) =
        if less?(x, 2)
        then 1
        else
          let val1 = (fib -(x,1)) in
            let val2 = (fib -(x,2)) in
              +(val1, val2)
      in (fib 4)"
     5)
    (fib-test
     "letrec fib(x) =
        if less?(x, 2)
        then 1
        else +((fib -(x, 1)), (fib -(x, 2)))
      in (fib 4)"
     5)
    ))

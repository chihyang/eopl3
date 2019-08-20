#lang eopl
(provide begin-test-list)
;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

(define begin-test-list
  '(
    ;; lists
    (begin-test-1
     "begin
        letrec double (x) = if zero?(x) then 0
                            else -((double -(x,1)),-2)
        in (double 6);
        let x = 5 in -(x, 6)
      end"
     -1)
    ))

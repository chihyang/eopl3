#lang eopl
(provide list-test-list)
;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

(define list-test-list
  '(
    ;; lists
    (cons-test
     "let x = 4 in cons(x, cons(cons(-(x,1), emptylist), emptylist))"
     (4 (3)))
    (null?-test
     "null?(cdr(let x = 4 in
                 cons(x, cons(cons(-(x,1),
                                   emptylist),
                              emptylist))))"
     #f)
    (list-test
     "let x = 4 in list(x, -(x,1), -(x,3))"
     (4 3 1))
    ))

#lang eopl
(require "exer6.36.cps-in-lang.scm")
(require "exer6.36.scm")
(require "chap06.s04.cps-interp.scm")

(require rackunit)
(require "chap06.s04.cps-tests.scm")
(require "exer6.36.begin-tests.scm")

(define test-name car)
(define test-program cadr)
(define test-answer caddr)
(define passed 0)
(define failed 0)

(define tests (append test-list begin-test-list))
(for-each
 (lambda (test)
   (let ((v1 (checked-run
              (compile (scan&parse (test-program test)))))
         (v2 (test-answer test)))
     (if  (equal? v1 v2)
          (begin
            (set! passed (+ passed 1)))
          (begin
            (set! failed (+ failed 1))
            (eopl:printf "test for ~a failed: expect ~a, actual ~a~%"
                         (test-name test) v2 v1)))))
 tests)

(if (eq? passed (length tests))
    (eopl:printf "all tests passed!~%")
    (eopl:printf "~%~a tests failed!~%" failed))

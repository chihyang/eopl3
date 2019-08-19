#lang eopl
(require "chap06.s03.cps-in-lang.scm")
(require "exer06.29.scm")
(require "cps-interp.scm")

(require rackunit)
(require "cps-tests.scm")

(define test-name car)
(define test-program cadr)
(define test-answer caddr)
(define passed 0)
(define failed 0)

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
 test-list)

(if (eq? passed (length test-list))
    (eopl:printf "all tests passed!~%")
    (eopl:printf "~%~a tests failed!~%" failed))

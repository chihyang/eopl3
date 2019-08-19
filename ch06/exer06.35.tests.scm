#lang eopl
(require "exer06.34.anf-in-lang.scm")
(require "exer06.34.scm")
(require "exer06.35.scm")
(require "exer06.35.cps-interp.scm")

(require rackunit)
(require "cps-tests.scm")
(require "exer06.34.less-tests.scm")

(define test-name car)
(define test-program cadr)
(define test-answer caddr)
(define passed 0)
(define failed 0)

(define tests (append test-list less?-test-list))
(for-each
 (lambda (test)
   (let ((v1 (checked-run
              (cps-compile (compile (scan&parse (test-program test))))))
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

#lang eopl
(require "chap08.s02.lang.scm")
(require "chap08.s02.checker.scm")
(require rackunit)
(require (only-in "exer8.12-15.scm"
                  [tests-for-check tests-for-check-exer8.12-15]))
(require (only-in "chap08.s02.module-tests.scm"
                  [tests-for-check tests-for-check-s02]))
(define tests-for-check (append tests-for-check-exer8.12-15 tests-for-check-s02))

(define test-name car)
(define test-program cadr)
(define test-answer caddr)
(define passed 0)
(define failed 0)

(for-each
 (lambda (test)
   (let ((v1 (checked-type-of
              (checked-scan&parse (test-program test))))
         (v2 (test-answer test)))
     (if  (equal? v1 v2)
          (begin
            (set! passed (+ passed 1)))
          (begin
            (set! failed (+ failed 1))
            (eopl:printf "test for type checking ~a failed: expect ~a, actual ~a~%"
                         (test-name test) v2 v1)))))
 tests-for-check)

(if (eq? passed (length tests-for-check))
    (eopl:printf "all tests passed!~%")
    (eopl:printf "~%~a tests failed!~%" failed))

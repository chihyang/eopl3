#lang eopl
(require "chap09.s03.lang.scm")
(require rackunit)
(require (only-in "chap09.s03.class-tests.scm"
                  [tests-for-parse tests-for-parse-s03]
                  [tests-for-run tests-for-run-s03]
                  [tests-for-check tests-for-check-s03]))
(define tests-for-parse (append tests-for-parse-s03))
(define tests-for-run (append tests-for-run-s03))
(define tests-for-check (append tests-for-check-s03))

(define test-name car)
(define test-program cadr)
(define test-answer caddr)
(define passed 0)
(define failed 0)

(for-each
 (lambda (test)
   (let ((v1 (test-scan&parse (test-program test)))
         (v2 (test-answer test)))
     (if  (equal? v1 v2)
          (begin
            (set! passed (+ passed 1)))
          (begin
            (set! failed (+ failed 1))
            (eopl:printf "test for parsing ~a failed: expect ~a, actual ~a~%"
                         (test-name test) v2 v1)))))
 tests-for-parse)

(if (eq? passed (+ (length tests-for-parse)))
    (eopl:printf "all tests passed!~%")
    (eopl:printf "~%~a tests failed!~%" failed))

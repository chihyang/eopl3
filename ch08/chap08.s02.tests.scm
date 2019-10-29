#lang eopl
(require "chap08.s02.lang.scm")
(require "chap08.s02.interp.scm")
(require "chap08.s02.checker.scm")
(require rackunit)
(require (only-in "exer8.12-15.scm"
                  [tests-for-parse tests-for-parse-exer8.12-15]
                  [tests-for-run tests-for-run-exer8.12-15]
                  [tests-for-check tests-for-check-exer8.12-15]))
(require (only-in "chap08.s02.module-tests.scm"
                  [tests-for-parse tests-for-parse-s02]
                  [tests-for-run tests-for-run-s02]
                  [tests-for-check tests-for-check-s02]))
(define tests-for-parse (append tests-for-parse-exer8.12-15 tests-for-parse-s02))
(define tests-for-run (append tests-for-run-exer8.12-15 tests-for-run-s02))
(define tests-for-check (append tests-for-check-exer8.12-15 tests-for-check-s02))

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

(for-each
 (lambda (test)
   (let ((v1 (checked-run
              (scan&parse (test-program test))))
         (v2 (test-answer test)))
     (if  (equal? v1 v2)
          (begin
            (set! passed (+ passed 1)))
          (begin
            (set! failed (+ failed 1))
            (eopl:printf "test for ~a failed: expect ~a, actual ~a~%"
                         (test-name test) v2 v1)))))
 tests-for-run)


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

(if (eq? passed (+ (length tests-for-parse) (length tests-for-run) (length tests-for-check)))
    (eopl:printf "all tests passed!~%")
    (eopl:printf "~%~a tests failed!~%" failed))

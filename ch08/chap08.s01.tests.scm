#lang eopl
(require "chap08.s01.lang.scm")
(require "chap08.s01.interp.scm")

(require rackunit)
(require "chap08.s01.module-tests.scm")

(define test-name car)
(define test-program cadr)
(define test-answer caddr)
(define passed 0)
(define failed 0)

(for-each
 (lambda (test)
   (let ((v1 (checked-scan&parse (test-program test)))
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

(if (eq? passed (+ (length tests-for-run) (length tests-for-parse)))
    (eopl:printf "all tests passed!~%")
    (eopl:printf "~%~a tests failed!~%" failed))

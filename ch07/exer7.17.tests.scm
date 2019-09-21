#lang eopl
(require "chap07.s04.lang.scm")
(require "exer7.17.infer.scm")
(require "chap07.s04.interp.scm")

(require rackunit)
(require "chap07.s04.infer-tests.scm")

(define test-name car)
(define test-program cadr)
(define test-answer caddr)
(define passed 0)
(define failed 0)

(for-each
 (lambda (test)
   (let ((v1 (checked-run
              (scan&parse (test-program test))))
         (v2 (test-answer test)))
     (if (equal? v1 v2)
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
              (scan&parse (test-program test))))
         (v2 (test-answer test)))
     (if (and (not (equal? v1 'error))
              (not (equal? v2 'error)))
         (if (equal-types? v1 v2)
             (begin
               (set! passed (+ passed 1)))
             (begin
               (set! failed (+ failed 1))
               (eopl:printf "test for ~a failed: expect ~a, actual ~a~%"
                            (test-name test) v2 v1)))
         (if (equal? v1 v2)
             (begin
               (set! passed (+ passed 1)))
             (begin
               (set! failed (+ failed 1))
               (eopl:printf "test for ~a failed: expect ~a, actual ~a~%"
                            (test-name test) v2 v1))))))
 tests-for-check)

(if (eq? passed (+ (length tests-for-check) (length tests-for-run)))
    (eopl:printf "all tests passed!~%")
    (eopl:printf "~%~a tests failed!~%" failed))

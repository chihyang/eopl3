#lang eopl
(require "exer8.27.lang.scm")
(require rackunit)
(require (only-in "exer8.19-23.scm"
                  [tests-for-parse tests-for-parse-exer8.19-23]
                  [tests-for-run tests-for-run-exer8.19-23]
                  [tests-for-check tests-for-check-exer8.19-23]))
(require (only-in "chap08.s03.module-tests.scm"
                  [tests-for-parse tests-for-parse-s03]
                  [tests-for-run tests-for-run-s03]
                  [tests-for-check tests-for-check-s03]))
(require (only-in "exer8.27.module-tests.scm"
                  [tests-for-parse tests-for-parse-exer8.27]
                  [tests-for-run tests-for-run-exer8.27]
                  [tests-for-check tests-for-check-exer8.27]))

(define tests-for-parse (append tests-for-parse-exer8.19-23
                                tests-for-parse-s03
                                tests-for-parse-exer8.27))
(define tests-for-run (append tests-for-run-exer8.19-23
                              tests-for-run-s03
                              tests-for-run-exer8.27))
(define tests-for-check (append tests-for-check-exer8.19-23
                                tests-for-check-s03
                                tests-for-check-exer8.27))

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

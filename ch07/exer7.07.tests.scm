#lang eopl
(require "exer7.05.lang.scm")
(require (only-in "exer7.05.checker.scm"
                  [type-of-program type-of-program-e5]))
(require (only-in "exer7.07.checker.scm"
                  [type-of-program type-of-program-e7]))
(require (only-in racket/base with-handlers exn:fail? exn-message))
(require "exer7.07.checked-tests.scm")
(require rackunit)

(define test-name car)
(define test-program cadr)
(define test-answer caddr)

(with-handlers
    [(exn:fail? (lambda (en) (eopl:printf "~a~%~%" (exn-message en))))]
  (type-of-program-e5 (scan&parse (test-program tests-for-check))))

(with-handlers
    [(exn:fail? (lambda (en) (eopl:printf "~a~%~%" (exn-message en))))]
  (type-of-program-e7 (scan&parse (test-program tests-for-check))))

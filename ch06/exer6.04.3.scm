#lang eopl

(require "pmatch.scm")

;; occurs-free? : Sym × LcExp x Cont → Bool
(define occurs-free?
  (lambda (search-var exp k)
    (pmatch exp
      [`,var
       (guard (symbol? var))
       (k (eqv? search-var var))]
      [`(lambda (,bound-var) ,body)
       (occurs-free? search-var
                     body
                     (lambda (v)
                       (k (and v (not (eqv? search-var bound-var))))))]
      [`(,rator ,rand)
       (occurs-free? search-var
                          rator
                          (lambda (v1)
                            (occurs-free? search-var rand
                                          (lambda (v2)
                                            (k (or v1 v2))))))])))

(define end-cont
  (lambda ()
    (lambda (val)
      (eopl:printf "End of computation.~%")
      (eopl:printf "This sentence should appear only once.~%")
      val)))

(require rackunit)
(check-equal?
 (occurs-free? 'x 'y (end-cont))
 #f)

(check-equal?
 (occurs-free? 'x 'x (end-cont))
 #t)

(check-equal?
 (occurs-free? 'x '(lambda (x) y) (end-cont))
 #f)

(check-equal?
 (occurs-free? 'x '(lambda (y) x) (end-cont))
 #t)

(check-equal?
 (occurs-free? 'x '((lambda (x) y) x) (end-cont))
 #t)

(check-equal?
 (occurs-free? 'x '((lambda (y) x) y) (end-cont))
 #t)

#lang eopl
(define-datatype lc-exp lc-exp?
  (var-exp
   (var lc-identifier?))
  (lambda-exp
   (bound-var lc-identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))
(define lc-identifier?
  (lambda (var)
    (and (symbol? var)
         (not (eqv? var 'lambda)))))

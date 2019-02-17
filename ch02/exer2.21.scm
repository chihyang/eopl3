#lang eopl
(define-datatype env env?
  (empty-env)
  (extend-env
   (id identifier?)
   (val val?)
   (sub-env env?)))
(define has-binding?
  (lambda (env-exp s)
    (cases env env-exp
           (empty-env
            #f)
           (extend-env
            (id val sub-env)
            (or (eqv? id s)
                (has-binding? sub-env s))))))

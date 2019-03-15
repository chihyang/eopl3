#lang eopl
(define identifier? symbol?)
(define val? number?)
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
            ()
            #f)
           (extend-env
            (id val sub-env)
            (or (eqv? id s)
                (has-binding? sub-env s))))))
;;; ---------------------- Test ----------------------
(eqv?
 (has-binding?
   (empty-env)
  'x)
 #f)
(eqv?
 (has-binding?
  (extend-env 'x 3 (extend-env 's 2 (empty-env)))
  'x)
 #t)
(eqv?
 (has-binding?
  (extend-env 'x 3 (extend-env 's 2 (empty-env)))
  's)
 #t)
(eqv?
 (has-binding?
  (extend-env 'x 3 (extend-env 's 2 (empty-env)))
  'y)
 #f)

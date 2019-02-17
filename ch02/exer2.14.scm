#lang eopl
;;; () -> Env
(define empty-env
  (lambda ()
    (list
     (lambda (search-var)
       (report-no-binding-found search-var))
     (lambda () #t)
     (lambda (search-var) #f))))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
;;; Var x SchemeVal x Env -> Env
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list
     (lambda (search-var)
       (if (eqv? search-var saved-var)
           saved-val
           (apply-env saved-env search-var)))
     (lambda () #f)
     (lambda (search-var)
       (if (eqv? search-var saved-var)
           #t
           (has-binding? saved-env search-var))))))
;;; Env x Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))
;;; Env -> #t | #f
(define empty-env?
  (lambda (env)
    ((cadr env))))
;;; Env x Var -> #t | #f
(define has-binding?
  (lambda (env search-var)
    ((caddr env) search-var)))
;;; ---- test ----
;;; ---- test ----
(has-binding? (empty-env) 's)
(has-binding? (extend-env 's 2 (empty-env)) 's)
(has-binding? (extend-env 's 2 (empty-env)) 's)
(has-binding? (extend-env 'x 'y (extend-env 's 2 (empty-env))) 'x)
(has-binding? (extend-env 's 9 (extend-env 'x 'y (extend-env 's 2 (empty-env)))) 'x)
(has-binding? (extend-env 's 9 (extend-env 'x 'y (extend-env 's 2 (empty-env)))) 'y)

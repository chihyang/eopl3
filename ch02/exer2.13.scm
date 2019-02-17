#lang eopl
;;; () -> Env
(define empty-env
  (lambda ()
    (list
     (lambda (search-var)
       (report-no-binding-found search-var))
     (lambda () #t))))
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
     (lambda () #f))))
;;; Env x Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))
;;; Env -> #t | #f
(define empty-env?
  (lambda (env)
    ((cadr env))))
;;; ---- test ----
(empty-env? (empty-env))
(empty-env? (extend-env 'a 2 (empty-env)))

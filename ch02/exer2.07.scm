#lang eopl
;;; env
(define empty-env
  (lambda () (list 'empty-env)))
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))
(define apply-env
  (lambda (env search-var)
    (apply-env-internal env search-var env)))
(define apply-env-internal
  (lambda (env search-var out-env)
    (cond
      ((eqv? (car env) 'empty-env)
       (report-no-binding-found search-var out-env))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env-internal saved-env search-var out-env))))
      (else
       (report-invalid-env env)))))
(define report-no-binding-found
  (lambda (search-var env)
    (eopl:error 'apply-env "No binding for ~s found in ~s" search-var env)))
(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))

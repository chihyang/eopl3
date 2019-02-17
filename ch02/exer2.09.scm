#lang eopl
;;; from exercise 2.5
(define empty-env (lambda () '()))
(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))
(define apply-env
  (lambda (env search-var)
    (cond
      ((null? env)
       (report-no-binding-found search-var))
      ((eqv? (caar env) search-var)
       (cdar env))
      (else
       (apply-env (cdr env) search-var)))))
(define empty-env? null?)
(define has-binding?
  (lambda (env s)
    (cond ((empty-env? env)
           #f)
          ((eqv? (caar env) s)
           #t)
          (else
           (has-binding? (cdr env) s)))))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
;;; ---- test ----
(has-binding? (empty-env) 's)
(has-binding? (extend-env 's 2 (empty-env)) 's)
(has-binding? (extend-env 's 2 (empty-env)) 's)
(has-binding? (extend-env 'x 'y (extend-env 's 2 (empty-env))) 'x)
(has-binding? (extend-env 's 9 (extend-env 'x 'y (extend-env 's 2 (empty-env)))) 'x)
(has-binding? (extend-env 's 9 (extend-env 'x 'y (extend-env 's 2 (empty-env)))) 'y)

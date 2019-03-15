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
(define extend-env*
  (lambda (vars vals env)
    (if (null? vars)
        env
        (extend-env* (cdr vars)
                     (cdr vals)
                     (extend-env (car vars) (car vals) env)))))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
;;; ---- test ----
(extend-env* '() '() (extend-env 's 2 (empty-env)))
(extend-env* '(c) '(5) (extend-env 's 2 (empty-env)))
(extend-env* '(a b c) '(3 4 5) (extend-env 's 2 (empty-env)))
(extend-env* '(a a d d d d b c) '(3 4 5 3 8 9 0 2) (extend-env 's 2 (empty-env)))

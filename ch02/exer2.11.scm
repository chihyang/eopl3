#lang eopl
(define empty-env (lambda () '()))
(define extend-env
  (lambda (var val env)
    (cons (cons (list var) (list val)) env)))
(define apply-env
  (lambda (env search-var)
    (cond
      ((null? env)
       (report-no-binding-found search-var))
      ((contains? search-var (caar env))
       (get-val search-var (caar env) (cdar env)))
      (else
       (apply-env (cdr env) search-var)))))
(define empty-env? null?)
(define extend-env*
  (lambda (vars vals env)
    (cons (cons vars vals) env)))
(define contains?
  (lambda (var lst)
    (if (null? lst)
        #f
        (if (eqv? var (car lst))
            #t
            (contains? var (cdr lst))))))
(define get-val
  (lambda (var var-lst val-lst)
    (if (null? var-lst)
        (report-no-binding-found var)
        (if (eqv? var (car var-lst))
            (car val-lst)
            (get-val var (cdr var-lst) (cdr val-lst))))))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
;;; ---- test ----
(extend-env* '() '() (extend-env 's 2 (empty-env)))
(extend-env* '(c) '(5) (extend-env 's 2 (empty-env)))
(extend-env* '(a b c) '(3 4 5) (extend-env 's 2 (empty-env)))
(extend-env* '(a a d d d d b c) '(3 4 5 3 8 9 0 2) (extend-env 's 2 (empty-env)))
;; error
(apply-env (extend-env* '(a a d d d d b c) '(3 4 5 3 8 9 0 2) (extend-env 's 2 (empty-env))) 'x)

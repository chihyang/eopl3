#lang eopl
(require "exer7.11.lang.scm")
(require "exer7.11.store.scm")
(require (only-in racket/base mcons))
(provide (all-defined-out))
;;; ---------------------- Environment (rewrite for section 7.3) ----------------------
(define member?
  (lambda (sym lst)
    (if (null? lst)
        #f
        (or (eqv? sym (car lst))
            (member? sym (cdr lst))))))
(define check-duplicates
  (lambda (lst)
    (cond ((null? lst) '())
          ((member? (car lst) (cdr lst)) (car lst))
          (else (check-duplicates (cdr lst))))))
(define empty-env?
  (lambda (env)
    (and (list? env)
         (not (null? env))
         (eqv? (car env) 'empty-env))))
(define extended-env?
  (lambda (env)
    (and (list? env)
         (not (null? env))
         (or (eqv? (car env) 'extend-env)
             (eqv? (car env) 'extend-env-rec)))))
(define environment?
  (lambda (env)
    (or (empty-env? env)
        (extended-env? env))))
(define empty-env
  (lambda () (list 'empty-env)))
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))
(define extend-env-rec
  (lambda (p-names p-vars p-bodies env)
    (let ((dup-name (check-duplicates p-names)))
      (if (null? dup-name)
          (list 'extend-env-rec
                (list p-names
                      p-vars
                      p-bodies
                      (map (lambda (v) (newref 'uninitialized)) p-names))
                env)
          (report-duplicate-id dup-name)))))
;; extend-env* : Listof(Id) x Listof(ExpVal) x Env -> Env
(define extend-env*
  (lambda (vars vals env)
    (let ((dup (check-duplicates vars))
          (var-len (length vars))
          (val-len (length vals)))
      (cond [(not (null? dup)) (report-duplicate-id dup)]
            [(< var-len val-len)
             (report-argument-mismatch 'greater)]
            [(> var-len val-len)
             (report-argument-mismatch 'less)]
            [else
             (letrec ((extend-env*-inner
                       (lambda (vars vals env)
                         (cond [(null? vars)
                                env]
                               [else
                                (extend-env*-inner
                                 (cdr vars)
                                 (cdr vals)
                                 (list 'extend-env (car vars) (car vals) env))]))))
               (extend-env*-inner vars vals env))]))))
(define apply-env
  (lambda (env search-var)
    (cond
     ((eqv? (car env) 'empty-env)
      (report-no-binding-found search-var))
     ((eqv? (car env) 'extend-env)
      (let ((saved-var (cadr env))
            (saved-ref (caddr env))
            (saved-env (cadddr env)))
        (if (eqv? search-var saved-var)
            saved-ref
            (apply-env saved-env search-var))))
     ((eqv? (car env) 'extend-env-rec)
      (let ((saved-p-names (list-ref (cadr env) 0))
            (saved-b-vars (list-ref (cadr env) 1))
            (saved-p-bodies (list-ref (cadr env) 2))
            (saved-p-refs (list-ref (cadr env) 3))
            (saved-env (caddr env)))
        (let ((func-idx (apply-env-rec search-var saved-p-names)))
          (if func-idx
              (let ((p-ref (list-ref saved-p-refs func-idx)))
                (when (uninitialized? p-ref)
                  (setref!
                   p-ref
                   (proc-val
                    (procedure
                     (list-ref saved-b-vars func-idx)
                     (list-ref saved-p-bodies func-idx)
                     env))))
                p-ref)
              (apply-env saved-env search-var)))))
     (else
      (report-invalid-env env)))))
(define apply-env-rec
  (lambda (var p-names)
    (let inner ([var var]
                [p-names p-names]
                [idx 0])
      (cond [(null? p-names) #f]
            [(eqv? var (car p-names)) idx]
            [else (inner var (cdr p-names) (+ idx 1))]))))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))
(define report-argument-mismatch
  (lambda (symp)
    (eopl:error 'extend-env* "Argument number is ~s than parameter number" symp)))
(define report-duplicate-id
  (lambda (sym)
    (eopl:error 'extend-env* "Duplicate identifier ~s" sym)))

;;; ---------------------- Expval ----------------------
(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body expression?)
   (saved-env environment?)))

(define-datatype mutpair mutpair?
  (a-pair
   (left-loc reference?)
   (right-loc reference?)))
;; make-pair : ExpVal x ExpVal -> MutPair
(define make-pair
  (lambda (val1 val2)
    (a-pair
     (newref val1)
     (newref val2))))
;; left : MutPair -> ExpVal
(define left
  (lambda (p)
    (cases mutpair p
           (a-pair (left-loc right-loc)
                   (deref left-loc)))))
;; right : MutPair -> ExpVal
(define right
  (lambda (p)
    (cases mutpair p
           (a-pair (left-loc right-loc)
                   (deref right-loc)))))
;; setleft : MutPair x ExpVal -> Unspecified
(define setleft
  (lambda (p val)
    (cases mutpair p
           (a-pair (left-loc right-loc)
                   (setref! left-loc val)))))
;; setright : MutPair x ExpVal -> Unspecified
(define setright
  (lambda (p val)
    (cases mutpair p
           (a-pair (left-loc right-loc)
                   (setref! right-loc val)))))

(define-datatype exp-val exp-val?
  (num-val
   (val number?))
  (bool-val
   (val boolean?))
  (proc-val
   (val proc?))
  (mutpair-val
   (val mutpair?)))

;; mutpair->schemeval : MutPair -> MPair
(define mutpair->schemeval
  (lambda (v)
    (cases mutpair v
           (a-pair
            (val1 val2)
            (list 'mcons
                  (expval->schemeval (deref val1))
                  (expval->schemeval (deref val2)))))))

;; expval->schemeval : ExpVal -> SchemeVal
(define expval->schemeval
  (lambda (v)
    (cases exp-val v
           (num-val
            (num)
            num)
           (bool-val
            (bool)
            bool)
           (proc-val
            (p)
            (cases proc p
                   (procedure
                    (var saved-env body)
                    `(λ (,var) ...))))
           (mutpair-val
            (val)
            (mutpair->schemeval val)))))

(define expval->num
  (lambda (value)
    (cases exp-val value
           (num-val
            (number)
            number)
           (else
            (report-invalid-exp-value 'num)))))
(define expval->bool
  (lambda (value)
    (cases exp-val value
           (bool-val
            (boolean)
            boolean)
           (else
            (report-invalid-exp-value 'bool)))))
(define expval->proc
  (lambda (value)
    (cases exp-val value
           (proc-val
            (proc1)
            proc1)
           (else
            (report-invalid-exp-value 'proc)))))
(define expval->mutpair
  (lambda (value)
    (cases exp-val value
           (mutpair-val
            (val)
            val)
           (else
            (report-invalid-exp-value 'mutpair)))))

(define report-invalid-exp-value
  (lambda (type)
    (eopl:error
     'exp-val
     "Not a valid exp value of type ~s" type)))

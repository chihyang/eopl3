#lang eopl
(require "exer9.06.lang.scm")
(require "chap09.s03.store.scm")
(provide (all-defined-out))
;;; ---------------------- Environment(from section 3.2) ----------------------
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var identifier?)
   (val reference?)
   (env environment?))
  (extend-env-rec
   (p-names (list-of identifier?))
   (b-vars (list-of (list-of identifier?)))
   (b-bodies (list-of expression?))
   (saved-env environment?))
  (extend-env-with-self-and-super
   (self-obj reference?)
   (super-name identifier?)
   (saved-env environment?)))

;;; extend-env* : Listof(Id) x Listof(Ref) -> Env
(define extend-env*
  (lambda (vars vals env)
    (check-duplicate-identifier! vars)
    ;; note this change, why is it necessary to exclude this check?
    ;; (check-var-val-number! vars vals)
    (let loop ((vars vars)
               (vals vals)
               (env env))
      (if (null? vars)
          env
          (loop (cdr vars)
                (cdr vals)
                (extend-env (car vars) (car vals) env))))))

;;; check-duplicate-identifier! : Listof(Id) -> Bool | Unspecified
(define check-duplicate-identifier!
  (lambda (vars)
    (let loop ((vs vars))
      (cond [(null? vs) #t]
            [(member? (car vs) (cdr vs))
             (report-duplicate-id (car vs) vars)]
            [else
             (loop (cdr vs))]))))

;;; member? : Sym x Listof(Sym) -> Bool
(define member?
  (lambda (sym lst)
    (cond [(null? lst) #f]
          [(eqv? sym (car lst)) #t]
          [else (member? sym (cdr lst))])))

;;; check-var-val-number! : Listof(Sym) x Listof(ExpVal) -> Bool | Unspecified
(define check-var-val-number!
  (lambda (vars vals)
    (let ((var-len (length vars))
          (val-len (length vals)))
      (cond [(< var-len val-len)
             (report-argument-mismatch 'more vars vals)]
            [(> var-len val-len)
             (report-argument-mismatch 'less vars vals)]
            [else #t]))))

(define apply-env
  (lambda (env search-var)
    (cases environment env
           (empty-env
            ()
            (report-no-binding-found search-var))
           (extend-env
            (saved-var saved-val saved-env)
            (if (equal? search-var saved-var)
                saved-val
                (apply-env saved-env search-var)))
           (extend-env-rec
            (p-names b-vars b-bodies saved-env)
            (let ((idx (lookup-proc-name p-names search-var)))
              (if idx
                  (let ((b-vars (list-ref b-vars idx))
                        (b-body (list-ref b-bodies idx)))
                    (newref (proc-val (procedure b-vars b-body env))))
                  (apply-env saved-env search-var))))
           (extend-env-with-self-and-super
            (self-obj super-name saved-env)
            (cond [(eqv? search-var '%self)
                   self-obj]
                  [(eqv? search-var '%super)
                   super-name]
                  [else
                   (apply-env saved-env search-var)])))))

;;; lookup-proc-name : Listof(Sym) x Sym -> Int | #f
(define lookup-proc-name
  (lambda (p-names search-var)
    (let loop ((p-names p-names)
               (idx 0))
      (cond [(null? p-names) #f]
            [(equal? search-var (car p-names)) idx]
            [else (loop (cdr p-names) (+ idx 1))]))))

;; init-env : () → Env
;; usage: (init-env) = []
(define init-env
  (lambda ()
    (empty-env)))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-no-module-found
  (lambda (search-var)
    (eopl:error 'apply-env "No module for ~s" search-var)))

(define report-lookup-failed
  (lambda (m-name var-name)
    (eopl:error 'lookup-qualified-var-in-env
                "can't retrieve variable from ~s take ~s from proc module"
                m-name var-name)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))
(define report-argument-mismatch
  (lambda (symp vars vals)
    (eopl:error 'extend-env*
                "Argument number is ~s than parameter number: ~a, ~a"
                symp vars vals)))
(define report-duplicate-id
  (lambda (sym)
    (eopl:error 'extend-env* "Duplicate identifier ~s" sym)))

;;; ---------------------- Expval ----------------------
(define-datatype proc proc?
  (procedure
   (var (list-of identifier?))
   (body expression?)
   (saved-env environment?)))

(define-datatype object object?
  (an-object
   (class-name identifier?)
   (fields (list-of reference?))))

;;; object->class-name : Object -> Sym
(define object->class-name
  (lambda (obj)
    (cases object obj
           (an-object
            (c-name fields)
            c-name))))

;;; object->fields : Object -> Listof(Ref)
(define object->fields
  (lambda (obj)
    (cases object obj
           (an-object
            (c-name fields)
            fields))))

(define-datatype exp-val exp-val?
  (num-val
   (val number?))
  (bool-val
   (val boolean?))
  (proc-val
   (val proc?))
  (null-val)
  (pair-val
   (val1 exp-val?)
   (val2 exp-val?))
  (obj-val
   (val object?)))

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
           (null-val
            ()
            '())
           (pair-val
            (val1 val2)
            `(,(expval->schemeval val1) .
              ,(expval->schemeval val2)))
           (obj-val
            (val)
            (cases object val
                   (an-object
                    (c-name fields)
                    (list c-name (map (lambda (f) (expval->schemeval (deref f))) fields))))))))

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
(define expval->obj
  (lambda (value)
    (cases exp-val value
           (obj-val (obj) obj)
           (else
            (report-invalid-exp-value 'object)))))
(define expval->pair
  (lambda (value)
    (cases exp-val value
           (pair-val
            (val1 val2)
            (cons
             (cases exp-val val1
                    (num-val (num) num)
                    (bool-val (bool) bool)
                    (proc-val (proc1) proc1)
                    (null-val () '())
                    (obj-val (val3) val3)
                    (pair-val (val3 val4) (expval->pair val1)))
             (cases exp-val val2
                    (num-val (num) num)
                    (bool-val (bool) bool)
                    (proc-val (proc1) proc1)
                    (null-val () '())
                    (obj-val (val3) val3)
                    (pair-val (val3 val4) (expval->pair val2)))))
           (else
            (report-invalid-exp-value 'pair)))))

(define report-invalid-exp-value
  (lambda (type)
    (eopl:error
     'exp-val
     "Not a valid exp value of type ~s" type)))

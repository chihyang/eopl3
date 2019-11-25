#lang eopl
(require "chap09.s03.lang.scm")
(require "chap09.s03.store.scm")
(provide (all-defined-out))
;;; ---------------------- Environment(from section 3.2) ----------------------
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var identifier?)
   (val exp-val?)
   (env environment?))
  (extend-env-rec
   (p-names (list-of identifier?))
   (b-vars (list-of (list-of identifier?)))
   (b-bodies (list-of expression?))
   (saved-env environment?))
  (extend-env-with-self-and-super
   (self-obj identifier?)
   (super-name identifier?)
   (saved-env environment?)))

;;; extend-env* : Listof(Id) x Listof(ExpVal) -> Env
(define extend-env*
  (lambda (vars vals env)
    (check-duplicate-identifier! vars)
    (check-var-val-number! vars vals)
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
                    (proc-val (procedure b-vars b-body env)))
                  (apply-env saved-env search-var))))
           (extend-env-with-module
            (m-name m-val saved-env)
            (if (equal? search-var m-name)
                m-val
                (apply-env saved-env search-var))))))

;;; lookup-proc-name : Listof(Sym) x Sym -> Int | #f
(define lookup-proc-name
  (lambda (p-names search-var)
    (let loop ((p-names p-names)
               (idx 0))
      (cond [(null? p-names) #f]
            [(equal? search-var (car p-names)) idx]
            [else (loop (cdr p-names) (+ idx 1))]))))

;;; lookup-qualified-var-in-env : Sym x Sym x Env -> ExpVal
(define lookup-qualified-var-in-env
  (lambda (m-name var-name env)
    (let ((m-val (lookup-module-name-in-env m-name env)))
      (cases typed-module m-val
             (simple-module
              (bindings)
              (apply-env bindings var-name))
             (proc-module
              (b-var body saved-env)
              (report-lookup-failed m-name var-name))))))

;;; lookup-module-name-in-env : Sym x Env -> ExpVal
(define lookup-module-name-in-env
  (lambda (m-name env)
    (let ((val (apply-env env m-name)))
      (if (typed-module? val)
          val
          (report-no-module-found m-name)))))

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

;;; ---------------------- Object ----------------------
(define-datatype object object?
  (an-object
   (class-name identifier?)
   (fields (list-of reference?))))

;;; new-object : ClassName (= Sym) -> Obj
(define new-object
  (lambda (class-name)
    (map (lambda (field-name)
           (newref (list 'uninitialized-field field-name)))
         (class->field-names (lookup-class class-name)))))

(define-datatype method method?
  (a-method
   (vars (list-of identifier?))
   (body expression?)
   (super-name identifier?)
   (field-names (list-of identifier?))))

;;; apply-method : Method x Obj x Listof(ExpVal) -> ExpVal
(define apply-method
  (lambda (m self args)
    (cases method m
           (a-method
            (vars body super-name field-names)
            (value-of
             body
             (extend-env* vars (map newref args)
                          (extend-env-with-self-and-super
                           self super-name
                           (extend-env field-names
                                       (object->fields self)
                                       (empty-env)))))))))

;;; ---------------------- Expval ----------------------
(define-datatype proc proc?
  (procedure
   (var identifier?)
   (body expression?)
   (saved-env environment?)))

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
            ))))

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
(define expval->ref
  (lambda (value)
    (cases exp-val value
           (ref-val (ref) ref)
           (else
            (report-invalid-exp-value 'ref)))))
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
                    (ref-val (ref) '(refto ,ref))
                    (pair-val (val3 val4) (expval->pair val1)))
             (cases exp-val val2
                    (num-val (num) num)
                    (bool-val (bool) bool)
                    (proc-val (proc1) proc1)
                    (null-val () '())
                    (ref-val (ref) '(refto ,ref))
                    (pair-val (val3 val4) (expval->pair val2)))))
           (else
            (report-invalid-exp-value 'pair)))))

(define report-invalid-exp-value
  (lambda (type)
    (eopl:error
     'exp-val
     "Not a valid exp value of type ~s" type)))

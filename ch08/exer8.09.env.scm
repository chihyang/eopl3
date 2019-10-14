#lang eopl
(require "exer8.09.lang.scm")
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
  (extend-env-with-module
   (m-name identifier?)
   (m-val typed-module?)
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
              (apply-env bindings var-name))))))

;;; lookup-module-name-in-env : Sym x Env -> ExpVal
(define lookup-module-name-in-env
  (lambda (m-name env)
    (let ((val (apply-env env m-name)))
      (if (typed-module? val)
          val
          (report-no-module-found m-name)))))

(define report-duplicate-id
  (lambda (sym syms)
    (eopl:error 'extend-env* "Duplicate identifier ~s in ~a"
                sym syms)))

(define report-argument-mismatch
  (lambda (symp vars vals)
    (eopl:error 'extend-env*
                "Argument number is ~s than parameter number: ~a, ~a"
                symp vars vals)))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-no-module-found
  (lambda (search-var)
    (eopl:error 'apply-env "No module for ~s" search-var)))

;;; ---------------------- Expval ----------------------
(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body expression?)
   (saved-env environment?)))

(define-datatype typed-module typed-module?
  (simple-module
   (bindings environment?)))

(define-datatype exp-val exp-val?
  (num-val
   (val number?))
  (bool-val
   (val boolean?))
  (proc-val
   (val proc?)))

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
                    `(λ (,var) ...)))))))

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
(define report-invalid-exp-value
  (lambda (type)
    (eopl:error
     'exp-val
     "Not a valid exp value of type ~s" type)))

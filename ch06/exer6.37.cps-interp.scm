#lang eopl
(require "exer6.37.cps-out-lang.scm")
(provide (all-defined-out))
;;; ---------------------- Environment (from section 3.2) ----------------------
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
         (eqv? (car env) 'extend-env))))
(define environment?
  (lambda (env)
    (or (empty-env? env)
        (extended-env? env))))
(define empty-env
  (lambda () (list 'empty-env)))
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))
(define extend-env*
  (lambda (vars vals env)
    (let ((duplicate (check-duplicates vars))
          (var-len (length vars))
          (val-len (length vals)))
      (cond [(not (null? duplicate))
             (report-duplicate-id duplicate)]
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
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
      (else
       (report-invalid-env env)))))
;;; apply-env-rec : Sym x Listof(Sym) x Listof(Listof(Sym)) x Listof(Expression) ->
;;;                 ((Listof(Sym), Expression) | '(),
;;;                  (Listof(Sym),
;;;                   Listof(Listof(Sym)),
;;;                   Listof(Expression)))
(define apply-env-rec
  (lambda (var p-names p-vars p-bodies)
    (cond [(null? p-names) '()]
          [(eqv? var (car p-names))
           (list (car p-vars) (car p-bodies))]
          [else
           (apply-env-rec var (cdr p-names) (cdr p-vars) (cdr p-bodies))])))
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
(define identifier? symbol?)
(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body tf-exp?)
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
  (ref-val
   (val number?)))
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
(define expval->null
  (lambda (value)
    (cases exp-val value
           (null-val
            ()
            '())
           (else
            (report-invalid-exp-value 'null)))))
(define expval->pair
  (lambda (value)
    (cases exp-val value
           (pair-val
            (val1 val2)
            (cons
             (cases exp-val val1
                    (num-val (num) num)
                    (bool-val (bool) bool)
                    (null-val () '())
                    (proc-val (proc1) proc1)
                    (pair-val (val3 val4) (expval->pair val1))
                    (ref-val (ref) ref))
             (cases exp-val val2
                    (num-val (num) num)
                    (bool-val (bool) bool)
                    (null-val () '())
                    (proc-val (proc1) proc1)
                    (pair-val (val3 val4) (expval->pair val2))
                    (ref-val (ref) ref))))
           (else
            (report-invalid-exp-value 'pair)))))
(define expval->ref
  (lambda (value)
    (cases exp-val value
           (ref-val
            (ref)
            ref)
           (else
            (report-invalid-exp-value 'ref)))))
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
           (null-val
            ()
            (eopl:pretty-print '()))
           (pair-val
            (val1 val2)
            (expval->pair v))
           (proc-val
            (p)
            (cases proc p
                   (procedure
                    (var saved-env body)
                    `(λ (,var) ...))))
           (ref-val
            (ref)
            ref))))
(define report-invalid-exp-value
  (lambda (type)
    (eopl:error
     'exp-val
     "Not a valid exp value of type ~s" type)))

;;; ---------------------- Store (from section 4.2) ----------------------
;; empty-store : () -> Sto
(define empty-store (lambda () '()))
;; usage : A scheme variable containing the current state of the
;; store. Initially set to a dummy value.
(define the-store 'uninitialized)
;; get-store : () -> Sto
(define get-store
  (lambda () the-store))
;; initialize-store! : () -> Unspecified
;; usage : (initialize-store!) sets the store to the empty store
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))
;; reference? : SchemeVal -> Bool
(define reference?
  (lambda (v)
    (integer? v)))
;; newref : ExpVal | Uninitialized -> Ref
(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))
;; deref : Ref -> ExpVal
(define deref
  (lambda (ref)
    (let ((val (list-ref the-store ref)))
      (if (eqv? val 'uninitialized)
          (report-uninitialized-reference ref the-store)
          val))))
;; setref! : Ref x ExpVal -> Unspecified
;; usage : sets the-store to a state like the original, but with position ref
;; containing val
(define setref!
  (lambda (ref val)
    (set! the-store
      (letrec ((setref-inner
                ;; usage : returns a list like store1, except that position ref1
                ;; contains val.
                (lambda (store1 ref1)
                  (cond [(null? store1)
                         (report-invalid-reference ref the-store)]
                        [(zero? ref1)
                         (cons val (cdr store1))]
                        [else
                         (cons (car store1)
                               (setref-inner (cdr store1) (- ref1 1)))]))))
        (setref-inner the-store ref)))))
(define report-invalid-reference
  (lambda (ref store)
    (eopl:error
     'exp-val
     "Not a valid reference ~a for store ~a" ref store)))
(define report-uninitialized-reference
  (lambda (ref store)
    (eopl:error
     'deref
     "Reference ~a is not initialized in store ~a" ref store)))

;;; ---------------------- Continuation ----------------------
(define end-cont
  (lambda ()
    (lambda (val)
      ;; (eopl:printf "End of computation.~%")
      val)))
(define apply-cont
  (lambda (cont val)
    (cont val)))

;;; ---------------------- Evaluate CPS-OUT expression ----------------------
;;; apply-procedure/k : Proc x Listof(ExpVal) x Cont -> FinalAnswer
(define apply-procedure/k
  (lambda (proc1 vals cont)
    (cases proc proc1
           (procedure
            (vars body saved-env)
            (value-of/k body (extend-env* vars vals saved-env) cont)))))

;;; value-of/k : TfExp x Env x Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (cases tf-exp exp
           (simple-exp->exp
            (simple)
            (apply-cont cont
                        (value-of-simple-exp simple env)))
           (cps-if-exp
            (exp1 exp2 exp3)
            (if (expval->bool (value-of-simple-exp exp1 env))
                (value-of/k exp2 env cont)
                (value-of/k exp3 env cont)))
           (cps-let-exp
            (vars exps body)
            (value-of/k body
                        (extend-env* vars
                                     (map (lambda (e) (value-of-simple-exp e env))
                                          exps)
                                     env)
                        cont))
           (cps-call-exp
            (rator rands)
            (let ((proc (expval->proc (value-of-simple-exp rator env)))
                  (args (map (lambda (e) (value-of-simple-exp e env)) rands)))
              (apply-procedure/k proc args cont)))
           (cps-printk-exp
            (simple-exp1 body)
            (eopl:printf "~a~%" (expval->schemeval (value-of-simple-exp simple-exp1 env)))
            (value-of/k body env cont))
           (cps-newrefk-exp
            (exp1 exp2)
            (let ((val1 (value-of-simple-exp exp1 env))
                  (val2 (value-of-simple-exp exp2 env)))
              (let ((newval (ref-val (newref val1))))
                (apply-procedure/k
                 (expval->proc val2)
                 (list newval)
                 cont))))
           (cps-derefk-exp
            (exp1 exp2)
            (let ((val1 (value-of-simple-exp exp1 env))
                  (val2 (value-of-simple-exp exp2 env)))
              (apply-procedure/k
               (expval->proc val2)
               (list (deref (expval->ref val1)))
               cont)))
           (cps-setrefk-exp
            (exp1 exp2 body)
            (let ((val1 (value-of-simple-exp exp1 env))
                  (val2 (value-of-simple-exp exp2 env)))
              (setref! (expval->ref val1) val2)
              (value-of/k body env cont))))))

;;; value-of-simple-exp : SimpleExp x Env -> ExpVal
(define value-of-simple-exp
  (lambda (exp env)
    (cases simple-exp exp
           (cps-const-exp
            (num)
            (num-val num))
           (cps-var-exp
            (var)
            (apply-env env var))
           (cps-proc-exp
            (vars body)
            (proc-val (procedure vars body env)))
           (cps-sum-exp
            (exp1)
            (let ((nums
                   (map (lambda (e) (expval->num (value-of-simple-exp e env)))
                        exp1)))
                 (num-val (apply + nums))))
           (cps-diff-exp
            (exp1 exp2)
            (let ((num1 (expval->num (value-of-simple-exp exp1 env)))
                  (num2 (expval->num (value-of-simple-exp exp2 env))))
              (num-val (- num1 num2))))
           (cps-zero?-exp
            (exp1)
            (bool-val (zero? (expval->num (value-of-simple-exp exp1 env))))))))

;;; value-of-cps-program : CPS-Out-Program -> FinalAnswer
(define value-of-cps-program
  (lambda (prog)
    (initialize-store!)
    (cases cps-program prog
           (cps-a-program
            (exp)
            (let ((val (value-of/k exp (empty-env) (end-cont))))
              (expval->schemeval val))))))

(define run
  (lambda (prgm)
    (value-of-cps-program prgm)))

;;; checked-run : String -> Int | Bool | Proc | '() | List | Pair | String (for exception)
(require racket/base)
(define checked-run
  (lambda (prgm)
    (with-handlers
        [(exn:fail? (lambda (en) 'error))]
      (run prgm))))

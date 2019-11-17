#lang eopl
(require "exer8.25.lang.scm")
(require "exer8.25.env.scm")
(provide value-of-program value-of run checked-run)

;;; ---------------------- Evaluate expression ----------------------
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
           (procedure
            (var body saved-env)
            (value-of body (extend-env var val saved-env))))))

;;; add-module-defns-to-env : Listof(ModuleDefn) x Env -> Env
(define add-module-defns-to-env
  (lambda (m-defs env)
    (if (null? m-defs)
        env
        (cases module-defn (car m-defs)
               (a-module-definition
                (m-name expected-iface m-body)
                (add-module-defns-to-env
                 (cdr m-defs)
                 (extend-env-with-module
                  m-name
                  (value-of-module-body m-body env)
                  env)))))))

;;; value-of-module-body : ModuleBody x Env -> TypedModule
(define value-of-module-body
  (lambda (m-body env)
    (cases module-body m-body
           (defns-module-body
             (defns)
             (simple-module (defns-to-env defns env)))
           (proc-module-body
            (m-names ifaces m-body)
            (proc-module m-names m-body env))
           (var-module-body
            (m-name)
            (lookup-module-name-in-env m-name env))
           (app-module-body
            (rator rands)
            (let ((rator-val (lookup-module-name-in-env rator env)))
              (let ((rand-vals
                     (map (lambda (r) (lookup-module-name-in-env r env))
                          rands)))
                (cases typed-module rator-val
                       (proc-module
                        (m-names m-body saved-env)
                        (value-of-module-body
                         m-body
                         (extend-env-with-module*
                          m-names rand-vals saved-env)))
                       (else
                        (report-bad-module-app rator-val)))))))))

(define report-bad-module-app
  (lambda (rator-val)
    (eopl:error 'value-of-module-body
                "can't apply non-proc-module-value ~s" rator-val)))

;;; defns-to-env : Listof(Defn) × Env → Env
(define defns-to-env
  (lambda (defns env)
    (if (null? defns)
        env
        (cases definition (car defns)
               (val-defn
                (var exp)
                (defns-to-env
                  (cdr defns)
                  (extend-env var
                              (value-of exp env)
                              env)))
               (else
                (defns-to-env (cdr defns) env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
           (qualified-var-exp
            (m-name var-name)
            (lookup-qualified-var-in-env m-name var-name env))
           (const-exp
            (num)
            (num-val num))
           (var-exp
            (var)
            (apply-env env var))
           (diff-exp
            (exp1 exp2)
            (num-val (- (expval->num (value-of exp1 env))
                        (expval->num (value-of exp2 env)))))
           (zero?-exp
            (exp1)
            (bool-val (eqv? (expval->num (value-of exp1 env)) 0)))
           (if-exp
            (exp1 exp2 exp3)
            (if (expval->bool (value-of exp1 env))
                (value-of exp2 env)
                (value-of exp3 env)))
           (let-exp
            (var exp1 body)
            (let ((val (value-of exp1 env)))
              (value-of body (extend-env var val env))))
           (letrec-exp
            (p-result-type p-name p-var p-arg-type p-body letrec-body)
            (value-of letrec-body (extend-env-rec (list p-name) (list p-var) (list p-body) env)))
           (proc-exp
            (vars var-type body)
            (proc-val (procedure vars body env)))
           (call-exp
            (rator rand)
            (let ((proc (expval->proc (value-of rator env)))
                  (arg (value-of rand env)))
              (apply-procedure proc arg))))))

;; value-of-program : Program -> SchemeVal
(define value-of-program
  (lambda (prog)
    (cases program prog
           (a-program
            (m-defs exp)
            (let ((new-env (add-module-defns-to-env m-defs (empty-env))))
              (let ((val (value-of exp new-env)))
                (expval->schemeval val)))))))

(define run
  (lambda (prgm)
    (value-of-program prgm)))

;;; checked-run : String -> Int | Bool | Proc | String (for exception)
(require (only-in racket/base with-handlers exn:fail?))
(define checked-run
  (lambda (prgm)
    (with-handlers
        [(exn:fail? (lambda (en) 'error))]
      (run prgm))))

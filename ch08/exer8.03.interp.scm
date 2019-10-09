#lang eopl
(require "exer8.03.lang.scm")
(require "exer8.03.env.scm")
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
                 (let ((actual-iface (value-of-module-body m-body env)))
                   (cases interface expected-iface
                          (simple-iface
                           (decls)
                           (extend-env-with-module
                            m-name
                            (simple-module (prune-module-env decls actual-iface (empty-env)))
                            env))))))))))

;;; value-of-module-body : ModuleBody x Env -> Env
(define value-of-module-body
  (lambda (m-body env)
    (cases module-body m-body
           (defns-module-body
             (defns)
             (defns-to-env defns env)))))

;;; prune-module-env : Listof(Decl) x Env x Env -> Env
(define prune-module-env
  (lambda (decls actual-env expected-env)
    (if (null? decls)
        expected-env
        (cases declaration (car decls)
               (val-decl
                (var-name var-type)
                (let ((val (apply-env actual-env var-name)))
                  (prune-module-env (cdr decls)
                                    actual-env
                                    (extend-env var-name val expected-env))))))))

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
                              env)))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
           (qualified-var-exp
            (qualified-name)
            (let ((m-name (module-name qualified-name))
                  (var-name (var-name qualified-name)))
              (lookup-qualified-var-in-env m-name var-name env)))
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

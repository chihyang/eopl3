#lang eopl
(require "exer8.06.lang.scm")
(require "exer8.06.env.scm")
(provide value-of-program value-of run checked-run)

;;; ---------------------- Evaluate expression ----------------------
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
           (procedure
            (vars body saved-env)
            (value-of body (extend-env* vars vals saved-env))))))

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
    (cases module-body-top m-body
           (nested-module-body
            (defns body)
            (value-of-module-body-bottom body (add-module-defns-to-env defns env))))))

(define value-of-module-body-bottom
  (lambda (m-body env)
    (cases module-body m-body
           (defns-module-body
             (defns)
             (defns-to-env defns env))
           (let-module-body
            (vars exps let-body)
            (let ((vals (map (lambda (e) (value-of e env)) exps)))
              (value-of-module-body-bottom let-body (extend-env* vars vals env))))
           (letrec-module-body
            (p-result-types p-names p-vars p-arg-types p-bodies letrec-body)
            (value-of-module-body-bottom letrec-body (extend-env-rec p-names p-vars p-bodies env))))))

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
            (vars exps body)
            (let ((vals (map (lambda (e) (value-of e env)) exps)))
              (value-of body (extend-env* vars vals env))))
           (letrec-exp
            (p-result-types p-names p-vars p-arg-types p-bodies letrec-body)
            (value-of letrec-body (extend-env-rec p-names p-vars p-bodies env)))
           (proc-exp
            (vars var-types body)
            (proc-val (procedure vars body env)))
           (call-exp
            (rator rands)
            (let ((proc (expval->proc (value-of rator env)))
                  (args (map (lambda (e) (value-of e env)) rands)))
              (apply-procedure proc args))))))

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

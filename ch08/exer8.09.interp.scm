#lang eopl
(require "exer8.09.lang.scm")
(require "exer8.09.env.scm")
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
                (m-name expected-iface m-spec)
                (let ((new-env (module-spec-env m-spec env))
                      (m-body (module-spec-body m-spec)))
                  (add-module-defns-to-env
                   (cdr m-defs)
                   (let ((actual-iface (value-of-module-body m-body new-env)))
                     (cases interface expected-iface
                            (simple-iface
                             (decls)
                             (extend-env-with-module
                              m-name
                              (simple-module (prune-module-env decls actual-iface (empty-env)))
                              env)))))))))))

(define module-spec-env
  (lambda (spec env)
    (cases module-spec spec
           (depends-module-body
            (m-depends m-body)
            (remove-non-dependency env m-depends))
           (simple-module-body
            (m-body)
            (remove-non-dependency-module-from-env '() env)))))

(define module-spec-body
  (lambda (spec)
    (cases module-spec spec
           (depends-module-body
            (m-depends m-body)
            m-body)
           (simple-module-body
            (m-body)
            m-body))))

;;; value-of-module-body : ModuleBody x Env -> Env
(define value-of-module-body
  (lambda (m-body env)
    (cases module-body m-body
           (defns-module-body
             (defns)
             (defns-to-env defns env))
           (let-module-body
            (vars exps let-body)
            (let ((vals (map (lambda (e) (value-of e env)) exps)))
              (value-of-module-body let-body (extend-env* vars vals env))))
           (letrec-module-body
            (p-result-types p-names p-vars p-arg-types p-bodies letrec-body)
            (value-of-module-body letrec-body (extend-env-rec p-names p-vars p-bodies env))))))

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

;;; defns-to-env : Listof(Defn) × Env → Env
(define remove-non-dependency
  (lambda (env depends)
    (cases module-depends depends
           (simple-module-depends
            (mdl mdls)
            (remove-non-dependency-module-from-env (cons mdl mdls) env)))))

(define remove-non-dependency-module-from-env
  (lambda (mdls env)
    (cases environment env
           (empty-env () (empty-env))
           (extend-env
            (saved-var saved-val saved-env)
            (extend-env
             saved-var saved-val
             (remove-non-dependency-module-from-env mdls saved-env)))
           (extend-env-rec
            (p-names b-vars b-bodies saved-env)
            (extend-env-rec
             p-names b-vars b-bodies
             (remove-non-dependency-module-from-env mdls saved-env)))
           (extend-env-with-module
            (m-name m-val saved-env)
            (if (member? m-name mdls)
                (extend-env-with-module
                 m-name m-val
                 (remove-non-dependency-module-from-env
                  mdls
                  saved-env))
                (remove-non-dependency-module-from-env mdls saved-env))))))

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
            (m-defs prgm-spec)
            (let ((env (add-module-defns-to-env m-defs (empty-env))))
              (value-of-program-spec prgm-spec env))))))

;; value-of-program-spec : ProgramSpec -> SchemeVal
(define value-of-program-spec
  (lambda (spec env)
    (cases program-spec spec
           (depends-program-spec
            (m-depends exp)
            (let ((pruned-env (remove-non-dependency env m-depends)))
              (let ((val (value-of exp pruned-env)))
                (expval->schemeval val))))
           (simple-program-spec
            (exp)
            (let ((pruned-env
                   (remove-non-dependency-module-from-env '() env)))
              (let ((val (value-of exp pruned-env)))
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

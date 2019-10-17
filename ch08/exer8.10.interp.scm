#lang eopl
(require "exer8.10.lang.scm")
(require "exer8.10.env.scm")
(require "exer8.10.store.scm")
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
                (add-module-defns-to-env
                 (cdr m-defs)
                 (extend-env-with-module
                  m-name
                  (newref (a-thunk expected-iface m-spec env))
                  env)))))))

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

;; value-of-program-spec : ProgramSpec -> SchemeVal
(define value-of-program-spec
  (lambda (spec env)
    (cases program-spec spec
           (imports-program-spec
            (m-imports exp)
            (let ((pruned-env (value-of-imports m-imports env)))
              (let ((val (value-of exp pruned-env)))
                (expval->schemeval val))))
           (simple-program-spec
            (exp)
            (let ((val (value-of exp env)))
              (expval->schemeval val))))))

;;; value-of-module-imports : ModuleImports x Env -> Env
(define value-of-imports
  (lambda (imports env)
    (cases module-imports imports
           (simple-module-imports
            (m-name m-names)
            (value-of-import-modules (cons m-name m-names) env)))))

;;; value-of-import-modules : Listof(Sym) x Env -> Env
(define value-of-import-modules
  (lambda (imports env)
    (cond [(null? imports) env]
          [else
           (let ((mod (apply-env env (car imports))))
             (if (reference? mod)
                 (let ((mod-val (deref mod)))
                   (if (typed-module? mod-val)
                       (value-of-import-modules (cdr imports) env)
                       (let ((thunk-val (value-of-thunk mod-val)))
                         (setref! mod thunk-val)
                         (value-of-import-modules (cdr imports) env))))
                 (report-no-module-found (car imports))))])))

;;; value-of-thunk : Thunk -> TypedModule
(define value-of-thunk
  (lambda (thk)
    (cases thunk thk
           (a-thunk
            (iface m-spec saved-env)
            (let ((actual-face (value-of-module-spec m-spec saved-env)))
              (cases interface iface
                     (simple-iface
                      (decls)
                      (simple-module (prune-module-env decls actual-face (empty-env))))))))))

;;; value-of-module-spec : ModuleSpec x Env -> Env
(define value-of-module-spec
  (lambda (spec env)
    (cases module-spec spec
           (imports-module-body
            (m-imports m-body)
            (let ((new-env (value-of-imports m-imports env)))
              (value-of-module-body m-body new-env)))
           (simple-module-body
            (m-body)
            (value-of-module-body m-body env)))))

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

;;; value-of : Exp x Env -> ExpVal
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
              (apply-procedure proc args)))
           (print-exp
            (exp1)
            (let ((val (value-of exp1 env)))
              (print-expval val)
              (num-val 33))))))

;; value-of-program : Program -> SchemeVal
(define value-of-program
  (lambda (prog)
    (initialize-store!)
    (cases program prog
           (a-program
            (m-defs prgm-spec)
            (let ((env (add-module-defns-to-env m-defs (empty-env))))
              (value-of-program-spec prgm-spec env))))))

(define print-expval
  (lambda (val)
    (eopl:printf "~a~%" (expval->schemeval val))))

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

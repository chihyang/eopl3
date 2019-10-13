#lang eopl
(require "exer8.07.lang.scm")
(provide checked-type-of type-of type-of-program type-of-exp)

;;; ---------------------- Type Environment ----------------------
(define-datatype tenv tenv?
  (empty-tenv)
  (extend-tenv
   (saved-var identifier?)
   (saved-type type?)
   (saved-tenv tenv?)))

;;; extend-env* : Listof(Id) x Listof(Type) -> TypeEnv
(define extend-tenv*
  (lambda (vars types tenv)
    (check-duplicate-identifier! vars)
    (check-var-type-number! vars types)
    (let loop ((vars vars)
               (types types)
               (tenv tenv))
      (if (null? vars)
          tenv
          (loop (cdr vars)
                (cdr types)
                (extend-tenv (car vars) (car types) tenv))))))

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

;;; check-var-type-number! : Listof(Sym) x Listof(Type) -> Bool | Unspecified
(define check-var-type-number!
  (lambda (vars types)
    (let ((var-len (length vars))
          (val-len (length types)))
      (cond [(< var-len val-len)
             (report-type-mismatch 'more vars types)]
            [(> var-len val-len)
             (report-type-mismatch 'less vars types)]
            [else #t]))))

(define apply-tenv
  (lambda (env var)
    (cases tenv env
           (empty-tenv
            ()
            (report-no-binding-type-found var))
           (extend-tenv
            (saved-var saved-type saved-tenv)
            (if (eq? var saved-var)
                saved-type
                (apply-tenv saved-tenv var))))))

;;; search-module : Tenv x Sym -> Bool | Iface
(define search-module
  (lambda (env var)
    (cases tenv env
           (empty-tenv
            ()
            #f)
           (extend-tenv
            (saved-var saved-type saved-tenv)
            (if (eq? var saved-var)
                (if (module-type? saved-type)
                    #t
                    (search-module saved-tenv var))
                (search-module saved-tenv var))))))

;;; module-type? : Type -> Bool
(define module-type?
  (lambda (ty)
    (cases type ty
           (module-type
            (val-decls)
            #t)
           (else #f))))

;;; lookup-qualified-var-in-tenv : Sym x Listof(Sym) x Env -> Type
(define lookup-qualified-var-in-tenv
  (lambda (m-name var-names env)
    (let ((ty (apply-tenv env m-name)))
      (if (null? var-names)
          ty
          (cases type ty
                 (module-type
                  (val-decls)
                  (lookup-qualified-var-in-tenv
                   (car var-names)
                   (cdr var-names)
                   (val-decls->tenv val-decls)))
                 (else
                  (report-no-binding-type-module-found m-name)))))))

;;; val-decls->tenv : Listof(Decl) -> TypeEnv
(define val-decls->tenv
  (lambda (decls)
    (if (null? decls)
        (empty-tenv)
        (cases declaration (car decls)
               (val-decl
                (var ty)
                (extend-tenv var ty
                             (val-decls->tenv (cdr decls))))))))

(define lookup-module-name-in-tenv
  (lambda (env m-name)
    (let ((ty (apply-tenv env m-name)))
      (if (interface? ty)
          ty
          (report-no-binding-type-module-found m-name)))))

(define report-duplicate-id
  (lambda (sym syms)
    (eopl:error 'extend-tenv* "Duplicate identifier ~s in ~a"
                sym syms)))

(define report-type-mismatch
  (lambda (symp vars types)
    (eopl:error 'extend-tenv*
                "Argument number is ~s than parameter number: ~a, ~a"
                symp vars (map type-to-external-form types))))

(define report-no-binding-type-found
  (lambda (search-var)
    (eopl:error 'apply-tenv "No binding for ~s" search-var)))

(define report-no-binding-type-module-found
  (lambda (search-var)
    (eopl:error 'apply-tenv "No binding module for ~s" search-var)))

;;; ---------------------- Type Checker ----------------------
;;; check-equal-type! : Type x Type x Expression -> Unspecified
(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (when (not (equal? ty1 ty2))
      (report-unequal-types ty1 ty2 exp))))
;;; check-equal-types! : Listof(Type) x Listof(Type) x Expression -> Unspecified
(define check-equal-types!
  (lambda (tys1 tys2 exp)
    (if (null? tys1)
        #t
        (if (equal? (car tys1) (car tys2))
            (check-equal-types! (cdr tys1) (cdr tys2) exp)
            (report-unequal-types (car tys1) (car tys2) exp)))))
;;; report-unequal-types : Type x Type x Expression -> Unspecified
(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-equal-type!
                "Types didn't match: ~s != ~s in ~%~a"
                (type-to-external-form ty1)
                (type-to-external-form ty2)
                exp)))
(define type-to-external-form
  (lambda (ty)
    (cases type ty
           (int-type () 'int)
           (bool-type () 'bool)
           (proc-type (arg-types result-type)
                      (list (map type-to-external-form arg-types)
                            '->
                            (type-to-external-form result-type)))
           (module-type
            (val-decls)
            (map declaration-to-external-form val-decls)))))

(define declaration-to-external-form
  (lambda (decl)
    (cases declaration decl
           (val-decl
            (var ty)
            (list var ty)))))

;;; add-module-defns-to-tenv : Listof(ModuleDefn) x TypeEnv -> TypeEnv
(define add-module-defns-to-tenv
  (lambda (m-defns env)
    (if (null? m-defns)
        env
        (cases module-defn (car m-defns)
               (a-module-definition
                (m-name expected-iface m-body)
                (let ((iface (search-module env m-name)))
                  (if iface
                      (report-duplicate-module-defn m-name)
                      (let ((actual-iface (interface-of m-body env)))
                        (if (<:-iface actual-iface expected-iface env)
                            (let ((new-tenv
                                   (extend-tenv
                                    m-name
                                    (interface-to->module-type expected-iface)
                                    env)))
                              (add-module-defns-to-tenv (cdr m-defns) new-tenv))
                            (report-module-doesnt-satisfy-iface
                             m-name expected-iface actual-iface))))))))))

;;; interface-to->module-type : Interface -> Type
(define interface-to->module-type
  (lambda (iface)
    (cases interface iface
           (simple-iface
            (val-decls)
            (module-type val-decls)))))

;;; interface-of : ModuleBody x TypeEnv -> Intreface
(define interface-of
  (lambda (m-body env)
    (cases module-body-top m-body
           (nested-module-body
            (defns body)
            (interface-of-bottom body (add-module-defns-to-tenv defns env))))))

(define interface-of-bottom
  (lambda (m-body env)
    (cases module-body m-body
           (defns-module-body
             (defns)
             (simple-iface (defns-to-decls defns env)))
           (let-module-body
            (b-vars b-exps let-body)
            (let ((b-types (map (lambda (e) (type-of-exp e env)) b-exps)))
              (interface-of-bottom let-body (extend-tenv* b-vars b-types env))))
           (letrec-module-body
            (result-types p-names b-vars arg-types b-bodies letrec-body)
            (let ((letrec-tenv (extend-tenv* p-names
                                             (map proc-type arg-types result-types)
                                             env)))
              (let ((checked-types
                     (map (lambda (e vs as)
                            (type-of-exp e (extend-tenv* vs as letrec-tenv)))
                          b-bodies
                          b-vars
                          arg-types)))
                (check-equal-type! checked-types result-types exp)
                (interface-of-bottom letrec-body letrec-tenv)))))))

;;; defns-to-decls : Listof(Defn) × TypeEnv → Listof(Decl)
(define defns-to-decls
  (lambda (defns env)
    (if (null? defns)
        '()
        (cases definition (car defns)
               (val-defn
                (var exp)
                (let ((actual-type (type-of-exp exp env)))
                  (cons (val-decl var actual-type)
                        (defns-to-decls
                          (cdr defns)
                          (extend-tenv var
                                       actual-type
                                       env)))))))))

;;; <:-iface : Interface x Interface x TypeEnv -> Bool
(define <:-iface
  (lambda (iface1 iface2 tenv)
    (cases interface iface1
           (simple-iface
            (decls1)
            (cases interface iface2
                   (simple-iface
                    (decls2)
                    (<:-decls decls1 decls2 tenv)))))))

;;; <:-iface : Listof(Decl) x Listof(Decl) x TypeEnv -> Bool
(define <:-decls
  (lambda (decls1 decls2 tenv)
    (cond [(null? decls2) #t]
          [(null? decls1) #f]
          [else
           (let ((name1 (decl->name (car decls1)))
                 (name2 (decl->name (car decls2))))
             (if (eqv? name1 name2)
                 (and
                  (equal?
                   (decl->type (car decls1))
                   (decl->type (car decls2)))
                  (<:-decls (cdr decls1) (cdr decls2) tenv))
                 (<:-decls (cdr decls1) decls2 tenv)))])))

(define decl->name
  (lambda (decl)
    (cases declaration decl
           (val-decl
            (var-name var-type)
            var-name))))

(define decl->type
  (lambda (decl)
    (cases declaration decl
           (val-decl
            (var-name var-type)
            var-type))))

(define report-module-doesnt-satisfy-iface
  (lambda (m-name exptected-iface actual-iface)
    (eopl:error
     'add-module-defns-to-tenv
     "Module does not satisfy interface: ~s"
     (list 'error-in-defn-of-module: m-name
           'expected-type: exptected-iface
           'actual-type: actual-iface))))

(define report-duplicate-module-defn
  (lambda (m-name)
    (eopl:error
     'add-module-defns-to-tenv
     "Duplicate module name: ~a~%" m-name)))

(define type-of-program
  (lambda (prgm)
    (cases program prgm
           (a-program
            (m-defns exp)
            (type-of-exp exp (add-module-defns-to-tenv m-defns (empty-tenv)))))))

(define type-of-exp
  (lambda (exp tenv)
    (cases expression exp
           (qualified-var-exp
            (m-name var-name var-names)
            (lookup-qualified-var-in-tenv m-name (cons var-name var-names) tenv))
           (const-exp
            (num)
            (int-type))
           (var-exp
            (var)
            (apply-tenv tenv var))
           (zero?-exp
            (exp1)
            (check-equal-type!
             (int-type)
             (type-of-exp exp1 tenv)
             exp1)
            (bool-type))
           (diff-exp
            (exp1 exp2)
            (check-equal-type!
             (int-type)
             (type-of-exp exp1 tenv)
             exp1)
            (check-equal-type!
             (int-type)
             (type-of-exp exp2 tenv)
             exp2)
            (int-type))
           (if-exp
            (exp1 exp2 exp3)
            (let ((ty1 (type-of-exp exp1 tenv))
                  (ty2 (type-of-exp exp2 tenv))
                  (ty3 (type-of-exp exp3 tenv)))
              (check-equal-type! (bool-type) ty1 exp1)
              (check-equal-type! ty2 ty3 exp)
              ty2))
           (proc-exp
            (vars tys body)
            (let ((result-type (type-of-exp body (extend-tenv* vars tys tenv))))
              (proc-type tys result-type)))
           (let-exp
            (b-vars b-exps let-body)
            (let ((b-types (map (lambda (e) (type-of-exp e tenv)) b-exps)))
              (type-of-exp let-body (extend-tenv* b-vars b-types tenv))))
           (letrec-exp
            (result-types p-names b-vars arg-types b-bodies letrec-body)
            (let ((letrec-tenv (extend-tenv* p-names
                                             (map proc-type arg-types result-types)
                                             tenv)))
              (let ((checked-types
                     (map (lambda (e vs as)
                            (type-of-exp e (extend-tenv* vs as letrec-tenv)))
                          b-bodies
                          b-vars
                          arg-types)))
                (check-equal-type! checked-types result-types exp)
                (type-of-exp letrec-body letrec-tenv))))
           (call-exp
            (rator rands)
            (let ((rator-type (type-of-exp rator tenv))
                  (rand-types (map (lambda (e) (type-of-exp e tenv)) rands)))
              (cases type rator-type
                     (proc-type
                      (arg-types result-type)
                      (check-equal-types! arg-types rand-types exp)
                      result-type)
                     (else
                      (report-rator-not-a-proc-type rator-type rator))))))))

(define report-rator-not-a-proc-type
  (lambda (ty1 exp)
    (eopl:error 'type-of-exp
                "Expect a procedure, actual ~a: ~a"
                (type-to-external-form ty1)
                exp)))

(define type-of
  (lambda (prgm)
    (type-to-external-form (type-of-program prgm))))

;;; checked-type-of : String -> Type | String (for exception)
(require (only-in racket/base with-handlers exn:fail?))
(define checked-type-of
  (lambda (prgm)
    (with-handlers
        [(exn:fail? (lambda (en) 'error))]
      (type-of prgm))))

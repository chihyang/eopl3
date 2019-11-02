#lang eopl
(require "chap08.s02.lang.scm")
(provide checked-type-of type-of type-of-program type-of-exp)

;;; ---------------------- Type Environment ----------------------
(define-datatype tenv tenv?
  (empty-tenv)
  (extend-tenv
   (saved-var identifier?)
   (saved-type type?)
   (saved-tenv tenv?))
  (extend-tenv-with-module
   (name identifier?)
   (interface interface?)
   (saved-env tenv?))
  (extend-tenv-with-type
   (name identifier?)
   (type type?)
   (saved-tenv tenv?)))

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
                (apply-tenv saved-tenv var)))
           (extend-tenv-with-module
            (m-name iface saved-tenv)
            (if (eq? var m-name)
                iface
                (apply-tenv saved-tenv var)))
           (extend-tenv-with-type
            (t-name t-type saved-tenv)
            (if (eq? var t-name)
                t-type
                (apply-tenv saved-tenv var))))))

;;; expand-type : Type x TypeEnv -> ExpandedType
(define expand-type
  (lambda (ty tenv)
    (cases type ty
           (int-type () (int-type))
           (bool-type () (bool-type))
           (proc-type
            (arg-type result-types)
            (proc-type (expand-type arg-type tenv)
                       (expand-type result-type tenv)))
           (named-type
            (name)
            (lookup-type-name-in-tenv tenv name))
           (qualified-type
            (m-name t-name)
            (lookup-qualified-var-in-tenv m-name t-name tenv)))))

;;; lookup-type-name-in-tenv : TypeEnv x Sym -> Type
(define lookup-type-name-in-tenv
  (lambda (tenv var)
    (apply-tenv tenv var)))

;;; lookup-qualified-var-in-tenv : Sym x Sym x TypeEnv -> Type
(define lookup-qualified-var-in-tenv
  (lambda (m-name var-name env)
    (let ((iface (lookup-module-name-in-tenv env m-name)))
      (cases interface iface
             (simple-iface
              (val-decls)
              (lookup-variable-name-in-decls var-name val-decls))))))

;;; lookup-variable-name-in-decls : Sym x Listof(Decl) -> Type
(define lookup-variable-name-in-decls
  (lambda (var-name var-decls)
    (cond [(null? var-decls) (report-no-binding-type-found var-name)]
          [else
           (cases declaration (car var-decls)
                  (val-decl
                   (saved-var saved-type)
                   (if (eq? var-name saved-var)
                       saved-type
                       (lookup-variable-name-in-decls
                        var-name
                        (cdr var-decls))))
                  (opaque-type-decl
                   (name)
                   (lookup-variable-name-in-decls
                    var-name
                    (cdr var-decls)))
                  (transparent-type-decl
                   (name ty)
                   (if (eq? var-name name)
                       ty
                       (lookup-variable-name-in-decls
                        var-name
                        (cdr var-decls)))))])))

(define lookup-module-name-in-tenv
  (lambda (env m-name)
    (let ((ty (apply-tenv env m-name)))
      (if (interface? ty)
          ty
          (report-no-binding-type-module-found m-name)))))

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
           (proc-type (arg-type result-types)
                      (list (type-to-external-form arg-type)
                            '->
                            (type-to-external-form result-type)))
           (named-type
            (name)
            name)
           (qualified-type
                         (name ty)
                         (list name ty)))))

;;; add-module-defns-to-tenv : Listof(ModuleDefn) x TypeEnv -> TypeEnv
(define add-module-defns-to-tenv
  (lambda (m-defns env)
    (if (null? m-defns)
        env
        (cases module-defn (car m-defns)
               (a-module-definition
                (m-name expected-iface m-body)
                (let ((actual-iface (interface-of m-body env)))
                  (if (<:-iface actual-iface expected-iface env)
                      (let ((new-tenv
                             (extend-tenv-with-module
                              m-name
                              (expand-iface
                               m-name expected-iface env)
                              env)))
                        (add-module-defns-to-tenv (cdr m-defns) new-tenv))
                      (report-module-doesnt-satisfy-iface
                       m-name expected-iface actual-iface))))))))

;;; interface-of : ModuleBody x TypeEnv -> Intreface
(define interface-of
  (lambda (m-body env)
    (cases module-body m-body
           (defns-module-body
             (defns)
             (simple-iface (defns-to-decls defns env))))))

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
                                       env)))))
               (type-defn
                (name ty)
                (let ((new-env (extend-tenv-with-type
                                name (expand-type ty env) env)))
                  (cons (transparent-type-decl name ty)
                        (defns-to-decls (cdr defns) new-env))))))))

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
                  (<:-decl
                   (car decls1) (car decls2) tenv)
                  (<:-decls
                   (cdr decls1) (cdr decls2)
                   (extend-tenv-with-decl
                    (car decls1) tenv)))
                 (<:-decls
                  (cdr decls1) decls2
                  (extend-tenv-with-decl
                   (car decls1) tenv))))])))

;;; extend-tenv-with-decl : Decl x TypeEnv -> TypeEnv
(define extend-tenv-with-decl
  (lambda (decl tenv)
    (cases declaration decl
           (val-decl (name ty) tenv)
           (opaque-type-decl
            (name)
            (extend-tenv-with-type
             name
             (qualified-type (fresh-module-name '%unknown) name)
             tenv))
           (transparent-type-decl
            (name ty)
            (extend-tenv-with-type
             name
             (expand-type ty tenv)
             tenv)))))

;;; <:-decl : Decl x Decl x TypeEnv -> Bool
(define <:-decl
  (lambda (decl1 decl2 tenv)
    (or
     (and
      (val-decl? decl1)
      (val-decl? decl2)
      (equiv-type?
       (decl->type decl1)
       (decl->type decl2)
       tenv))
     (and
      (transparent-type-decl? decl1)
      (transparent-type-decl? decl2)
      (equiv-type?
       (decl->type decl1)
       (decl->type decl2)
       tenv))
     (and
      (transparent-type-decl? decl1)
      (opaque-type-decl? decl2))
     (and
      (opaque-type-decl? decl1)
      (opaque-type-decl? decl2)))))

;;; equiv-type? : Type x Type x TypeEnv -> Bool
(define equiv-type?
  (lambda (t1 t2 tenv)
    (equal?
     (expand-type t1 tenv)
     (expand-type t2 tenv))))

;;; fresh-module-name : Sym -> Sym
(define module-name-sn 0)
(define fresh-module-name
  (lambda (name)
    (set! module-name-sn (+ module-name-sn 1))
    (string->symbol
     (string-append
      (symbol->string name)
      "%"
      (number->string module-name-sn)))))

(define val-decl?
  (lambda (decl)
    (cases declaration decl
           (val-decl
            (var-name var-type)
            #t)
           (else #f))))

(define transparent-type-decl?
  (lambda (decl)
    (cases declaration decl
           (transparent-type-decl
            (name ty)
            #t)
           (else #f))))

(define opaque-type-decl?
  (lambda (decl)
    (cases declaration decl
           (opaque-type-decl
            (name)
            #t)
           (else #f))))

(define decl->name
  (lambda (decl)
    (cases declaration decl
           (val-decl
            (var-name var-type)
            var-name)
           (transparent-type-decl
            (name ty)
            name)
           (opaque-type-decl
            (name)
            name))))

(define decl->type
  (lambda (decl)
    (cases declaration decl
           (val-decl
            (var-name var-type)
            var-type)
           (transparent-type-decl
            (name ty)
            ty)
           (opaque-type-decl
            (name)
            (named-type name)))))

;;; expand-iface : Sym x Iface x TypeEnv -> Iface
(define expand-iface
  (lambda (m-name iface tenv)
    (cases interface iface
           (simple-iface
            (decls)
            (simple-iface
             (expand-decls m-name decls tenv))))))

;;; expand-decls : Sym x Listof(Decl) x TypeEnv -> Listof(Decl)
(define expand-decls
  (lambda (m-name decls tenv)
    (if (null? decls)
        '()
        (cases declaration (car decls)
                  (val-decl
                   (saved-var saved-type)
                   (let ((expanded-type
                          (expand-type saved-type tenv)))
                     (cons (val-decl saved-var expanded-type)
                           (expand-decls m-name (cdr decls) tenv))))
                  (opaque-type-decl
                   (t-name)
                   (let ((expanded-type
                          (qualified-type m-name t-name)))
                     (let ((new-tenv
                            (extend-tenv-with-type t-name expanded-type tenv)))
                       (cons (transparent-type-decl t-name expanded-type)
                             (expand-decls m-name (cdr decls) new-tenv)))))
                  (transparent-type-decl
                   (t-name t-type)
                   (let ((expanded-type
                          (expand-type t-type tenv)))
                     (let ((new-tenv
                            (extend-tenv-with-type t-name expanded-type tenv)))
                       (cons (transparent-type-decl t-name expanded-type)
                             (expand-decls m-name (cdr decls) new-tenv)))))))))

(define report-module-doesnt-satisfy-iface
  (lambda (m-name exptected-iface actual-iface)
    (eopl:error
     'add-module-defns-to-tenv
     "Module does not satisfy interface: ~s"
     (list 'error-in-defn-of-module: m-name
           'expected-type: exptected-iface
           'actual-type: actual-iface))))

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
            (m-name var-name)
            (lookup-qualified-var-in-tenv m-name var-name tenv))
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
            (let ((arg-type (expand-type ty tenv)))
              (let ((result-type (type-of-exp body (extend-tenv var arg-type tenv))))
                (proc-type arg-type result-type))))
           (let-exp
            (b-vars b-exps let-body)
            (let ((b-type (type-of-exp b-exp tenv)))
              (type-of-exp let-body (extend-tenv b-var (expand-type b-type tenv) tenv))))
           (letrec-exp
            (result-types p-names b-vars arg-types b-bodies letrec-body)
            (let ((letrec-tenv (extend-tenv p-name
                                            (expand-type (proc-type arg-type result-type) tenv)
                                            tenv)))
              (let ((checked-type (type-of-exp
                                   b-body
                                   (extend-tenv b-var (expand-type arg-type letrec-tenv) letrec-tenv))))
                (check-equal-type! checked-type result-type exp)
                (type-of-exp letrec-body letrec-tenv))))
           (call-exp
            (rator rands)
            (let ((rator-type (type-of-exp rator tenv))
                  (rand-type (type-of-exp rand tenv)))
              (cases type rator-type
                     (proc-type
                      (arg-type result-type)
                      (check-equal-type! arg-type rand-type exp)
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

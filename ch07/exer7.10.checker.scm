#lang eopl
(require "exer7.10.lang.scm")
(provide checked-type-of type-of type-of-program type-of-exp)

;;; ---------------------- Type Environment ----------------------
(define-datatype tenv tenv?
  (empty-tenv)
  (extend-tenv
   (saved-var identifier?)
   (saved-type type?)
   (saved-tenv tenv?)))

(define extend-tenv*
  (lambda (vars types tenv)
    (letrec ((extend-tenv*-inner
              (lambda (vars types tenv)
                (if (null? vars)
                    tenv
                    (extend-tenv*-inner (cdr vars)
                                        (cdr types)
                                        (extend-tenv (car vars) (car types) tenv))))))
      (let ((var-len (length vars))
            (type-len (length types)))
        (cond [(< var-len type-len)
               (report-type-var-number-mismatch 'greater)]
              [(> var-len type-len)
               (report-type-var-number-mismatch 'less)]
              [else
               (extend-tenv*-inner vars types tenv)])))))

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

(define report-no-binding-type-found
  (lambda (search-var)
    (eopl:error 'apply-tenv "No binding for ~s" search-var)))

(define report-type-var-number-mismatch
  (lambda (symp vars types)
    (eopl:error 'extend-tenv* "type number is ~s than variable number in:~%~a, ~a"
                symp vars (map type-to-external-form types))))

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
           (list-type (type)
                      (list 'listof (type-to-external-form type)))
           (ref-type (type)
                     (list 'refto (type-to-external-form type)))
           (void-type () 'void))))

(define type-of-program
  (lambda (prgm)
    (cases program prgm
           (a-program
            (exp)
            (type-of-exp exp (empty-tenv))))))

(define type-of-exp
  (lambda (exp tenv)
    (cases expression exp
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
            (let ((b-types (type-of-exps b-exps tenv)))
              (type-of-exp let-body (extend-tenv* b-vars b-types tenv))))
           (letrec-exp
            (result-types p-names b-vars arg-types b-bodies letrec-body)
            (let ((letrec-tenv
                   (extend-tenv*
                    p-names
                    (map proc-type arg-types result-types)
                    tenv)))
              (let ((checked-types
                     (map (lambda (b-body b-var arg-type)
                            (type-of-exp
                             b-body
                             (extend-tenv* b-var arg-type letrec-tenv)))
                          b-bodies
                          b-vars
                          arg-types)))
                (check-equal-types! checked-types result-types exp)
                (type-of-exp letrec-body letrec-tenv))))
           (call-exp
            (rator rands)
            (let ((rator-type (type-of-exp rator tenv))
                  (rand-types (type-of-exps rands tenv)))
              (cases type rator-type
                     (proc-type
                      (arg-types result-type)
                      (check-equal-types! arg-types rand-types exp)
                      result-type)
                     (else
                      (report-rator-not-a-proc-type rator-type rator)))))
           (list-exp
            (exp1 exps)
            (let ((ty1 (type-of-exp exp1 tenv))
                  (tys (type-of-exps exps tenv)))
              (map
               (lambda (ty) (check-equal-type! ty ty1 exp))
               tys)
              (list-type ty1)))
           (cons-exp
            (exp1 exp2)
            (let ((ty1 (type-of-exp exp1 tenv))
                  (ty2 (type-of-exp exp2 tenv)))
              (check-equal-type! (list-type ty1) ty2 exp)
              ty2))
           (null-exp
            (exp1)
            (let ((ty1 (type-of-exp exp1 tenv)))
              (cases type ty1
                     (list-type
                      (ty)
                      (bool-type))
                     (else
                      (report-exp-not-a-list-type ty1 exp1)))))
           (emptylist-exp
            (ty)
            (list-type ty))
           (car-exp
            (exp1)
            (let ((ty1 (type-of-exp exp1 tenv)))
              (cases type ty1
                     (list-type
                      (ty)
                      ty)
                     (else
                      (report-exp-not-a-list-type ty1 exp1)))))
           (cdr-exp
            (exp1)
            (let ((ty1 (type-of-exp exp1 tenv)))
              (cases type ty1
                     (list-type
                      (ty)
                      ty1)
                     (else
                      (report-exp-not-a-list-type ty1 exp1)))))
           (newref-exp
            (exp1)
            (let ((ty1 (type-of-exp exp1 tenv)))
              (ref-type ty1)))
           (deref-exp
            (exp1)
            (let ((ty1 (type-of-exp exp1 tenv)))
              (cases type ty1
                     (ref-type
                      (ty1)
                      ty1)
                     (else
                      (report-exp-not-a-ref-type ty1 exp1)))))
           (setref-exp
            (exp1 exp2)
            (let ((ty1 (type-of-exp exp1 tenv))
                  (ty2 (type-of-exp exp2 tenv)))
              (cases type ty1
                     (ref-type
                      (ty)
                      (check-equal-type! ty ty2 exp)
                      (void-type))
                     (else
                      (report-exp-not-a-ref-type ty1 exp1)))))
           (begin-exp
            (exp1 exps)
            (if (null? exps)
                (type-of-exp exp1 tenv)
                (begin
                  ;; is it necessary to evaluate type of exp1?
                  (type-of-exp exp1 tenv)
                  (type-of-exp (begin-exp (car exps) (cdr exps)) tenv)))))))

(define report-rator-not-a-proc-type
  (lambda (ty1 exp)
    (eopl:error 'type-of-exp
                "Expect a procedure, actual ~a: ~a"
                (type-to-external-form ty1)
                exp)))

(define report-exp-not-a-list-type
  (lambda (ty1 exp)
    (eopl:error 'type-of-exp
                "Expect a list, actual ~a: ~a"
                (type-to-external-form ty1)
                exp)))

(define report-exp-not-a-ref-type
  (lambda (ty1 exp)
    (eopl:error 'type-of-exp
                "Expect a reference, actual ~a: ~a"
                (type-to-external-form ty1)
                exp)))

(define type-of-exps
  (lambda (exps tenv)
    (if (null? exps)
        '()
        (cons (type-of-exp (car exps) tenv)
              (type-of-exps (cdr exps) tenv)))))

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

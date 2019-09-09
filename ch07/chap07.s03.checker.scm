#lang eopl
(require "chap07.s03.lang.scm")
(provide type-of-program type-of-exp)

;;; ---------------------- Type Environment ----------------------
(define-datatype tenv tenv?
  (empty-tenv)
  (extend-tenv
   (saved-var identifier?)
   (saved-type type?)
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
                (apply-tenv saved-tenv var))))))

(define report-no-binding-type-found
  (lambda (search-var)
    (eopl:error 'apply-tenv "No binding for ~s" search-var)))

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
           (proc-type (arg-type result-type)
                      (list (type-to-external-form arg-type)
                            '->
                            (type-to-external-form result-type))))))

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
            (var ty body)
            (let ((result-type (type-of-exp body (extend-tenv var ty tenv))))
              (proc-type ty result-type)))
           (let-exp
            (b-var b-exp let-body)
            (let ((b-type (type-of-exp b-exp tenv)))
              (type-of-exp let-body (extend-tenv b-var b-type tenv))))
           (letrec-exp
            (result-type p-name b-var arg-type b-body letrec-body)
            (let ((letrec-tenv (extend-tenv p-name
                                            (proc-type arg-type result-type)
                                            tenv)))
              (let ((checked-type (type-of-exp
                                   b-body
                                   (extend-tenv b-var arg-type letrec-tenv))))
                (check-equal-type! checked-type result-type exp)
                (type-of-exp letrec-body letrec-tenv))))
           (call-exp
            (rator rand)
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

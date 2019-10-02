#lang eopl
(require "chap07.s04.lang.scm")
(provide checked-type-of type-of type-of-program type-of-exp equal-types?)

;;; ---------------------- Type Environment ----------------------
(define-datatype tenv tenv?
  (empty-tenv)
  (extend-tenv
   (saved-var identifier?)
   (saved-type type?)
   (saved-tenv tenv?)))

;;; init-tenv : () -> Type
(define init-tenv
  (lambda ()
    (empty-tenv)))

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

;;; ---------------------- Substitution ----------------------
;;; apply-one-subst : Type x Tvar x Type -> Type
(define apply-one-subst
  (lambda (ty0 tvar ty1)
    (cases type ty0
           (int-type () ty0)
           (bool-type () ty0)
           (proc-type
            (tv tb)
            (proc-type
             (apply-one-subst tv tvar ty1)
             (apply-one-subst tb tvar ty1)))
           (tvar-type
            (sn)
            (if (equal? ty0 tvar) ty1 ty0)))))

;;; apply-subst-to-type : Type x Subst -> Type
(define apply-subst-to-type
  (lambda (ty subst)
    (cases type ty
           (int-type () (int-type))
           (bool-type () (bool-type))
           (proc-type
            (t1 t2)
            (proc-type
             (apply-subst-to-type t1 subst)
             (apply-subst-to-type t2 subst)))
           (tvar-type
            (sn)
            (let ((tmp (assoc ty subst)))
              (if tmp
                  (cdr tmp)
                  ty))))))

;;; emtpty-subst : () -> Subst
(define substitution?
  (list-of
   (lambda (t)
     (if (pair? t)
         (and
          (tvar-type? (car t))
          (type? (cdr t)))
         #f))))

;;; emtpty-subst : () -> Subst
(define empty-subst
  (lambda () '()))

;;; extend-subst : Subst x Tvar x Type -> Subst
(define extend-subst
  (lambda (subst tvar ty)
    (cons (cons tvar ty)
          (map (lambda (p)
                 (let ((oldlhs (car p))
                       (oldrhs (cdr p)))
                   (cons oldlhs
                         (apply-one-subst oldrhs tvar ty))))
               subst))))

;;; ---------------------- Unifier ----------------------
;;; unifier : Type x Type x Subst x Exp -> Subst
(define unifier
  (lambda (ty1 ty2 subst exp)
    (let ((ty1 (apply-subst-to-type ty1 subst))
          (ty2 (apply-subst-to-type ty2 subst)))
      (cond [(equal? ty1 ty2) subst]
            [(tvar-type? ty1)
             (if (no-occurrence? ty1 ty2)
                 (extend-subst subst ty1 ty2)
                 (report-no-occurrence-violation ty1 ty2 exp))]
            [(tvar-type? ty2)
             (if (no-occurrence? ty2 ty1)
                 (extend-subst subst ty2 ty1)
                 (report-no-occurrence-violation ty2 ty1 exp))]
            [(and (proc-type? ty1) (proc-type? ty2))
             (let ((subst (unifier (proc-type->arg-type ty1)
                                   (proc-type->arg-type ty2)
                                   subst exp)))
               (let ((subst (unifier (proc-type->result-type ty1)
                                     (proc-type->result-type ty2)
                                     subst exp)))
                 subst))]
            [else (report-unification-failure ty1 ty2 exp)]))))

;;; no-occurrence? : Type x Type -> Bool
(define no-occurrence?
  (lambda (ty1 ty2)
    (cases type ty2
           (int-type () #t)
           (bool-type () #t)
           (proc-type
            (t1 t2)
            (and (no-occurrence? ty1 t1)
                 (no-occurrence? ty1 t2)))
           (tvar-type
            (sn)
            (not (equal? ty1 ty2))))))

;;; tvar-type? : Type -> Bool
(define tvar-type?
  (lambda (ty)
    (cases type ty
           (tvar-type (sn) #t)
           (else #f))))

;;; proc-type? : Type -> Bool
(define proc-type?
  (lambda (ty)
    (cases type ty
           (proc-type (t1 t2) #t)
           (else #f))))

;;; proc-type->arg-type : Type -> Type
(define proc-type->arg-type
  (lambda (ty)
    (cases type ty
           (proc-type (t1 t2) t1)
           (else #f))))

;;; proc-type->result-type : Type -> Type
(define proc-type->result-type
  (lambda (ty)
    (cases type ty
           (proc-type (t1 t2) t2)
           (else #f))))

(define report-unification-failure
  (lambda (ty1 ty2 exp)
    (eopl:error 'unification-failure
                "Type mismatch: ~s doesn't match ~s in ~s~%"
                (type-to-external-form ty1)
                (type-to-external-form ty2)
                exp)))

(define report-no-occurrence-violation
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-no-occurence!
                "Can't unify: type variable ~s occurs in type ~s in expression ~s~%"
                (type-to-external-form ty1)
                (type-to-external-form ty2)
                exp)))

;;; ---------------------- Type Inference ----------------------
;;; optype->type : OptinalType -> Type
(define optype->type
  (lambda (otype)
    (cases optional-type otype
           (no-type () (fresh-tvar-type))
           (a-type (ty) ty))))

;;; fresh-tvar-type : () -> Type
(define serial-number 0)
(define fresh-tvar-type
  (lambda ()
    (set! serial-number (+ serial-number 1))
    (tvar-type serial-number)))

;;; type-to-external-form : Type -> List
(define type-to-external-form
  (lambda (ty)
    (cases type ty
           (int-type () 'int)
           (bool-type () 'bool)
           (proc-type (arg-type result-type)
                      (list (type-to-external-form arg-type)
                            '->
                            (type-to-external-form result-type)))
           (tvar-type (sn)
                      (string->symbol
                       (string-append
                        "ty"
                        (number->string sn)))))))

;;; Equation = Type x Type x Exp
(define-datatype equation equation?
  (an-equation
   (lhs type?)
   (rhs type?)
   (exp expression?)))

;;; type-of-program : Program -> Listof(Equation)
(define equations-of-program
  (lambda (prgm)
    (set! serial-number 0)
    (cases program prgm
           (a-program
            (exp)
            (let ((tvar (fresh-tvar-type)))
              (equations-of-exp exp tvar (init-tenv)))))))

;;; equations-of-exp : Exp x Tvar x Tenv -> Listof(Equation)
(define equations-of-exp
  (lambda (exp tvar tenv)
    (cases expression exp
           (const-exp
            (num)
            (list (an-equation tvar (int-type) exp)))
           (zero?-exp
            (exp1)
            (let ((tvar1 (fresh-tvar-type)))
              (append
               (list
                (an-equation tvar (bool-type) exp)
                (an-equation tvar1 (int-type) exp))
               (equations-of-exp exp1 tvar1 tenv))))
           (diff-exp
            (exp1 exp2)
            (let ((tvar1 (fresh-tvar-type))
                  (tvar2 (fresh-tvar-type)))
              (append
               (list
                (an-equation tvar (int-type) exp)
                (an-equation tvar1 (int-type) exp)
                (an-equation tvar2 (int-type) exp))
               (equations-of-exp exp1 tvar1 tenv)
               (equations-of-exp exp2 tvar2 tenv))))
           (if-exp
            (exp1 exp2 exp3)
            (let ((tvar1 (fresh-tvar-type))
                  (tvar2 (fresh-tvar-type))
                  (tvar3 (fresh-tvar-type)))
              (append
               (list
                (an-equation tvar1 (bool-type) exp)
                (an-equation tvar tvar2 exp)
                (an-equation tvar tvar3 exp))
               (equations-of-exp exp1 tvar1 tenv)
               (equations-of-exp exp2 tvar2 tenv)
               (equations-of-exp exp3 tvar3 tenv))))
           (var-exp
            (var)
            (list (an-equation tvar (apply-tenv tenv var) exp)))
           (let-exp
            (b-var b-exp let-body)
            (let ((tvar-bvar (fresh-tvar-type))
                  (tvar-bexp (fresh-tvar-type))
                  (tvar-body (fresh-tvar-type)))
              (append
               (list
                (an-equation tvar tvar-body exp)
                (an-equation tvar-bvar tvar-bexp exp))
               (equations-of-exp b-exp tvar-bexp tenv)
               (equations-of-exp let-body tvar-body (extend-tenv b-var tvar-bvar tenv)))))
           (proc-exp
            (var opty body)
            (let ((arg-type (optype->type opty))
                  (result-type (fresh-tvar-type)))
              (append
               (list
                (an-equation tvar (proc-type arg-type result-type) exp))
               (equations-of-exp body result-type (extend-tenv var arg-type tenv)))))
           (letrec-exp
            (result-otype p-name b-var arg-otype b-body letrec-body)
            (let ((result-type (optype->type result-otype))
                  (arg-type (optype->type arg-otype))
                  (letrec-body-type (fresh-tvar-type)))
              (let ((tenv-for-letrec-body
                     (extend-tenv p-name
                                  (proc-type arg-type result-type)
                                  tenv)))
                (append
                 (list
                  (an-equation tvar letrec-body-type exp))
                 (equations-of-exp b-body result-type
                                   (extend-tenv b-var arg-type tenv-for-letrec-body))
                 (equations-of-exp letrec-body letrec-body-type tenv-for-letrec-body)))))
           (call-exp
            (rator rand)
            (let ((rator-type (fresh-tvar-type))
                  (result-type (fresh-tvar-type)))
              (append
               (list
                (an-equation rator-type (proc-type result-type tvar) exp))
               (equations-of-exp rator rator-type tenv)
               (equations-of-exp rand result-type tenv))))
           )))

;;; type-of-program : Program -> Type
(define type-of-program
  (lambda (prgm)
    (set! serial-number 0)
    (cases program prgm
           (a-program
            (exp)
            (let ((tvar (fresh-tvar-type)))
              (let ((equas
                     (equations-of-exp exp tvar (init-tenv))))
                (let ((subst (type-of-exp equas (empty-subst))))
                  (apply-subst-to-type tvar subst))))))))

;;; type-of-exp : Listof(Equation) x Subst -> Subst
(define type-of-exp
  (lambda (equas subst)
    (if (null? equas)
        subst
        (cases equation (car equas)
               (an-equation
                (lhs rhs exp)
                (let ((subst (unifier lhs rhs subst exp)))
                  (type-of-exp (cdr equas) subst)))))))

;;; ---------------------- Gensysm ----------------------
;;; TvarTypeSym = a symbol ending with a digit
;;; A-list = Listof(Pair(TvarTypeSym, TvarTypeSym))

(define equal-types?
  (lambda (ty1 ty2)
    (equal-up-to-gensyms? ty1 ty2)))

;;; equal-up-to-gensyms? : S-exp x S-exp -> Bool
(define equal-up-to-gensyms?
  (lambda (sexp1 sexp2)
    (equal?
     (apply-subst-to-sexp (canonical-subst sexp1) sexp1)
     (apply-subst-to-sexp (canonical-subst sexp2) sexp2))))

;;; canonical-subst : S-exp -> A-list
(define canonical-subst
  (lambda (sexp)
    ;; loop : S-exp x A-list -> A-list
    (let loop ((sexp sexp)
               (table '()))
      (cond [(null? sexp) table]
            [(tvar-type-sym? sexp)
             (cond
              [(assq sexp table) table]
              [else
               (cons
                (cons sexp (ctr->ty (length table)))
                table)])]
            [(pair? sexp)
             (loop (cdr sexp)
                   (loop (car sexp) table))]
            [else table]))))

;;; tvar-type-sym? : Sym -> Bool
(define tvar-type-sym?
  (lambda (sym)
    (and (symbol? sym)
         (char-numeric? (car (reverse (symbol->list sym)))))))

;;; symbol->list : Sym -> List
(define symbol->list
  (lambda (x)
    (string->list (symbol->string x))))

;;; apply-subst-to-sexp : A-list x S-exp -> S-exp
(define apply-subst-to-sexp
  (lambda (subst sexp)
    (cond
     [(null? sexp) sexp]
     [(tvar-type-sym? sexp)
      (cdr (assq sexp subst))]
     [(pair? sexp)
      (cons (apply-subst-to-sexp subst (car sexp))
            (apply-subst-to-sexp subst (cdr sexp)))]
     [else sexp])))

;;; ctr-> ty : N -> Sym
(define ctr->ty
  (lambda (n)
    (string->symbol (string-append "tvar" (number->string n)))))

;;; ---------------------- Interface ----------------------
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

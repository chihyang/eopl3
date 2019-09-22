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
(define the-subst 'uninitialized)

;;; subst-initialize! : () -> Unspecified
(define subst-initialize!
  (lambda ()
    (set! the-subst (empty-subst))))

;;; get-subst : () -> Subst
(define get-subst
  (lambda () the-subst))

;;; apply-subst-to-type : Type x Subst -> Type
(define apply-subst-to-type
  (lambda (ty)
    (cases type ty
           (int-type () (int-type))
           (bool-type () (bool-type))
           (proc-type
            (t1 t2)
            (proc-type
             (apply-subst-to-type t1)
             (apply-subst-to-type t2)))
           (tvar-type
            (sn)
            (let ((tmp (subst-of ty)))
              (if tmp
                  (apply-subst-to-type (cdr tmp))
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

;;; extend-subst! : Tvar x Type -> Subst
(define extend-subst!
  (lambda (tvar ty)
    (set! the-subst (cons (cons tvar ty) the-subst))))

;;; subst-of : Tvar -> Type
(define subst-of
  (lambda (tvar)
    (assoc tvar the-subst)))

;;; ---------------------- Unifier ----------------------
;;; unifier : Type x Type x Exp -> Unspecified
(define unifier
  (lambda (ty1 ty2 exp)
    (let ((ty1
           (if (tvar-type? ty1) (apply-subst-to-type ty1) ty1))
          (ty2
           (if (tvar-type? ty2) (apply-subst-to-type ty2) ty2)))
      (cond [(equal? ty1 ty2) #t]
            [(tvar-type? ty1)
             (if (no-occurrence? ty1 ty2)
                 (extend-subst! ty1 ty2)
                 (report-no-occurrence-violation ty1 ty2 exp))]
            [(tvar-type? ty2)
             (if (no-occurrence? ty2 ty1)
                 (extend-subst! ty2 ty1)
                 (report-no-occurrence-violation ty2 ty1 exp))]
            [(and (proc-type? ty1) (proc-type? ty2))
             (unifier (proc-type->arg-type ty1)
                      (proc-type->arg-type ty2)
                      exp)
             (unifier (proc-type->result-type ty1)
                      (proc-type->result-type ty2)
                      exp)]
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

;;; type-of-program : Program -> Type
(define type-of-program
  (lambda (prgm)
    (set! serial-number 0)
    (subst-initialize!)
    (cases program prgm
           (a-program
            (exp)
            (apply-subst-to-type (type-of-exp exp (init-tenv)))))))

;;; type-of-exp : Exp x Tenv -> Type
(define type-of-exp
  (lambda (exp tenv)
    (cases expression exp
           (const-exp
            (num)
            (int-type))
           (zero?-exp
            (exp1)
            (let ((ty1 (type-of-exp exp1 tenv)))
              (unifier ty1 (int-type) exp)
              (bool-type)))
           (diff-exp
            (exp1 exp2)
            (let ((ty1 (type-of-exp exp1 tenv)))
              (unifier ty1 (int-type) exp1)
              (let ((ty2 (type-of-exp exp2 tenv)))
                (unifier ty2 (int-type) exp2)
                (int-type))))
           (if-exp
            (exp1 exp2 exp3)
            (let ((ty1 (type-of-exp exp1 tenv)))
              (unifier ty1 (bool-type) exp1)
              (let ((ty2 (type-of-exp exp2 tenv)))
                (let ((ty3 (type-of-exp exp3 tenv)))
                  (unifier ty2 ty3 exp)
                  ty2))))
           (var-exp
            (var)
            (apply-tenv tenv var))
           (let-exp
            (b-var b-exp let-body)
            (let ((ty1 (type-of-exp b-exp tenv)))
              (type-of-exp let-body (extend-tenv b-var ty1 tenv))))
           (proc-exp
            (var opty body)
            (let ((arg-type (optype->type opty)))
              (let ((result-type
                     (type-of-exp body
                                  (extend-tenv var arg-type tenv))))
                (proc-type arg-type result-type))))
           (letrec-exp
            (result-otype p-name b-var arg-otype b-body letrec-body)
            (let ((result-type (optype->type result-otype))
                  (arg-type (optype->type arg-otype)))
              (let ((tenv-for-letrec-body (extend-tenv p-name
                                                       (proc-type arg-type result-type)
                                                       tenv)))
                (let ((p-body-type
                       (type-of-exp b-body
                                    (extend-tenv b-var
                                                 arg-type
                                                 tenv-for-letrec-body))))
                  (unifier p-body-type result-type b-body)
                  (type-of-exp letrec-body tenv-for-letrec-body)))))
           (call-exp
            (rator rand)
            (let ((result-type (fresh-tvar-type)))
              (let ((rator-type (type-of-exp rator tenv)))
                (let ((rand-type (type-of-exp rand tenv)))
                  (unifier rator-type
                           (proc-type rand-type result-type)
                           exp)
                  result-type)))
            ))))

(define report-rator-not-a-proc-type
  (lambda (ty1 exp)
    (eopl:error 'type-of-exp
                "Expect a procedure, actual ~a: ~a"
                (type-to-external-form ty1)
                exp)))

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

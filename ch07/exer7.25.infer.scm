#lang eopl
(require "exer7.25.lang.scm")
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
           (list-type
            (te)
            (list-type
             (apply-one-subst te tvar ty1)))
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
           (list-type
            (te)
            (list-type
             (apply-subst-to-type te subst)))
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
            [(and (list-type? ty1) (list-type? ty2))
             (unifier (list-type->elem-type ty1)
                      (list-type->elem-type ty2)
                      subst
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
           (list-type
            (te)
            (no-occurrence? ty1 te))
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

;;; list-type? : Type -> Bool
(define list-type?
  (lambda (ty)
    (cases type ty
           (list-type (t1) #t)
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

;;; list-type->elem-type : Type -> Type
(define list-type->elem-type
  (lambda (ty)
    (cases type ty
           (list-type (te) te)
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
           (list-type
            (te)
            (list 'listof
                  (type-to-external-form te)))
           (tvar-type (sn)
                      (string->symbol
                       (string-append
                        "ty"
                        (number->string sn)))))))

;;; Answer = Type x Subst
(define-datatype answer answer?
  (an-answer
   (ty type?)
   (subst substitution?)))

;;; type-of-program : Program -> Type
(define type-of-program
  (lambda (prgm)
    (set! serial-number 0)
    (cases program prgm
           (a-program
            (exp)
            (cases answer
                   (type-of-exp exp
                                (init-tenv)
                                (empty-subst))
                   (an-answer (ty subst)
                              (apply-subst-to-type ty subst)))))))

;;; type-of-exp : Exp x Tenv x Subst -> Answer
(define type-of-exp
  (lambda (exp tenv subst)
    (cases expression exp
           (const-exp
            (num)
            (an-answer (int-type) subst))
           (zero?-exp
            (exp1)
            (cases answer (type-of-exp exp1 tenv subst)
                   (an-answer
                    (ty1 subst1)
                    (let ((subst2
                           (unifier ty1 (int-type) subst1 exp)))
                      (an-answer (bool-type) subst2)))))
           (diff-exp
            (exp1 exp2)
            (cases answer (type-of-exp exp1 tenv subst)
                   (an-answer
                    (ty1 subst1)
                    (let ((subst1
                           (unifier ty1 (int-type) subst1 exp1)))
                      (cases answer (type-of-exp exp2 tenv subst1)
                             (an-answer
                              (ty2 subst2)
                              (let ((subst2
                                     (unifier ty2 (int-type) subst2 exp2)))
                                (an-answer (int-type) subst2))))))))
           (if-exp
            (exp1 exp2 exp3)
            (cases answer (type-of-exp exp1 tenv subst)
                   (an-answer
                    (ty1 subst1)
                    (let ((subst1 (unifier ty1 (bool-type) subst1 exp1)))
                      (cases answer (type-of-exp exp2 tenv subst1)
                             (an-answer
                              (ty2 subst2)
                              (cases answer (type-of-exp exp3 tenv subst2)
                                     (an-answer
                                      (ty3 subst3)
                                      (let ((subst4 (unifier ty2 ty3 subst3 exp)))
                                        (an-answer ty2 subst4))))))))))
           (var-exp
            (var)
            (an-answer (apply-tenv tenv var) subst))
           (let-exp
            (b-var b-exp let-body)
            (cases answer (type-of-exp b-exp tenv subst)
                   (an-answer
                    (ty1 subst1)
                    (type-of-exp let-body
                                 (extend-tenv b-var ty1 tenv)
                                 subst1))))
           (proc-exp
            (var opty body)
            (let ((arg-type (optype->type opty)))
              (cases answer (type-of-exp body
                                         (extend-tenv var arg-type tenv)
                                         subst)
                     (an-answer
                      (result-type subst)
                      (an-answer (proc-type arg-type result-type)
                                 subst)))))
           (letrec-exp
            (result-otype p-name b-var arg-otype b-body letrec-body)
            (let ((result-type (optype->type result-otype))
                  (arg-type (optype->type arg-otype)))
              (let ((tenv-for-letrec-body
                     (extend-tenv p-name
                                  (proc-type arg-type result-type)
                                  tenv)))
                (cases answer (type-of-exp b-body
                                           (extend-tenv b-var
                                                        arg-type
                                                        tenv-for-letrec-body)
                                           subst)
                       (an-answer
                        (p-body-type subst)
                        (let ((subst (unifier p-body-type result-type subst b-body)))
                          (type-of-exp letrec-body tenv-for-letrec-body subst)))))))
           (call-exp
            (rator rand)
            (let ((result-type (fresh-tvar-type)))
              (cases answer (type-of-exp rator tenv subst)
                     (an-answer
                      (rator-type subst)
                      (cases answer (type-of-exp rand tenv subst)
                             (an-answer
                              (rand-type subst)
                              (let ((subst (unifier rator-type
                                                    (proc-type rand-type result-type)
                                                    subst
                                                    exp)))
                                (an-answer result-type subst))))))))
           (list-exp
            (exp1 exps)
            (cases answer (type-of-exp exp1 tenv subst)
                   (an-answer
                    (ty1 subst)
                    (if (null? exps)
                        (an-answer (list-type ty1) subst)
                        (cases answer (type-of-exp (list-exp (car exps)
                                                             (cdr exps))
                                                   tenv
                                                   subst)
                               (an-answer
                                (ty2 subst)
                                (let ((subst (unifier (list-type ty1) ty2 subst exp)))
                                  (an-answer ty2 subst))))))))
           (cons-exp
            (exp1 exp2)
            (cases answer (type-of-exp exp1 tenv subst)
                   (an-answer
                    (ty1 subst)
                    (cases answer (type-of-exp exp2 tenv subst)
                           (an-answer
                            (ty2 subst)
                            (let ((subst (unifier (list-type ty1) ty2 subst exp)))
                              (an-answer ty2 subst)))))))
           (null-exp
            (exp1)
            (cases answer (type-of-exp exp1 tenv subst)
                   (an-answer
                    (ty1 subst)
                    (cases type ty1
                           (list-type
                            (et)
                            (an-answer (bool-type) subst))
                           (else
                            (report-rator-not-a-list-type ty1 exp1))))))
           (emptylist-exp
            ()
            (let ((tvar (fresh-tvar-type)))
              (an-answer (list-type tvar) subst)))
           (car-exp
            (exp1)
            (cases answer (type-of-exp exp1 tenv subst)
                   (an-answer
                    (ty1 subst)
                    (cases type ty1
                           (list-type
                            (et)
                            (an-answer et subst))
                           (else
                            (report-rator-not-a-list-type ty1 exp1))))))
           (cdr-exp
            (exp1)
            (cases answer (type-of-exp exp1 tenv subst)
                   (an-answer
                    (ty1 subst)
                    (cases type ty1
                           (list-type
                            (et)
                            (an-answer ty1 subst))
                           (else
                            (report-rator-not-a-list-type ty1 exp1)))))))))

(define report-rator-not-a-proc-type
  (lambda (ty1 exp)
    (eopl:error 'type-of-exp
                "Expect a procedure, actual ~a: ~a"
                (type-to-external-form ty1)
                exp)))

(define report-rator-not-a-list-type
  (lambda (ty1 exp)
    (eopl:error 'type-of-exp
                "Expect a list, actual ~a: ~a"
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

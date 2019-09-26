#lang eopl
(require "exer7.24.lang.scm")
(provide checked-type-of type-of type-of-program type-of-exp equal-types?)

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

(define report-type-var-number-mismatch
  (lambda (symp vars types)
    (eopl:error 'extend-tenv* "type number is ~s than variable number in:~%~a, ~a"
                symp vars (map type-to-external-form types))))

;;; ---------------------- Substitution ----------------------
;;; apply-one-subst : Type x Tvar x Type -> Type
(define apply-one-subst
  (lambda (ty0 tvar ty1)
    (cases type ty0
           (int-type () ty0)
           (bool-type () ty0)
           (proc-type
            (tvs tb)
            (proc-type
             (map (lambda (t) (apply-one-subst t tvar ty1)) tvs)
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
            (ts t2)
            (proc-type
             (map (lambda (t) (apply-subst-to-type t subst)) ts)
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
             (let ((subst (unifier-multiple-types
                           (proc-type->arg-types ty1)
                           (proc-type->arg-types ty2)
                           subst exp)))
               (let ((subst (unifier (proc-type->result-type ty1)
                                     (proc-type->result-type ty2)
                                     subst exp)))
                 subst))]
            [else (report-unification-failure ty1 ty2 exp)]))))

;;; unifier-multiple-types : Listof(Type) x Listof(Type) x Subst x Exp -> Subst
(define unifier-multiple-types
  (lambda (tys1 tys2 subst exp)
    (cond [(null? tys1)
           (if (null? tys2)
               subst
               (report-unification-types-mismatch 'less exp))]
          [(null? tys2)
           (report-unification-types-mismatch 'more exp)]
          [else
           (let ((subst (unifier (car tys1) (car tys2) subst exp)))
             (unifier-multiple-types (cdr tys1) (cdr tys2) subst exp))])))

;;; no-occurrence? : Type x Type -> Bool
(define no-occurrence?
  (lambda (ty1 ty2)
    (cases type ty2
           (int-type () #t)
           (bool-type () #t)
           (proc-type
            (ts t2)
            (and ((list-of (lambda (t) (no-occurrence? ty1 t))) ts)
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
           (proc-type (ts t2) #t)
           (else #f))))

;;; proc-type->arg-types : Type -> Listof(Type)
(define proc-type->arg-types
  (lambda (ty)
    (cases type ty
           (proc-type (ts t2) ts)
           (else #f))))

;;; proc-type->result-type : Type -> Type
(define proc-type->result-type
  (lambda (ty)
    (cases type ty
           (proc-type (ts t2) t2)
           (else #f))))

(define report-unification-failure
  (lambda (ty1 ty2 exp)
    (eopl:error 'unification-failure
                "Type mismatch: ~s doesn't match ~s in ~s~%"
                (type-to-external-form ty1)
                (type-to-external-form ty2)
                exp)))

(define report-unification-types-mismatch
  (lambda (sym exp)
    (eopl:error 'unification-failure "Type number mismatch in ~a:~%lhs type number is ~s rhs"
                exp sym)))

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
           (proc-type (arg-types result-type)
                      (list (map type-to-external-form arg-types)
                            '->
                            (type-to-external-form result-type)))
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
            (b-vars b-exps let-body)
            (let ((b-type-subst (type-of-exps b-exps tenv subst)))
              (let ((b-types (car b-type-subst))
                    (b-subst (cdr b-type-subst)))
                (type-of-exp let-body
                             (extend-tenv* b-vars b-types tenv)
                             b-subst))))
           (proc-exp
            (vars optys body)
            (let ((arg-types (map optype->type optys)))
              (cases answer (type-of-exp body
                                         (extend-tenv* vars arg-types tenv)
                                         subst)
                     (an-answer
                      (result-type subst)
                      (an-answer (proc-type arg-types result-type)
                                 subst)))))
           (letrec-exp
            (result-otypes p-names b-vars arg-otypes b-bodies letrec-body)
            (let ((result-types (map optype->type result-otypes))
                  (arg-types (map (lambda (es)
                                    (map (lambda (o) (optype->type o)) es))
                                  arg-otypes)))
              (let ((tenv-for-letrec-body
                     (extend-tenv* p-names
                                   (map proc-type arg-types result-types)
                                   tenv)))
                (let ((subst
                       (type-of-letrec-p-bodies
                        result-types b-vars arg-types b-bodies tenv-for-letrec-body subst)))
                  (type-of-exp letrec-body tenv-for-letrec-body subst)))))
           (call-exp
            (rator rands)
            (let ((result-type (fresh-tvar-type)))
              (let ((rand-types-subst (type-of-exps rands tenv subst)))
                (let ((rand-types (car rand-types-subst))
                      (rand-subst (cdr rand-types-subst)))
                  (cases answer (type-of-exp rator tenv rand-subst)
                         (an-answer
                          (rator-type subst)
                          (let ((subst (unifier rator-type
                                                (proc-type rand-types result-type)
                                                subst
                                                exp)))
                            (an-answer result-type subst))))))))
           )))

;;; type-of-exps : Listof(Exp) x Tenv x Subst -> (Listof(Types) . Subst)
(define type-of-exps
  (lambda (exps tenv subst)
    (let loop ((exps exps)
               (subst subst)
               (k (lambda (ts subst) (cons ts subst))))
      (if (null? exps)
          (k '() subst)
          (cases answer (type-of-exp (car exps) tenv subst)
                 (an-answer
                  (ty subst)
                  (loop (cdr exps)
                        subst
                        (lambda (ts s)
                          (k (cons ty ts) subst)))))))))

;;; type-of-letrec-p-bodies :
;;; Listof(Tvar) x Listof(Listof(Id)) x Listof(Listof(Tvar)) x Listof(Exp) x Tenv x Subst -> Subst
(define type-of-letrec-p-bodies
  (lambda (result-types b-vars arg-types p-bodies tenv subst)
    (if (null? p-bodies)
        subst
        (cases answer (type-of-exp (car p-bodies)
                                   (extend-tenv* (car b-vars)
                                                 (car arg-types)
                                                 tenv)
                                   subst)
               (an-answer
                (ty subst)
                (let ((subst (unifier ty (car result-types) subst (car p-bodies))))
                  (type-of-letrec-p-bodies
                   (cdr result-types)
                   (cdr b-vars)
                   (cdr arg-types)
                   (cdr p-bodies)
                   tenv
                   subst)))))))

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

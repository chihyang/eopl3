#lang eopl
;;; ---------------------- Environment(from section 3.2) ----------------------
(define member?
  (lambda (sym lst)
    (if (null? lst)
        #f
        (or (eqv? sym (car lst))
            (member? sym (cdr lst))))))
(define check-duplicates
  (lambda (lst)
    (cond ((null? lst) '())
          ((member? (car lst) (cdr lst)) (car lst))
          (else (check-duplicates (cdr lst))))))
(define empty-env?
  (lambda (env)
    (and (list? env)
         (not (null? env))
         (eqv? (car env) 'empty-env))))
(define extended-env?
  (lambda (env)
    (and (list? env)
         (not (null? env))
         (or (eqv? (car env) 'extend-env)
             (eqv? (car env) 'extend-env-rec)))))
(define environment?
  (lambda (env)
    (or (empty-env? env)
        (extended-env? env))))
(define empty-env
  (lambda () (list 'empty-env)))
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))
(define extend-env-rec
  (lambda (p-names p-vars p-bodies env)
    (let ((dup-name (check-duplicates p-names)))
      (if (null? dup-name)
          (list 'extend-env-rec (list p-names p-vars p-bodies) env)
          (report-duplicate-id dup-name)))))
;; extend-env* : Listof(Id) x Listof(ExpVal) x Env -> Env
(define extend-env*
  (lambda (vars vals env)
    (let ((dup (check-duplicates vars))
          (var-len (length vars))
          (val-len (length vals)))
      (cond [(not (null? dup)) (report-duplicate-id dup)]
            [(< var-len val-len)
             (report-argument-mismatch 'greater)]
            [(> var-len val-len)
             (report-argument-mismatch 'less)]
            [else
             (letrec ((extend-env*-inner
                       (lambda (vars vals env)
                         (cond [(null? vars)
                                env]
                               [else
                                (extend-env*-inner
                                 (cdr vars)
                                 (cdr vals)
                                 (list 'extend-env (car vars) (car vals) env))]))))
               (extend-env*-inner vars vals env))]))))
(define apply-env
  (lambda (env search-var)
    (cond
     ((eqv? (car env) 'empty-env)
      (report-no-binding-found search-var))
     ((eqv? (car env) 'extend-env)
      (let ((saved-var (cadr env))
            (saved-val (caddr env))
            (saved-env (cadddr env)))
        (if (eqv? search-var saved-var)
            saved-val
            (apply-env saved-env search-var))))
      ((eqv? (car env) 'extend-env-rec)
       (let ((func (apply-env-rec search-var
                                  (car (cadr env))
                                  (cadr (cadr env))
                                  (caddr (cadr env)))))
         (if (null? func)
             (apply-env (caddr env) search-var)
             (let ((saved-p-vars (car func))
                   (saved-p-body (cadr func)))
               (proc-val (procedure saved-p-vars saved-p-body env))))))
     (else
      (report-invalid-env env)))))
(define apply-env-rec
  (lambda (var p-names p-vars p-bodies)
    (cond [(null? p-names) '()]
          [(eqv? var (car p-names))
           (list (car p-vars) (car p-bodies))]
          [else
           (apply-env-rec var (cdr p-names) (cdr p-vars) (cdr p-bodies))])))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))
(define report-argument-mismatch
  (lambda (symp)
    (eopl:error 'extend-env* "Argument number is ~s than parameter number" symp)))
(define report-duplicate-id
  (lambda (sym)
    (eopl:error 'extend-env* "Duplicate identifier ~s" sym)))

;;; ---------------------- Type ----------------------
(define-datatype type type?
  (int-type)
  (bool-type)
  (proc-type
   (arg-type type?)
   (result-type type?)))

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

;;; ---------------------- Expval ----------------------
(define identifier? symbol?)
(define-datatype proc proc?
  (procedure
   (var identifier?)
   (body expression?)
   (saved-env environment?)))
(define-datatype exp-val exp-val?
  (num-val
   (val number?))
  (bool-val
   (val boolean?))
  (proc-val
   (val proc?)))
(define expval->num
  (lambda (value)
    (cases exp-val value
           (num-val
            (number)
            number)
           (else
            (report-invalid-exp-value 'num)))))
(define expval->bool
  (lambda (value)
    (cases exp-val value
           (bool-val
            (boolean)
            boolean)
           (else
            (report-invalid-exp-value 'bool)))))
(define expval->proc
  (lambda (value)
    (cases exp-val value
           (proc-val
            (proc1)
            proc1)
           (else
            (report-invalid-exp-value 'proc)))))
(define report-invalid-exp-value
  (lambda (type)
    (eopl:error
     'exp-val
     "Not a valid exp value of type ~s" type)))

;;; ---------------------- Syntax for the CHECKED language ----------------------
;;; Program    ::= Expression
;;;                a-program (exp1)
;;; Expression ::= Number
;;;                const-exp (num)
;;; Expression ::= -(Expression , Expression)
;;;                diff-exp (exp1 exp2)
;;; Expression ::= zero? (Expression)
;;;                zero?-exp (exp1)
;;; Expression ::= if Expression then Expression else Expression
;;;                if-exp (exp1 exp2 exp3)
;;; Expression ::= Identifier
;;;                var-exp (var)
;;; Expression ::= let Identifier = Expression in Expression
;;;                let-exp (var exp1 body)
;;; Expression ::= proc (Identifier : Type) Expression
;;;                proc-exp (var ty body)
;;; Expression ::= letrec Type (Identifier : Type) = Expression in Expression
;;;                letrec-exp (p-result-type p-name b-var b-var-type p-body letrec-body)
;;; Expression ::= (Expression Expression)
;;;                call-exp (rator rand)
;;; Parse Expression
(define let-scanner-spec
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number ((or (concat digit (arbno digit))
                 (concat "-" digit (arbno digit))
                 (concat (arbno digit) "." digit (arbno digit))
                 (concat "-" (arbno digit) "." digit (arbno digit))
                 (concat digit (arbno digit) "." (arbno digit))
                 (concat "-" digit (arbno digit) "." (arbno digit)))) number)))
(define let-grammar
  '((program (expression) a-program)
    (expression (number)
                const-exp)
    (expression ("-" "(" expression "," expression ")")
                diff-exp)
    (expression ("zero?" "(" expression ")")
                zero?-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression (identifier)
                var-exp)
    (expression ("let" identifier "=" expression "in" expression)
                let-exp)
    (expression ("proc" "(" identifier ":" type-dec ")" expression)
                proc-exp)
    (expression ("letrec" type-dec identifier "(" identifier ":" type-dec ")" expression expression)
                letrec-exp)
    (expression ("(" expression expression ")")
                call-exp)
    (type-dec ("int")
              int-type-dec)
    (type-dec ("bool")
              bool-type-dec)
    (type-dec ("(" type-dec "->" type-dec ")")
              proc-type-dec)))

;;; ---------------------- type checker ----------------------
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

(define type-dec-to-type
  (lambda (ty)
    (cases type-dec ty
           (int-type-dec
            ()
            (int-type))
           (bool-type-dec
            ()
            (bool-type))
           (proc-type-dec
            (arg-type result-type)
            (proc-type (type-dec-to-type arg-type)
                       (type-dec-to-type result-type))))))

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
             (type-of-exp exp1 tenv))
            (bool-type))
           (diff-exp
            (exp1 exp2)
            (check-equal-type!
             (int-type)
             (type-of-exp exp1 tenv))
            (check-equal-type!
             (int-type)
             (type-of-exp exp2 tenv))
            (int-type))
           (if-exp
            (exp1 exp2 exp3)
            (let ((ty1 (type-of-exp exp1 tenv))
                  (ty2 (type-of-exp exp2 tenv))
                  (ty3 (type-of-exp exp3 tenv)))
              (check-equal-type! (bool-type) ty1)
              (check-equal-type! ty2 ty3)
              ty2))
           (proc-exp
            (var ty body)
            (let ((arg-type (type-dec-to-type ty)))
              (let ((result-type (type-of-exp body (extend-tenv var arg-type tenv))))
                (proc-type arg-type result-type))))
           (let-exp
            (b-var b-exp let-body)
            (let ((b-type (type-of-exp b-exp tenv)))
              (type-of-exp let-body (extend-tenv b-var b-type tenv))))
           (letrec-exp
            (p-result-type p-name b-var b-arg-type b-body letrec-body)
            (let ((arg-type (type-dec-to-type b-arg-type))
                  (result-type (type-dec-to-type p-result-type)))
              (let ((checked-type (type-of-exp
                                   b-body
                                   (extend-env b-var arg-type
                                               (extend-env p-name
                                                           (proc-type arg-type result-type)
                                                           tenv)))))
                (check-equal-type! checked-type result-type)
                (type-of-exp letrec-body
                             (extend-env p-name
                                         (proc-type arg-type result-type)
                                         tenv)))))
           (call-exp
            (rator rand)
            (let ((rator-type (type-of-exp rator tenv))
                  (rand-type (type-of-exp rator tenv)))
              (cases type rator-type
                     (proc-type
                      (arg-type result-type)
                      (check-equal-type! arg-type rand-type))
                     (else
                      (report-invalid-procedure rator-type exp))))))))

(define report-invalid-procedure
  (lambda (ty1 exp)
    (eopl:error 'type-of-exp
                "Expect a procedure, actual ~a~%~a~%"
                (type-to-external-form ty1)
                exp)))

;;; ---------------------- interpreter ----------------------
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
           (procedure
            (var body saved-env)
            (value-of body (extend-env var val saved-env))))))

;;; ---------------------- Sllgen operations ----------------------
(sllgen:make-define-datatypes let-scanner-spec let-grammar)
(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes let-scanner-spec let-grammar)))
(define just-scan
  (sllgen:make-string-scanner let-scanner-spec let-grammar))
(define scan&parse
  (sllgen:make-string-parser let-scanner-spec let-grammar))
(define read-eval-print
  (sllgen:make-rep-loop
   "--> "
   value-of-program
   (sllgen:make-stream-parser let-scanner-spec let-grammar)))
(define run
  (lambda (exp)
    (value-of-program (scan&parse exp))))

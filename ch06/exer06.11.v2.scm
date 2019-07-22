#lang eopl
;;; ---------------------- Environment (from section 3.2) ----------------------
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
(define extend-env*
  (lambda (vars vals env)
    (let ((duplicate (check-duplicates vars))
          (var-len (length vars))
          (val-len (length vals)))
      (cond [(not (null? duplicate))
             (report-duplicate-id duplicate)]
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
(define extend-env-rec
  (lambda (p-names p-vars p-bodies env)
    (let ((dup-name (check-duplicates p-names)))
      (if (null? dup-name)
          (list 'extend-env-rec (list p-names p-vars p-bodies) env)
          (report-duplicate-id dup-name)))))
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
;;; apply-env-rec : Sym x Listof(Sym) x Listof(Listof(Sym)) x Listof(Expression) ->
;;;                 ((Listof(Sym), Expression) | '(),
;;;                  (Listof(Sym),
;;;                   Listof(Listof(Sym)),
;;;                   Listof(Expression)))
(define apply-env-rec
  (lambda (var p-names p-vars p-bodies)
    (if (null? p-names)
        '()
        (if (eqv? var (car p-names))
            (list (car p-vars) (car p-bodies))
            (apply-env-rec var (cdr p-names) (cdr p-vars) (cdr p-bodies))))))
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
;;; ---------------------- Expval ----------------------
(define identifier? symbol?)
(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body tf-exp?)
   (saved-env environment?)))
(define-datatype exp-val exp-val?
  (num-val
   (val number?))
  (bool-val
   (val boolean?))
  (proc-val
   (val proc?))
  (null-val)
  (pair-val
   (val1 exp-val?)
   (val2 exp-val?)))
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
(define expval->null
  (lambda (value)
    (cases exp-val value
           (null-val
            ()
            '())
           (else
            (report-invalid-exp-value 'null)))))
(define expval->pair
  (lambda (value)
    (cases exp-val value
           (pair-val
            (val1 val2)
            (cons
             (cases exp-val val1
                    (num-val (num) num)
                    (bool-val (bool) bool)
                    (null-val () '())
                    (proc-val (proc1) proc1)
                    (pair-val (val3 val4) (expval->pair val1)))
             (cases exp-val val2
                    (num-val (num) num)
                    (bool-val (bool) bool)
                    (null-val () '())
                    (proc-val (proc1) proc1)
                    (pair-val (val3 val4) (expval->pair val2)))))
           (else
            (report-invalid-exp-value 'pair)))))
;; expval->schemeval : ExpVal -> SchemeVal
(define expval->schemeval
  (lambda (v)
    (cases exp-val v
           (num-val
            (num)
            num)
           (bool-val
            (bool)
            bool)
           (null-val
            ()
            (eopl:pretty-print '()))
           (pair-val
            (val1 val2)
            (expval->pair v))
           (proc-val
            (p)
            (cases proc p
                   (procedure
                    (var saved-env body)
                    `(λ (,var) ...)))))))
(define report-invalid-exp-value
  (lambda (type)
    (eopl:error
     'exp-val
     "Not a valid exp value of type ~s" type)))
;;; ---------------------- Continuation ----------------------
(define end-cont
  (lambda ()
    (lambda (val)
      (eopl:printf "End of computation.~%")
      val)))
(define apply-cont
  (lambda (cont val)
    (cont val)))

;;; ---------------------- Syntax for the CPS-OUT language ----------------------
;;; TfProgram  ::= TfExp
;;;                a-cps-program (exp1)
;;; SimpleExp  ::= Number
;;;                cps-const-exp (num)
;;; SimpleExp  ::= Identifier
;;;                cps-var-exp (var)
;;; SimpleExp  ::= emptylist
;;;                cps-emptylist-exp ()
;;; SimpleExp  ::= cons(SimpleExp , SimpleExp)
;;;                cps-cons-exp (exp1 exp2)
;;; SimpleExp  ::= car (SimpleExp)
;;;                cps-car-exp (exp1)
;;; SimpleExp  ::= cdr (SimpleExp)
;;;                cps-cdr-exp (exp1)
;;; SimpleExp  ::= list ({SimpleExp}{,}*)
;;;                cps-list-exp (exps)
;;; SimpleExp  ::= null? (SimpleExp)
;;;                cps-null?-exp (exp1)
;;; SimpleExp  ::= add1 (SimpleExp)
;;;                cps-add1-exp (exp1)
;;; SimpleExp  ::= - (SimpleExp, SimpleExp)
;;;                cps-diff-exp (exp1, exp2)
;;; SimpleExp  ::= * (SimpleExp, SimpleExp)
;;;                cps-mul-exp (exp1, exp2)
;;; SimpleExp  ::= greater? (SimpleExp, SimpleExp)
;;;                cps-greater?-exp (exp1, exp2)
;;; SimpleExp  ::= less? (SimpleExp, SimpleExp)
;;;                cps-less?-exp (exp1, exp2)
;;; SimpleExp  ::= equal? (SimpleExp, SimpleExp)
;;;                cps-equal?-exp (exp1, exp2)
;;; SimpleExp  ::= zero? (SimpleExp)
;;;                cps-zero?-exp (exp1)
;;; SimpleExp  ::= number? (SimpleExp)
;;;                cps-number?-exp (exp1)
;;; TfExp      ::= SimpleExp
;;;                simple-exp->exp (simple-exp1)
;;; TfExp      ::= if SimpleExp then TfExp else TfExp
;;;                cps-if-exp (exp1 exp2 exp3)
;;; TfExp      ::= let {Identifier = SimpleExp}* in TfExp
;;;                cps-let-exp (vars exps body)
;;; TfExp      ::= letrec {Identifier (Identifier*,) = TfExp}* in TfExp
;;;                cps-letrec-exp (p-names b-vars p-bodies body)
;;; TfExp      ::= (SimpleExp SimpleExp*)
;;;                cps-call-exp (rator rands)
;;; Parse Expression
(define let-scanner-spec
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    (number ((or (concat digit (arbno digit))
                 (concat "-" digit (arbno digit))
                 (concat (arbno digit) "." digit (arbno digit))
                 (concat "-" (arbno digit) "." digit (arbno digit))
                 (concat digit (arbno digit) "." (arbno digit))
                 (concat "-" digit (arbno digit) "." (arbno digit)))) number)
    (comment ("%" (arbno (not #\newline))) skip)))
(define let-grammar
  '((cps-program (tf-exp) a-cps-program)
    (simple-exp (number)
                cps-const-exp)
    (simple-exp (identifier)
                cps-var-exp)
    (simple-exp ("proc" "(" (separated-list identifier ",") ")" tf-exp)
                cps-proc-exp)
    (simple-exp ("emptylist")
                cps-emptylist-exp)
    (simple-exp ("cons" "(" simple-exp "," simple-exp ")")
                cps-cons-exp)
    (simple-exp ("car" "(" simple-exp ")")
                cps-car-exp)
    (simple-exp ("cdr" "(" simple-exp ")")
                cps-cdr-exp)
    (simple-exp ("list" "(" (separated-list simple-exp ",") ")")
                cps-list-exp)
    (simple-exp ("null?" "(" simple-exp ")")
                cps-null?-exp)
    (simple-exp ("add1" "(" simple-exp ")")
                cps-add1-exp)
    (simple-exp ("-" "(" simple-exp "," simple-exp ")")
                cps-diff-exp)
    (simple-exp ("*" "(" simple-exp "," simple-exp ")")
                cps-mul-exp)
    (simple-exp ("zero?" "(" simple-exp ")")
                cps-zero?-exp)
    (simple-exp ("equal?" "(" simple-exp "," simple-exp ")")
                cps-equal?-exp)
    (simple-exp ("greater?" "(" simple-exp "," simple-exp ")")
                cps-greater?-exp)
    (simple-exp ("less?" "(" simple-exp "," simple-exp ")")
                cps-less?-exp)
    (simple-exp ("number?" "(" simple-exp ")")
                cps-number?-exp)
    (tf-exp (simple-exp)
            simple-exp->exp)
    (tf-exp ("if" simple-exp "then" tf-exp "else" tf-exp)
            cps-if-exp)
    (tf-exp ("let" (arbno identifier "=" simple-exp) "in" tf-exp)
            cps-let-exp)
    (tf-exp ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" tf-exp) "in" tf-exp)
            cps-letrec-exp)
    (tf-exp ("(" simple-exp (arbno simple-exp) ")")
            cps-call-exp)))

;;; ---------------------- Evaluate CPS-OUT expression ----------------------
;;; apply-procedure/k : Proc x Listof(ExpVal) x Cont -> FinalAnswer
(define apply-procedure/k
  (lambda (proc1 vals cont)
    (cases proc proc1
           (procedure
            (vars body saved-env)
            (value-of/k body (extend-env* vars vals saved-env) cont)))))
;;; value-of/k : TfExp x Env x Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (cases tf-exp exp
           (simple-exp->exp
            (simple)
            (apply-cont cont
                        (value-of-simple-exp simple env)))
           (cps-if-exp
            (exp1 exp2 exp3)
            (if (expval->bool (value-of-simple-exp exp1 env))
                (value-of/k exp2 env cont)
                (value-of/k exp3 env cont)))
           (cps-let-exp
            (vars exps body)
            (value-of/k body
                        (extend-env* vars
                                     (map (lambda (e) (value-of-simple-exp e env))
                                          exps)
                                     env)
                        cont))
           (cps-letrec-exp
            (p-names p-vars p-bodies letrec-body)
            (value-of/k letrec-body (extend-env-rec p-names p-vars p-bodies env) cont))
           (cps-call-exp
            (rator rands)
            (let ((proc (expval->proc (value-of-simple-exp rator env)))
                  (args (map (lambda (e) (value-of-simple-exp e env)) rands)))
              (apply-procedure/k proc args cont))))))
;;; value-of-simple-exp : SimpleExp x Env -> ExpVal
(define value-of-simple-exp
  (lambda (exp env)
    (cases simple-exp exp
           (cps-const-exp
            (num)
            (num-val num))
           (cps-var-exp
            (var)
            (apply-env env var))
           (cps-emptylist-exp
            ()
            (null-val))
           (cps-car-exp
            (exp1)
            (let ((val (value-of-simple-exp exp1 env)))
              (cases exp-val val
                     (pair-val
                      (first rest)
                      first)
                     (else (report-invalid-exp-value 'pair-val)))))
           (cps-cdr-exp
            (exp1)
            (let ((val (value-of-simple-exp exp1 env)))
              (cases exp-val val
                     (pair-val
                      (first rest)
                      rest)
                     (else (report-invalid-exp-value 'pair-val)))))
           (cps-cons-exp
            (exp1 exp2)
            (let ((val1 (value-of-simple-exp exp1 env))
                  (val2 (value-of-simple-exp exp2 env)))
              (pair-val val1 val2)))
           (cps-list-exp
            (exps)
            (if (null? exps)
                (null-val)
                (pair-val
                 (value-of-simple-exp (car exps) env)
                 (value-of-simple-exp (cps-list-exp (cdr exps)) env))))
           (cps-null?-exp
            (exp1)
            (let ((val (value-of-simple-exp exp1 env)))
              (cases exp-val val
                     (null-val
                      ()
                      (bool-val #t))
                     (else
                      (bool-val #f)))))
           (cps-proc-exp
            (vars body)
            (proc-val (procedure vars body env)))
           (cps-add1-exp
            (exp1)
            (let ((num1 (expval->num (value-of-simple-exp exp1 env))))
              (num-val (+ num1 1))))
           (cps-diff-exp
            (exp1 exp2)
            (let ((num1 (expval->num (value-of-simple-exp exp1 env)))
                  (num2 (expval->num (value-of-simple-exp exp2 env))))
              (num-val (- num1 num2))))
           (cps-mul-exp
            (exp1 exp2)
            (let ((num1 (expval->num (value-of-simple-exp exp1 env)))
                  (num2 (expval->num (value-of-simple-exp exp2 env))))
              (num-val (* num1 num2))))
           (cps-equal?-exp
            (exp1 exp2)
            (let ((num1 (expval->num (value-of-simple-exp exp1 env)))
                  (num2 (expval->num (value-of-simple-exp exp2 env))))
              (bool-val (equal? num1 num2))))
           (cps-greater?-exp
            (exp1 exp2)
            (let ((num1 (expval->num (value-of-simple-exp exp1 env)))
                  (num2 (expval->num (value-of-simple-exp exp2 env))))
              (bool-val (> num1 num2))))
           (cps-less?-exp
            (exp1 exp2)
            (let ((num1 (expval->num (value-of-simple-exp exp1 env)))
                  (num2 (expval->num (value-of-simple-exp exp2 env))))
              (bool-val (< num1 num2))))
           (cps-zero?-exp
            (exp1)
            (bool-val (zero? (expval->num (value-of-simple-exp exp1 env)))))
           (cps-number?-exp
            (exp1)
            (cases exp-val (value-of-simple-exp exp1 env)
                   (num-val
                    (num)
                    (bool-val #t))
                   (else
                    (bool-val #f)))))))
;;; value-of-cps-program : CPS-Out-Program -> FinalAnswer
(define value-of-cps-program
  (lambda (prog)
    (cases cps-program prog
           (a-cps-program
            (exp)
            (let ((val (value-of/k exp (empty-env) (end-cont))))
              (expval->schemeval val))))))

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
   value-of-cps-program
   (sllgen:make-stream-parser let-scanner-spec let-grammar)))
(define run-cps
  (lambda (exp)
    (value-of-cps-program (scan&parse exp))))

(provide run-cps)

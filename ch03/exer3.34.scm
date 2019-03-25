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
(define empty-env
  (lambda ()
    (lambda (search-var)
      (report-no-binding-found search-var))))
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))
(define extend-env-list
  (lambda (saved-vars saved-vals saved-env)
    (let ((duplicate (check-duplicates saved-vars))
          (var-len (length saved-vars))
          (val-len (length saved-vals)))
      (cond ((not (null? duplicate))
             (report-duplicate-id duplicate))
            ((< var-len val-len)
             (report-argument-mismatch 'greater))
            ((> var-len val-len)
             (report-argument-mismatch 'less))
            (else
             (lambda (search-var)
               (cond ((null? saved-vals)
                      (apply-env saved-env search-var))
                     ((eqv? search-var (car saved-vars))
                      (car saved-vals))
                     (else
                      (apply-env (extend-env-list (cdr saved-vars) (cdr saved-vals) saved-env)
                                 search-var)))))))))
(define extend-env-rec
  (lambda (p-names p-vars p-bodys saved-env)
    (let ((duplicate (check-duplicates p-vars)))
      (if (null? duplicate)
          (lambda (search-var)
            (if (null? p-names)
                (apply-env saved-env search-var)
                (let ((func (apply-env-rec search-var p-names p-vars p-bodys)))
                  (if (null? func)
                      (apply-env saved-env search-var)
                      (proc-val
                       (procedure (car func)
                                  (cadr func)
                                  (extend-env-rec p-names p-vars p-bodys saved-env)))))))
          (report-duplicate-id duplicate)))))
(define apply-env
  (lambda (env search-var)
    (env search-var)))
(define apply-env-rec
  (lambda (var p-names p-vars p-bodys)
    (if (null? p-names)
        '()
        (if (eqv? var (car p-names))
            (list (car p-vars) (car p-bodys))
            (apply-env-rec var (cdr p-names) (cdr p-vars) (cdr p-bodys))))))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))
(define report-argument-mismatch
  (lambda (symp)
    (eopl:error 'extend-env-list "Argument number is ~s than parameter number" symp)))
(define report-duplicate-id
  (lambda (sym)
    (eopl:error 'extend-env-list "Duplicate identifier ~s" sym)))

;;; ---------------------- Expval ----------------------
(define identifier? symbol?)
;;; proc? : SchemeVal -> Bool
(define proc?
  (lambda (val)
    (procedure? val)))
;;; procedure : Var x Exp x Env -> Proc
(define procedure
  (lambda (vars body env)
    (lambda (vals)
      (value-of body (extend-env-list vars vals env)))))
;;; apply-procedure : Proc x ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 vals)
    (proc1 vals)))
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
     "No a valid exp value of type ~s" type)))

;;; ---------------------- Syntax for the PROC language ----------------------
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
;;; Expression ::= let {Identifier = Expression}* in Expression
;;;                let-exp (var exp1 body)
;;; Expression ::= proc (Identifier) Expression
;;;                proc-exp (var body)
;;; Expression ::= letrec Identifier (Identifier*,) = Expression in Expression
;;;                letrec-exp (p-name b-var p-exp1 letrec-body)
;;; Expression ::= (Expression Expression*)
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
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression)
                proc-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
                letrec-exp)
    (expression ("(" expression (arbno expression) ")")
                call-exp)))

;;; ---------------------- Evaluate expression ----------------------
(define value-of-list
  (lambda (exps env)
    (if (null? exps)
        '()
        (cons (value-of (car exps) env)
              (value-of-list (cdr exps) env)))))
(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp
            (num)
            (num-val num))
           (var-exp
            (var)
            (apply-env env var))
           (diff-exp
            (exp1 exp2)
            (num-val (- (expval->num (value-of exp1 env))
                        (expval->num (value-of exp2 env)))))
           (zero?-exp
            (exp1)
            (bool-val (eqv? (expval->num (value-of exp1 env)) 0)))
           (if-exp
            (exp1 exp2 exp3)
            (if (expval->bool (value-of exp1 env))
                (value-of exp2 env)
                (value-of exp3 env)))
           (let-exp
            (var exp1 body)
            (value-of-let-exp var exp1 body env))
           (proc-exp
            (vars body)
            (proc-val (procedure vars body env)))
           (letrec-exp
            (p-names p-vars p-bodys letrec-body)
            (value-of letrec-body (extend-env-rec p-names p-vars p-bodys env)))
           (call-exp
            (rator rand)
            (let ((proc (expval->proc (value-of rator env)))
                  (args (value-of-list rand env)))
              (apply-procedure proc args))))))
(define value-of-let-exp
  (lambda (var exp body env)
    (value-of
     body
     (let ((values (map (lambda (val) (value-of val env)) exp)))
       (extend-env-list var values env)))))
(define value-of-program
  (lambda (prog)
    (cases program prog
           (a-program
            (exp)
            (let ((val (value-of exp (empty-env))))
              (cases exp-val val
                     (num-val
                      (num)
                      num)
                     (bool-val
                      (bool)
                      bool)
                     (proc-val
                      (val)
                      val)))))))

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

;;; ---------------------- Test ----------------------
(eqv?
 (run "letrec double (x) = if zero?(x) then 0
                           else -((double -(x,1)),-2)
       in (double 6)")
 12)
(eqv?
 (run "let a = 3
       in let p = proc (x) -(x,a)
              a = 5
          in -(a, (p 2))")
 6)
(eqv?
 (run "letrec f (x, y) = if zero?(x) then y
                         else
                            if zero?(y) then x
                            else -((f -(x,1) -(y,1)), -2)
       in (f 4 12)")
 16)
(eqv?
 (run "letrec
         even(x) = if zero?(x) then 1 else (odd -(x,1))
         odd(x)  = if zero?(x) then 0 else (even -(x,1))
       in (odd 13)")
 1)
;; error
(eqv?
 (run "let in
       let x = 1
           y = 2
       in letrec f(y, y) = -(x, -(0, y))
          in (f 2)")
 3)

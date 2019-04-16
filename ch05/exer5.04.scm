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
  (lambda (p-name p-var p-body env)
    (list 'extend-env-rec p-name p-var p-body env)))
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
       (let ((saved-p-name (cadr env))
             (saved-p-var (caddr env))
             (saved-p-body (cadddr env))
             (saved-env (car (cddddr env))))
         (if (eqv? search-var saved-p-name)
             (proc-val (procedure saved-p-var saved-p-body env))
             (apply-env saved-env search-var))))
      (else
       (report-invalid-env env)))))
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

;;; ---------------------- Continuation ----------------------
;; FinalAnswer = ExpVal
;; Cont = ExpVal -> FinalAnswer
;; end-cont : () -> Cont
(define-datatype continuation continuation?
  (end-cont)
  (zero1-cont
   (cont continuation?))
  (let-exp-cont
   (var identifier?)
   (body expression?)
   (env environment?)
   (cont continuation?))
  (if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (env environment?)
   (cont continuation?))
  (diff1-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (diff2-cont
   (val1 exp-val?)
   (cont continuation?))
  (rator-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (rand-cont
   (val exp-val?)
   (cont continuation?))
  (let-multi-exp-cont
   (saved-vars (list-of identifier?))
   (saved-vals (list-of exp-val?))
   (cont-vars (list-of identifier?))
   (cont-exps (list-of expression?))
   (body expression?)
   (env environment?)
   (cont continuation?)))
;; apply-cont : Cont x ExpVal -> FinalAnswer
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
           (end-cont
            ()
            (begin
              (eopl:printf "End of computation.~%")
              val))
           (zero1-cont
            (cont)
            (apply-cont cont (bool-val (zero? (expval->num val)))))
           (let-exp-cont
            (var body env cont)
            (value-of/k body (extend-env var val env) cont))
           (if-test-cont
            (exp2 exp3 env cont)
            (if (expval->bool val)
                (value-of/k exp2 env cont)
                (value-of/k exp3 env cont)))
           (diff1-cont
            (exp2 env cont)
            (value-of/k exp2 env (diff2-cont val cont)))
           (diff2-cont
            (val1 cont)
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val)))
              (apply-cont cont (num-val (- num1 num2)))))
           (rator-cont
            (exp2 env cont)
            (value-of/k exp2 env (rand-cont val cont)))
           (rand-cont
            (rator cont)
            (let ((proc1 (expval->proc rator)))
              (apply-procedure/k proc1 val cont)))
           (let-multi-exp-cont
            (saved-vars saved-vals cont-vars cont-exps body env cont)
            (if (null? cont-exps)
                (value-of/k body
                            (extend-env* (cons (car cont-vars) saved-vars)
                                         (cons val saved-vals)
                                         env)
                            cont)
                (value-of/k (car cont-exps)
                            env
                            (let-multi-exp-cont (cons (car cont-vars) saved-vars)
                                                (cons val saved-vals)
                                                (cdr cont-vars)
                                                (cdr cont-exps)
                                                body
                                                env
                                                cont)))))))

;;; ---------------------- Expval ----------------------
(define identifier? symbol?)
(define-datatype proc proc?
  (procedure
   (var identifier?)
   (body expression?)
   (saved-env environment?)))
;; apply-procedure/k : Proc x ExpVal x Cont -> ExpVal
(define apply-procedure/k
  (lambda (proc1 val cont)
    (cases proc proc1
           (procedure
            (var body saved-env)
            (value-of/k body (extend-env var val saved-env) cont)))))
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
;;; Expression ::= let Identifier = Expression in Expression
;;;                let-exp (var exp1 body)
;;; Expression ::= let2 Identifier = Expression Identifier = Expression in Expression
;;;                let2-exp (var1 exp1 var2 exp2 body)
;;; Expression ::= let3 Identifier = Expression
;;;                     Identifier = Expression
;;;                     Identifier = Expression in Expression
;;;                let3-exp (var1 exp1 var2 exp2 var3 exp3 body)
;;; Expression ::= letrec Identifier (Identifier) = Expression in Expression
;;;                let-exp (var exp1 body)
;;; Expression ::= proc (Identifier) Expression
;;;                proc-exp (var body)
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
    (expression ("let2" identifier "=" expression identifier "=" expression "in" expression)
                let2-exp)
    (expression ("let3" identifier "=" expression identifier "=" expression identifier "=" expression "in" expression)
                let3-exp)
    (expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression)
                letrec-exp)
    (expression ("proc" "(" identifier ")" expression)
                proc-exp)
    (expression ("(" expression expression ")")
                call-exp)))

;;; ---------------------- Evaluate expression ----------------------
;; value-of/k : Exp x Env x Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
           (const-exp
            (num)
            (apply-cont cont (num-val num)))
           (var-exp
            (var)
            (apply-cont cont (apply-env env var)))
           (diff-exp
            (exp1 exp2)
            (value-of/k exp1 env (diff1-cont exp2 env cont)))
           (zero?-exp
            (exp1)
            (value-of/k exp1 env (zero1-cont cont)))
           (if-exp
            (exp1 exp2 exp3)
            (value-of/k exp1 env (if-test-cont exp2 exp3 env cont)))
           (let-exp
            (var exp1 body)
            (value-of/k exp1 env (let-multi-exp-cont '() '() (list var) '() body env cont)))
           (let2-exp
            (var1 exp1 var2 exp2 body)
            (value-of/k exp1 env (let-multi-exp-cont '() '() (list var1 var2) (list exp2) body env cont)))
           (let3-exp
            (var1 exp1 var2 exp2 var3 exp3 body)
            (value-of/k exp1 env (let-multi-exp-cont '() '() (list var1 var2 var3) (list exp2 exp3) body env cont)))
           (letrec-exp
            (p-name p-var p-body letrec-body)
            (value-of/k letrec-body (extend-env-rec p-name p-var p-body env) cont))
           (proc-exp
            (var body)
            (apply-cont cont (proc-val (procedure var body env))))
           (call-exp
            (rator rand)
            (value-of/k rator env (rator-cont rand env cont))))))
;; value-of-program : Program -> FinalAnswer
(define value-of-program
  (lambda (prog)
    (cases program prog
           (a-program
            (exp)
            (let ((val (value-of/k exp (empty-env) (end-cont))))
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
 (run "let2 x = 3 f = proc(x) -(x, -3) in (f 3)")
 6)
(eqv?
 (run "let3 x = 3 f = proc(x) -(x, -3) y = 4 in -(x, y)")
 -1)

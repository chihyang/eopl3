#lang eopl
;;; procedural version as in exercise 5.1, this is inlined version
;;; ---------------------- Environment(from section 3.2) ----------------------
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

;;; ---------------------- Continuation ----------------------
;; FinalAnswer = ExpVal
;; Cont = ExpVal -> FinalAnswer
;; end-cont : () -> Cont
(define end-cont
  (lambda ()
    (lambda (val)
      (begin
        (eopl:printf "End of computation.~%")
        val))))

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
            (cont (num-val num)))
           (var-exp
            (var)
            (cont (apply-env env var)))
           (diff-exp
            (exp1 exp2)
            (value-of/k exp1
                        env
                        (lambda (val)
                          (value-of/k exp2 env
                                      (lambda (val2)
                                        (let ((num1 (expval->num val))
                                              (num2 (expval->num val2)))
                                          (cont (num-val (- num1 num2)))))))))
           (zero?-exp
            (exp1)
            (value-of/k exp1 env
                        (lambda (val)
                          (cont
                           (bool-val
                            (zero? (expval->num val)))))))
           (if-exp
            (exp1 exp2 exp3)
            (value-of/k exp1 env
                        (lambda (val)
                          (if (expval->bool val)
                              (value-of/k exp2 env cont)
                              (value-of/k exp3 env cont)))))
           (let-exp
            (var exp1 body)
            (value-of/k exp1 env
                        (lambda (val)
                          (value-of/k body (extend-env var val env) cont))))
           (letrec-exp
            (p-name p-var p-body letrec-body)
            (value-of/k letrec-body (extend-env-rec p-name p-var p-body env) cont))
           (proc-exp
            (var body)
            (cont (proc-val (procedure var body env))))
           (call-exp
            (rator rand)
            (value-of/k rator env
                        (lambda (val1)
                          (value-of/k rand env
                                      (lambda (val2)
                                        (let ((proc1 (expval->proc val1)))
                                          (apply-procedure/k proc1 val2 cont))))))))))
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
(require rackunit)
(check-eqv?
  (run "letrec double (x) = if zero?(x) then 0
                       else -((double -(x,1)),-2)
      in (double 6)")
  12)
(check-eqv?
 (run "let x = 7 in
         let y = 2 in
           let y = let x = -(x, 1) in -(x, y)
             in -(-(x,8), y)")
 -5)

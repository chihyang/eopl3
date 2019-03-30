#lang eopl
;;; ---------------------- Environment(from section 3.2) ----------------------
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (vars valid-vars?)
   (vals vector?)
   (env environment?)))
(define extend-env-rec
  (lambda (p-names b-vars bodies saved-env)
    (let ((vec (make-vector (length p-names))))
      (let ((new-env (extend-env p-names vec saved-env)))
        (make-proc-vec! vec 0 b-vars bodies new-env)
        new-env))))
(define make-proc-vec!
  (lambda (vec n b-vars bodies env)
    (cond ([null? b-vars] vec)
          (else
           (vector-set!
            vec n (proc-val (procedure (car b-vars) (car bodies) env)))
           (make-proc-vec! vec (+ n 1) (cdr b-vars) (cdr bodies) env)))))
(define apply-env
  (lambda (env search-var)
    (cases environment env
           (empty-env
            ()
            (report-no-binding-found search-var))
           (extend-env
            (saved-vars saved-vals saved-env)
            (let ((val (search-val saved-vars saved-vals search-var)))
              (if (null? val)
                  (apply-env saved-env search-var)
                  val))))))
(define search-val
  (lambda (saved-vars saved-vals search-var)
    (search-from-vector-vals saved-vars saved-vals search-var 0)))
(define search-from-vector-vals
  (lambda (saved-vars saved-vals search-var n)
    (cond ((null? saved-vars)
           '())
          ((eqv? search-var (car saved-vars))
           (vector-ref saved-vals n))
          (else
           (search-from-vector-vals
            (cdr saved-vars) saved-vals search-var (+ n 1))))))
(define valid-vars?
  (lambda (vars)
    (and ((list-of identifier?) vars)
         (no-duplicate? vars))))
(define no-duplicate?
  (lambda (vars)
    (let ((duplicate (check-duplicates vars)))
      (if (null? duplicate)
          #t
          (report-duplicate-id duplicate)))))
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
(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))
(define report-argument-mismatch
  (lambda (symp)
    (eopl:error 'apply-procedure "Argument number is ~s than parameter number" symp)))
(define report-duplicate-id
  (lambda (sym)
    (eopl:error 'extend-env "Duplicate identifier ~s" sym)))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))

;;; ---------------------- Expval ----------------------
(define identifier? symbol?)
(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body expression?)
   (saved-env environment?)))
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
           (procedure
            (vars body saved-env)
            (let ((var-len (length vars))
                  (val-len (length vals)))
              (cond ((< var-len val-len)
                     (report-argument-mismatch 'greater))
                    ((> var-len val-len)
                     (report-argument-mismatch 'less))
                    (else
                     (value-of body (extend-env vars (list->vector vals) saved-env)))))))))
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
;;; Expression ::= *(Expression , Expression)
;;;                mul-exp (exp1 exp2)
;;; Expression ::= add1(Expression)
;;;                add1-exp (exp1)
;;; Expression ::= zero? (Expression)
;;;                zero?-exp (exp1)
;;; Expression ::= if Expression then Expression else Expression
;;;                if-exp (exp1 exp2 exp3)
;;; Expression ::= Identifier
;;;                var-exp (var)
;;; Expression ::= let {Identifier = Expression}* in Expression
;;;                let-exp (var exp1 body)
;;; Expression ::= proc (Identifier*,) Expression
;;;                proc-exp (var body)
;;; Expression ::= letrec {Identifier (Identifier*,) = Expression}* in Expression
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
    (expression ("add1" "(" expression ")")
                add1-exp)
    (expression ("*" "(" expression "," expression ")")
                mul-exp)
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
           (add1-exp
            (exp1)
            (num-val (+ 1 (expval->num (value-of exp1 env)))))
           (mul-exp
            (exp1 exp2)
            (num-val (* (expval->num (value-of exp1 env))
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
            (vars exps body)
            (value-of body (extend-env vars (list->vector (value-of-list exps env)) env)))
           (proc-exp
            (vars body)
            (proc-val (procedure vars body env)))
           (letrec-exp
            (p-names p-vars p-bodies letrec-body)
            (value-of letrec-body (extend-env-rec p-names p-vars p-bodies env)))
           (call-exp
            (rator rand)
            (let ((proc (expval->proc (value-of rator env)))
                  (args (value-of-list rand env)))
              (apply-procedure proc args))))))
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
;; lexical scoping
(eqv?
 (run "let fact = proc (n) add1(n)
       in let fact = proc (n)
                     if zero?(n) then 1
                     else *(n, (fact -(n,1)))
          in (fact 5)")
 25)
(eqv?
 (run "letrec fact(n) = add1(n)
       in letrec fact(n) = if zero?(n) then 1
                           else *(n, (fact -(n,1)))
          in (fact 5)")
 120)
(eqv?
 (run "letrec
         even(x) = if zero?(x) then 1 else (odd -(x,1))
         odd(x)  = if zero?(x) then 0 else (even -(x,1))
       in (odd 13)")
 1)
(eqv?
 (run "let x = 3 in
       letrec
         even(x) = if zero?(x) then 1 else (odd -(x,1))
         odd(x)  = if zero?(x) then 0 else (even -(x,1))
       in let y = x in (even y)")
 0)

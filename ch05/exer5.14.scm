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
;;; apply-env-rec : Sym x Listof(Sym) x Listof(Listof(Sym)) x Listof(Expression) ->
;;;                 ((Listof(Sym), Expression) | '(),
;;;                  (Listof(Sym),
;;;                   Listof(Listof(Sym)),
;;;                   Listof(Expression)))
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

;;; ---------------------- Continuation ----------------------
(define current-cont-size 0)
(define largest-cont-size 0)
(define add1-cont-size!
  (lambda ()
    (set! current-cont-size (+ current-cont-size 1))))
(define sub1-cont-size!
  (lambda ()
    (set! current-cont-size (- current-cont-size 1))))
(define get-current-cont-size
  (lambda () current-cont-size))
(define get-largest-cont-size
  (lambda () largest-cont-size))
(define update-largest-cont-size!
  (lambda ()
    (if (< largest-cont-size current-cont-size)
        (set! largest-cont-size current-cont-size)
        (set! largest-cont-size largest-cont-size))))
(define init-cont-tracker!
  (lambda ()
    (begin
      (set! current-cont-size 1)
      (set! largest-cont-size 0))))
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
  (mul1-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (mul2-cont
   (val1 exp-val?)
   (cont continuation?))
  (rator-cont
   (exps (list-of expression?))
   (env environment?)
   (cont continuation?))
  (rand-cont
   (val exp-val?)
   (saved-rands (list-of exp-val?))
   (cont-exps (list-of expression?))
   (env environment?)
   (cont continuation?)))
;; apply-cont : Cont x ExpVal -> FinalAnswer
(define apply-cont
  (lambda (cont val)
    (track-cont-dec! cont)
    (cases continuation cont
           (end-cont
            ()
            (begin
              (eopl:printf "End of computation.~%")
              (eopl:printf "Largest continuation size is ~s.~%" (get-largest-cont-size))
              val))
           (zero1-cont
            (cont)
            (let ((v (zero? (expval->num val))))
              (apply-cont cont (bool-val v))))
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
            (let* ((num1 (expval->num val1))
                   (num2 (expval->num val))
                   (diff-val (- num1 num2)))
              (apply-cont cont (num-val diff-val))))
           (mul1-cont
            (exp2 env cont)
            (value-of/k exp2 env (mul2-cont val cont)))
           (mul2-cont
            (val1 cont)
            (let* ((num1 (expval->num val1))
                   (num2 (expval->num val))
                   (mul-val (* num1 num2)))
              (apply-cont cont (num-val mul-val))))
           (rator-cont
            (exps env cont)
            (if (null? exps)
                (apply-procedure/k (expval->proc val) '() cont)
                (value-of/k (car exps) env (rand-cont val '() (cdr exps) env cont))))
           (rand-cont
            (rator saved-vals cont-exps env cont)
            (if (null? cont-exps)
                (let ((proc1 (expval->proc rator)))
                  (apply-procedure/k proc1 (reverse (cons val saved-vals)) cont))
                (value-of/k (car cont-exps)
                            env
                            (rand-cont rator (cons val saved-vals)
                                       (cdr cont-exps) env cont)))))))

;;; ---------------------- Expval ----------------------
(define identifier? symbol?)
(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body expression?)
   (saved-env environment?)))
;; apply-procedure/k : Proc x ExpVal x Cont -> ExpVal
(define apply-procedure/k
  (lambda (proc1 vals cont)
    (cases proc proc1
           (procedure
            (vars body saved-env)
            (value-of/k body (extend-env* vars vals saved-env) cont)))))
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
;;; Expression ::= zero? (Expression)
;;;                zero?-exp (exp1)
;;; Expression ::= if Expression then Expression else Expression
;;;                if-exp (exp1 exp2 exp3)
;;; Expression ::= Identifier
;;;                var-exp (var)
;;; Expression ::= let Identifier = Expression in Expression
;;;                let-exp (var exp1 body)
;;; Expression ::= letrec {Identifier (Identifier*,)}* = Expression in Expression
;;;                letrec-exp (p-name b-var p-exp1 letrec-body)
;;; Expression ::= proc (Identifier*,) Expression
;;;                proc-exp (var body)
;;; Expression ::= (Expression Expression*)
;;;                call-exp (rator rand)
;;; Parse Expression
(define let-scanner-spec
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit "-"))) symbol)
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
    (expression ("*" "(" expression "," expression ")")
                mul-exp)
    (expression ("zero?" "(" expression ")")
                zero?-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression (identifier)
                var-exp)
    (expression ("let" identifier "=" expression "in" expression)
                let-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
                letrec-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression)
                proc-exp)
    (expression ("(" expression (arbno expression) ")")
                call-exp)))

;;; ---------------------- Evaluate expression ----------------------
;; value-of/k : Exp x Env x Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (track-cont-inc! exp)
    (cases expression exp
           (const-exp
            (num)
            (apply-cont cont (num-val num)))
           (var-exp
            (var)
            (let ((val (apply-env env var)))
              (apply-cont cont val)))
           (diff-exp
            (exp1 exp2)
            (value-of/k exp1 env (diff1-cont exp2 env cont)))
           (mul-exp
            (exp1 exp2)
            (value-of/k exp1 env (mul1-cont exp2 env cont)))
           (zero?-exp
            (exp1)
            (value-of/k exp1 env (zero1-cont cont)))
           (if-exp
            (exp1 exp2 exp3)
            (value-of/k exp1 env (if-test-cont exp2 exp3 env cont)))
           (let-exp
            (var exp1 body)
            (value-of/k exp1 env (let-exp-cont var body env cont)))
           (letrec-exp
            (p-names p-vars p-bodies letrec-body)
            (value-of/k letrec-body (extend-env-rec p-names p-vars p-bodies env) cont))
           (proc-exp
            (vars body)
            (apply-cont cont (proc-val (procedure vars body env))))
           (call-exp
            (rator rand)
            (value-of/k rator env (rator-cont rand env cont))))))
;; value-of-program : Program -> FinalAnswer
(define value-of-program
  (lambda (prog)
    (cases program prog
           (a-program
            (exp)
            (begin
              (init-cont-tracker!)
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
                        val))))))))

;;; ---------------------- print utility ----------------------
;; track-cont-dec! : Cont -> Unspecified
(define track-cont-dec!
  (lambda (cont)
    (update-largest-cont-size!)
    (cases continuation cont
           (end-cont
            ()
            (sub1-cont-size!))
           (zero1-cont
            (cont)
            (sub1-cont-size!))
           (let-exp-cont
            (var body env cont)
            (sub1-cont-size!))
           (if-test-cont
            (exp2 exp3 env cont)
            (sub1-cont-size!))
           (diff2-cont
            (val1 cont)
            (sub1-cont-size!))
           (mul2-cont
            (val1 cont)
            (sub1-cont-size!))
           (rator-cont
            (exps env cont)
            (if (null? exps)
                (sub1-cont-size!)
                20))
           (rand-cont
            (rator saved-vals cont-exps env cont)
            (if (null? cont-exps)
                (sub1-cont-size!)
                20))
           (else
            20))))
;; print-value-of/k : Exp -> Unspecified
(define track-cont-inc!
  (lambda (exp)
    (cases expression exp
           (diff-exp
            (exp1 exp2)
            (add1-cont-size!))
           (mul-exp
            (exp1 exp2)
            (add1-cont-size!))
           (zero?-exp
            (exp1)
            (add1-cont-size!))
           (if-exp
            (exp1 exp2 exp3)
            (add1-cont-size!))
           (let-exp
            (var exp1 body)
            (add1-cont-size!))
           (letrec-exp
            (p-names p-vars p-bodies letrec-body)
            (add1-cont-size!))
           (call-exp
            (rator rand)
            (add1-cont-size!))
           (else
            21))))

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
(run "letrec fact(x) = if zero?(x) then 1 else *(x, (fact -(x,1)))
      in (fact 4)")
(run "letrec fact(x) = if zero?(x) then 1 else *(x, (fact -(x,1)))
      in (fact 5)")
(run "letrec fact(x) = if zero?(x) then 1 else *(x, (fact -(x,1)))
      in (fact 6)")
(run "letrec fact(x) = if zero?(x) then 1 else *(x, (fact -(x,1)))
      in (fact 7)")
(run "letrec fact-iter-acc(n, a) = if zero?(n) then a else (fact-iter-acc -(n,1) *(n, a))
      in (fact-iter-acc 4 1)")
(run "letrec fact-iter-acc(n, a) = if zero?(n) then a else (fact-iter-acc -(n,1) *(n, a))
      in (fact-iter-acc 5 1)")
(run "letrec fact-iter-acc(n, a) = if zero?(n) then a else (fact-iter-acc -(n,1) *(n, a))
      in (fact-iter-acc 6 1)")
(run "letrec fact-iter-acc(n, a) = if zero?(n) then a else (fact-iter-acc -(n,1) *(n, a))
      in (fact-iter-acc 7 1)")

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
;; FinalAnswer = ExpVal
;; Cont = ExpVal -> FinalAnswer
;; end-cont : () -> Cont
(define end-cont
  (lambda () '()))
(define zero1-cont
  (lambda (cont)
    (cons (zero1-frame) cont)))
(define if-test-cont
  (lambda (exp2 exp3 env cont)
    (cons (if-test-frame exp2 exp3 env) cont)))
(define diff1-cont
  (lambda (exp2 env cont)
    (cons (diff1-frame exp2 env) cont)))
(define diff2-cont
  (lambda (val1 cont)
    (cons (diff2-frame val1) cont)))
(define rator-cont
  (lambda (exps env cont)
    (cons (rator-frame exps env) cont)))
(define rand-cont
  (lambda (val saved-rands cont-exps env cont)
    (cons (rand-frame val saved-rands cont-exps env) cont)))
(define let-cont
  (lambda (saved-vars saved-vals cont-vars cont-exps body env cont)
    (cons (let-frame saved-vars saved-vals cont-vars cont-exps body env) cont)))
(define cons1-cont
  (lambda (exp2 env cont)
    (cons (cons1-frame exp2 env) cont)))
(define cons2-cont
  (lambda (val1 cont)
    (cons (cons2-frame val1) cont)))
(define car-cont
  (lambda (cont)
    (cons (car-frame) cont)))
(define cdr-cont
  (lambda (cont)
    (cons (cdr-frame) cont)))
(define null1-cont
  (lambda (cont)
    (cons (null1-frame) cont)))
(define list1-cont
  (lambda (exps env cont)
    (cons (list1-frame exps env) cont)))
(define list2-cont
  (lambda (val cont)
    (cons (list2-frame val) cont)))
(define-datatype frame frame?
  (zero1-frame)
  (if-test-frame
   (exp2 expression?)
   (exp3 expression?)
   (env environment?))
  (diff1-frame
   (exp2 expression?)
   (env environment?))
  (diff2-frame
   (val1 exp-val?))
  (rator-frame
   (exps (list-of expression?))
   (env environment?))
  (rand-frame
   (val exp-val?)
   (saved-rands (list-of exp-val?))
   (cont-exps (list-of expression?))
   (env environment?))
  (let-frame
   (saved-vars (list-of identifier?))
   (saved-vals (list-of exp-val?))
   (cont-vars (list-of identifier?))
   (cont-exps (list-of expression?))
   (body expression?)
   (env environment?))
  (cons1-frame
   (exp2 expression?)
   (env environment?))
  (cons2-frame
   (val1 exp-val?))
  (car-frame)
  (cdr-frame)
  (null1-frame)
  (list1-frame
   (exps expression?)
   (env environment?))
  (list2-frame
   (first-val exp-val?)))
;; apply-cont : Cont x ExpVal -> FinalAnswer
(define apply-cont
  (lambda (cont val)
    (if (null? cont)
        (begin
          (eopl:printf "End of computation.~%")
          val)
        (let ((cur-frame (car cont))
              (next-cont (cdr cont)))
          (cases frame cur-frame
                 (zero1-frame
                  ()
                  (apply-cont next-cont (bool-val (zero? (expval->num val)))))
                 (if-test-frame
                  (exp2 exp3 env)
                  (if (expval->bool val)
                      (value-of/k exp2 env next-cont)
                      (value-of/k exp3 env next-cont)))
                 (diff1-frame
                  (exp2 env)
                  (value-of/k exp2 env (diff2-cont val next-cont)))
                 (diff2-frame
                  (val1)
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val)))
                    (apply-cont next-cont (num-val (- num1 num2)))))
                 (rator-frame
                  (exps env)
                  (if (null? exps)
                      (apply-procedure/k (expval->proc val) '() next-cont)
                      (value-of/k (car exps) env (rand-cont val '() (cdr exps) env next-cont))))
                 (rand-frame
                  (rator saved-vals cont-exps env)
                  (if (null? cont-exps)
                      (let ((proc1 (expval->proc rator)))
                        (apply-procedure/k proc1 (reverse (cons val saved-vals)) next-cont))
                      (value-of/k (car cont-exps)
                                  env
                                  (rand-cont rator (cons val saved-vals) (cdr cont-exps) env next-cont))))
                 (let-frame
                  (saved-vars saved-vals cont-vars cont-exps body env)
                  (if (null? cont-exps)
                      (let ((l-vars (reverse (cons (car cont-vars) saved-vars)))
                            (l-vals (reverse (cons val saved-vals))))
                        (value-of/k body
                                    (extend-env* l-vars l-vals env)
                                    next-cont))
                      (value-of/k (car cont-exps)
                                  env
                                  (let-cont (cons (car cont-vars) saved-vars)
                                            (cons val saved-vals)
                                            (cdr cont-vars)
                                            (cdr cont-exps)
                                            body
                                            env
                                            next-cont))))
                 (cons1-frame
                  (exp2 env)
                  (value-of/k exp2 env (cons2-cont val next-cont)))
                 (cons2-frame
                  (val1)
                  (apply-cont next-cont (pair-val val1 val)))
                 (car-frame
                  ()
                  (cases exp-val val
                         (pair-val
                          (first rest)
                          (apply-cont next-cont first))
                         (else (report-invalid-exp-value 'pair-val))))
                 (cdr-frame
                  ()
                  (cases exp-val val
                         (pair-val
                          (first rest)
                          (apply-cont next-cont rest))
                         (else (report-invalid-exp-value 'pair-val))))
                 (null1-frame
                  ()
                  (cases exp-val val
                         (null-val
                          ()
                          (apply-cont next-cont (bool-val #t)))
                         (else (apply-cont next-cont (bool-val #f)))))
                 (list1-frame
                  (exps env)
                  (value-of/k exps env (list2-cont val next-cont)))
                 (list2-frame
                  (first-val)
                  (apply-cont next-cont (pair-val first-val val))))))))

;;; ---------------------- Expval ----------------------
(define identifier? symbol?)
(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body expression?)
   (saved-env environment?)))
;; apply-procedure/k : Proc x Listof(ExpVal) x Cont -> ExpVal
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
;;; Expression ::= emptylist
;;;                emptylist-exp
;;; Expression ::= cons (Expression, Expression)
;;;                cons-exp (exp1 exp2)
;;; Expression ::= car (Expression)
;;;                car-exp (exp1)
;;; Expression ::= cdr (Expression)
;;;                cdr-exp (exp1)
;;; Expression ::= null? (Expression)
;;;                null?-exp (exp1)
;;; Expression ::= list (Expression, Expression, ...)
;;;                list-exp (exp1)
;;; Expression ::= let {Identifier = Expression}* in Expression
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
    (expression ("emptylist")
                emptylist-exp)
    (expression ("cons" "(" expression "," expression ")")
                cons-exp)
    (expression ("car" "(" expression ")")
                car-exp)
    (expression ("cdr" "(" expression ")")
                cdr-exp)
    (expression ("null?" "(" expression ")")
                null?-exp)
    (expression ("list" "(" (separated-list expression ",") ")")
                list-exp)
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
    (cases expression exp
           (const-exp
            (num)
            (apply-cont cont (num-val num)))
           (var-exp
            (var)
            (apply-cont cont (apply-env env var)))
           (emptylist-exp
            ()
            (apply-cont cont (null-val)))
           (cons-exp
            (exp1 exp2)
            (value-of/k exp1 env (cons1-cont exp2 env cont)))
           (car-exp
            (exp1)
            (value-of/k exp1 env (car-cont cont)))
           (cdr-exp
            (exp1)
            (value-of/k exp1 env (cdr-cont cont)))
           (null?-exp
            (exp1)
            (value-of/k exp1 env (null1-cont cont)))
           (list-exp
            (exps)
            (if (null? exps)
                (apply-cont cont (null-val))
                (value-of/k (car exps) env (list1-cont (list-exp (cdr exps)) env cont))))
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
            (vars exps body)
            (value-of/k (car exps) env (let-cont '() '()  vars (cdr exps) body env cont)))
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
                      val)
                     (null-val
                      ()
                      '())
                     (pair-val
                      (val1 val2)
                      (expval->pair val))))))))

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
 (run "let x = 3 f = proc(x) -(x, -3) in (f 3)")
 6)
(eqv?
 (run "let x = 3 f = proc(x) -(x, -3) y = 4 in -(x, y)")
 -1)
;; tests from exercise 3.16
(equal?
 (run "let x = 4 in cons(x, cons(cons(-(x,1), emptylist), emptylist))")
 '(4 (3)))
(eqv?
 (run "null?(cdr(let x = 4 in
                 cons(x, cons(cons(-(x,1),
                                   emptylist),
                              emptylist))))")
 #f)
(equal?
 (run
  "let x = 4 in list(x, -(x,1), -(x,3))")
 '(4 3 1))
(eqv?
 (run "let x = 30
         in let x = -(x,1)
                y = -(x,2)
           in -(x,y)")
 1)
;; error
(run "let x = 30
         in let x = -(x,1)
                x = -(x,2)
           in -(x,x)")
;; tests from exercise 3.21
(eqv?
 (run "((proc (x) proc (y) -(y,-(0,x)) 3) 4)")
 7)
(eqv?
 (run "(proc (x, y) -(y,-(0,x)) 3 4)")
 7)
;; error
(run "(proc (x, x) -(y,-(0,x)) 3 4)")
(run "(proc (x, y) -(y,-(0,x)) 3)")
(run "(proc (x, y) -(y,-(0,x)) 3 4 5)")
;; tests from exercise 3.32
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

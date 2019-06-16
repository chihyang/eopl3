#lang eopl
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
    (cons
     (lambda (val)
       (begin
         (eopl:printf "End of computation.~%")
         val))
     (lambda (val)
       (begin
         (report-uncaught-exception)
         #f)))))
;; zero1-cont : Cont -> Cont
(define zero1-cont
  (lambda (cont)
    (cons
     (lambda (val)
       (apply-cont cont
                   (bool-val
                    (zero? (expval->num val)))))
     (lambda (val)
       (apply-handler cont val)))))
;; let-exp-cont : Var x Exp x Env x Cont -> Cont
(define let-exp-cont
  (lambda (var body env cont)
    (cons
     (lambda (val)
       (value-of/k body (extend-env var val env) cont))
     (lambda (val)
       (apply-handler cont val)))))
;; cons1-cont : Exp x Env x Cont -> Cont
(define cons1-cont
  (lambda (exp2 env cont)
    (cons
     (lambda (val)
       (value-of/k exp2 env (cons2-cont val cont)))
     (lambda (val)
       (apply-handler cont val)))))
;; cons2-cont : Val x Cont -> Cont
(define cons2-cont
  (lambda (val1 cont)
    (cons
     (lambda (val)
       (apply-cont cont (pair-val val1 val)))
     (lambda (val)
       (apply-handler cont val)))))
;; car-cont : Cont -> Cont
(define car-cont
  (lambda (cont)
    (cons
     (lambda (val)
       (cases exp-val val
              (pair-val
               (first rest)
               (apply-cont cont first))
              (else (report-invalid-exp-value 'pair-val))))
     (lambda (val)
       (apply-handler cont val)))))
;; null1-cont : Cont -> Cont
(define null1-cont
  (lambda (cont)
    (cons
     (lambda (val)
       (cases exp-val val
              (null-val
               ()
               (apply-cont cont (bool-val #t)))
              (else (apply-cont cont (bool-val #f)))))
     (lambda (val)
       (apply-handler cont val)))))
;; list1-cont : Exp x Env x Cont -> Cont
(define list1-cont
  (lambda (exps env cont)
    (cons
     (lambda (val)
       (value-of/k exps env (list2-cont val cont)))
     (lambda (val)
       (apply-handler cont val)))))
;; list2-cont : ExpVal x Cont -> Cont
(define list2-cont
  (lambda (first-val cont)
    (cons
     (lambda (val)
       (apply-cont cont (pair-val first-val val)))
     (lambda (val)
       (apply-handler cont val)))))
;; cdr-cont : Cont -> Cont
(define cdr-cont
  (lambda (cont)
    (cons
     (lambda (val)
       (cases exp-val val
              (pair-val
               (first rest)
               (apply-cont cont rest))
              (else (report-invalid-exp-value 'pair-val))))
     (lambda (val)
       (apply-handler cont val)))))
;; if-test-cont : Exp x Exp x Env x Cont -> Cont
(define if-test-cont
  (lambda (exp2 exp3 env cont)
    (cons
     (lambda (val)
       (if (expval->bool val)
           (value-of/k exp2 env cont)
           (value-of/k exp3 env cont)))
     (lambda (val)
       (apply-handler cont val)))))
;; diff1-cont : Exp x Env x Cont -> Cont
(define diff1-cont
  (lambda (exp2 env cont)
    (cons
     (lambda (val)
       (value-of/k exp2 env (diff2-cont val cont)))
     (lambda (val)
       (apply-handler cont val)))))
;; diff2-cont : ExpVal x Cont -> Cont
(define diff2-cont
  (lambda (val1 cont)
    (cons
     (lambda (val2)
       (let ((num1 (expval->num val1))
             (num2 (expval->num val2)))
         (apply-cont cont (num-val (- num1 num2)))))
     (lambda (val2)
       (apply-handler cont val2)))))
;; rand-cont : Exp x Env x Cont -> Cont
(define rator-cont
  (lambda (rand env cont)
    (cons
     (lambda (val)
       (value-of/k rand env (rand-cont val cont)))
     (lambda (val)
       (apply-handler val)))))
;; rand-cont : ExpVal x Cont -> Cont
(define rand-cont
  (lambda (rator cont)
    (cons
     (lambda (val)
       (let ((proc1 (expval->proc rator)))
         (apply-procedure/k proc1 val cont)))
     (lambda (val)
       (apply-handler cont val)))))
;; let2-exp-cont : Var x Var x Exp x Exp x Env x Cont -> Cont
(define let2-exp-cont
  (lambda (var1 var2 exp2 body env cont)
    (cons
     (lambda (val1)
       (value-of/k
        exp2 env
        (let-exp-cont var2 body (extend-env var1 val1 env) cont)))
     (lambda (val1)
       (apply-handler cont val1)))))
;; try-cont : Var x Exp x Env x Cont -> Cont
(define try-cont
  (lambda (var handler-exp env cont)
    (cons
     (lambda (val)
       (apply-cont cont val))
     (lambda (val)
       (value-of/k handler-exp (extend-env var val env) cont)))))
;; raise1-cont : Cont -> Cont
(define raise1-cont
  (lambda (cont)
    (cons
     (lambda (val)
       (apply-handler cont val))
     (lambda (val)
       (apply-handler cont val)))))
;; apply-cont : Cont x ExpVal -> FinalAnswer
(define apply-cont
  (lambda (cont v)
    ((car cont) v)))
;; apply-handler : Cont x ExpVal -> FinalAnswer
(define apply-handler
  (lambda (cont v)
    ((cdr cont) v)))
(define report-uncaught-exception
  (lambda ()
    (eopl:printf "Uncaught exception!~%")))

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
;;; Expression ::= letrec Identifier (Identifier) = Expression in Expression
;;;                let-exp (var exp1 body)
;;; Expression ::= proc (Identifier) Expression
;;;                proc-exp (var body)
;;; Expression ::= (Expression Expression)
;;;                call-exp (rator rand)
;;; Expression ::= try Expression catch (Identifier) Expression
;;;                try-exp (exp1 var handler-exp)
;;; Expression ::= raise Expression
;;;                raise-exp (exp)
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
    (expression ("let" identifier "=" expression "in" expression)
                let-exp)
    (expression ("let2" identifier "=" expression identifier "=" expression "in" expression)
                let2-exp)
    (expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression)
                letrec-exp)
    (expression ("proc" "(" identifier ")" expression)
                proc-exp)
    (expression ("(" expression expression ")")
                call-exp)
    (expression ("try" expression "catch" "(" identifier ")" expression)
                try-exp)
    (expression ("raise" expression)
                raise-exp)
))

;;; ---------------------- Evaluate expression ----------------------
;; value-of/k : Exp x Env x Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
           (const-exp
            (num)
            (apply-cont cont (num-val num)))
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
            (value-of/k exp1 env (let-exp-cont var body env cont)))
           (let2-exp
            (var1 exp1 var2 exp2 body)
            (value-of/k exp1 env (let2-exp-cont var1 var2 exp2 body env cont)))
           (letrec-exp
            (p-name p-var p-body letrec-body)
            (value-of/k letrec-body (extend-env-rec p-name p-var p-body env) cont))
           (proc-exp
            (var body)
            (apply-cont cont (proc-val (procedure var body env))))
           (call-exp
            (rator rand)
            (value-of/k rator env (rator-cont rand env cont)))
           (try-exp
            (exp1 var handler-exp)
            (value-of/k exp1 env (try-cont var handler-exp env cont)))
           (raise-exp
            (exp1)
            (value-of/k exp1 env (raise1-cont cont))))))
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
(require rackunit)
(check-eqv?
 (run "letrec double (x) = if zero?(x) then 0
                       else -((double -(x,1)),-2)
       in (double 6)")
 12)

;; tests from exercise 3.16
(check-equal?
 (run "let x = 4 in cons(x, cons(cons(-(x,1), emptylist), emptylist))")
 '(4 (3)))
(check-eqv?
 (run "null?(cdr(let x = 4 in
                 cons(x, cons(cons(-(x,1),
                                   emptylist),
                              emptylist))))")
 #f)
(check-equal?
 (run
  "let x = 4 in list(x, -(x,1), -(x,3))")
 '(4 3 1))

;; tests from exercise 3.21
(check-eqv?
 (run "((proc (x) proc (y) -(y,-(0,x)) 3) 4)")
 7)

;; tests from exercise 4.17 and 4.18
(check-eqv?
 (run "let x = 22
        in let f = proc (z)
                    let zz = -(z,x)
                    in zz
           in -((f 66), (f 55))")
 11)
(check-eqv?
 (run "letrec times4(x) = if zero?(x) then 0
                         else -((times4 -(x,1)), -4)
      in (times4 3)")
 12)

;;; exception
(check-eqv?
 (run
  "let index = proc (n)
        letrec inner (lst)
          = if null? (lst)
            then raise 99
            else if zero?(-(car(lst), n))
                 then 0
                 else -((inner cdr(lst)), -1)
        in proc (lst)
           try (inner lst)
           catch (x) -1
   in ((index 5) list(2, 3))")
 -1)

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
         (eqv? (car env) 'extend-env))))
(define environment?
  (lambda (env)
    (or (empty-env? env)
        (extended-env? env))))
(define empty-env
  (lambda () (list 'empty-env)))
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))
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
      (else
       (report-invalid-env env)))))
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
   (var identifier?)
   (body expression?)
   (saved-env environment?)))
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
           (procedure
            (var body saved-env)
            (value-of body (extend-env var val saved-env))))))
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
    (expression ("proc" "(" identifier ")" expression)
                proc-exp)
    (expression ("(" expression expression ")")
                call-exp)))

;;; ---------------------- Evaluate expression ----------------------
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
            (value-of body (extend-env var (value-of exp1 env) env)))
           (proc-exp
            (var body)
            (proc-val (procedure var body env)))
           (call-exp
            (rator rand)
            (let ((proc (expval->proc (value-of rator env)))
                  (arg (value-of rand env)))
              (apply-procedure proc arg))))))
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
;; exer 3.23
(eqv?
 (run "let makemult = proc (maker)
                       proc (x)
                         if zero? (x)
                         then 0
                         else -(((maker maker) -(x, 1)), -4)
       in let times4 = proc (x) ((makemult makemult) x)
          in (times4 3)")
 12)

(eqv?
 (run "let makemult = proc (maker)
                       proc (x)
                         proc (y)
                           if zero? (x) then 0
                           else -((((maker maker) -(x, 1)) y), -(0, y))
       in let times4 = proc (x) (((makemult makemult) x) 4)
         in (times4 3)")
 12)

(eqv?
 (run "let makemult = proc (maker)
                       proc (x)
                         proc (y)
                           if zero? (x) then 0
                           else -((((maker maker) -(x, 1)) y), -(0, y))
      in let times = proc (z) proc (x) (((makemult makemult) x) z)
         in ((times 3) 4)")
 12)

(eqv?
 (run "let makemult = proc (maker)
                       proc (x)
                         proc (y)
                           if zero? (x) then 0
                           else -((((maker maker) -(x, 1)) y), -(0, y))
      in let times = proc (z) proc (x) (((makemult makemult) x) z)
         in let makefact = proc (maker)
                         proc (x)
                           if zero? (x) then 1
                           else ((times ((maker maker) -(x, 1))) x)
            in let fact = proc (x) ((makefact makefact) x)
               in (fact 4)")
 24)

;; exer 3.24
(eqv?
 (run "let makeodd = proc (maker)
                      proc (maker2)
                        proc (x)
                          if zero? (x) then 0
                          else (((maker maker2) maker) -(x, 1))
      in let makeeven = proc (maker)
                          proc (maker2)
                            proc (x)
                              if zero? (x) then 1
                              else (((maker maker2) maker) -(x, 1))
         in let odd = proc (x) (((makeodd makeeven) makeodd) x)
            in (odd 5)")
 1)

(eqv?
 (run "let makeodd = proc (maker)
                      proc (maker2)
                        proc (x)
                          if zero? (x) then 0
                          else (((maker maker2) maker) -(x, 1))
       in let makeeven = proc (maker)
                          proc (maker2)
                            proc (x)
                              if zero? (x) then 1
                              else (((maker maker2) maker) -(x, 1))
          in let even = proc (x) (((makeeven makeodd) makeeven) x)
             in (even 5)")
 0)

;; exer 3.25
(eqv?
 (run "let makerec = proc (f)
        let d = proc (x)
                  proc (z) ((f (x x)) z)
        in proc (n) ((f (d d)) n)
       in let maketimes4 = proc (f)
                            proc (x)
                              if zero?(x) then 0
                              else -((f -(x,1)), -4)
          in let times4 = (makerec maketimes4)
             in (times4 3)")
 12)

#lang eopl
;;; procedural version as in exercise 5.41, this is inlined version
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
  (lambda ()
    (lambda (val)
      (begin
        (eopl:printf "End of computation.~%")
        val))))
;; end-enp-cont : () -> Unspecified
(define end-enp-cont
  (lambda ()
    (lambda (val)
      (begin
        (report-uncaught-exception)
        #f))))
;;; is it possible to inline a recursive continuation without using Y
;;; combinator?
(define let-cont
  (lambda (saved-vars saved-vals cont-vars cont-exps body env cont enp-cont)
    (lambda (val)
      (if (null? cont-exps)
          (let ((l-vars (reverse (cons (car cont-vars) saved-vars)))
                (l-vals (reverse (cons val saved-vals))))
            (value-of/k body
                        (extend-env* l-vars
                                     l-vals
                                     env)
                        cont
                        enp-cont))
          (value-of/k (car cont-exps)
                      env
                      (let-cont (cons (car cont-vars) saved-vars)
                                (cons val saved-vals)
                                (cdr cont-vars)
                                (cdr cont-exps)
                                body
                                env
                                cont
                                enp-cont)
                      enp-cont)))))
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
;; apply-procedure/k : Proc x ExpVal x Cont x Cont -> ExpVal
(define apply-procedure/k
  (lambda (proc1 val cont enp-cont)
    (cases proc proc1
           (procedure
            (var body saved-env)
            (value-of/k body (extend-env var val saved-env) cont enp-cont)))))
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
;;; Expression ::= let {Identifier = Expression}{*} in Expression
;;;                let-exp (vars exps body)
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
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    (expression ("letrec" (arbno identifier "(" identifier ")" "=" expression) "in" expression)
                letrec-exp)
    (expression ("proc" "(" identifier ")" expression)
                proc-exp)
    (expression ("(" expression expression ")")
                call-exp)
    (expression ("try" expression "catch" "(" identifier ")" expression)
                try-exp)
    (expression ("raise" expression)
                raise-exp)))

;;; ---------------------- Evaluate expression ----------------------
;; value-of/k : Exp x Env x Cont x Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont enp-cont)
    (cases expression exp
           (const-exp
            (num)
            (cont (num-val num)))
           (emptylist-exp
            ()
            (cont (null-val)))
           (cons-exp
            (exp1 exp2)
            (value-of/k exp1
                        env
                        (lambda (val)
                          (value-of/k exp2
                                      env
                                      (lambda (val2)
                                        (cont (pair-val val val2)))
                                      enp-cont))
                        enp-cont))
           (car-exp
            (exp1)
            (value-of/k exp1
                        env
                        (lambda (val)
                          (cases exp-val val
                                 (pair-val
                                  (first rest)
                                  (cont first))
                                 (else (report-invalid-exp-value 'pair-val))))
                        enp-cont))
           (cdr-exp
            (exp1)
            (value-of/k exp1
                        env
                        (lambda (val)
                          (cases exp-val val
                                 (pair-val
                                  (first rest)
                                  (cont rest))
                                 (else (report-invalid-exp-value 'pair-val))))
                        enp-cont))
           (null?-exp
            (exp1)
            (value-of/k exp1
                        env
                        (lambda (val)
                          (cases exp-val val
                                 (null-val
                                  ()
                                  (cont (bool-val #t)))
                                 (else (cont (bool-val #f)))))
                        enp-cont))
           (list-exp
            (exps)
            (if (null? exps)
                (cont (null-val))
                (value-of/k (car exps)
                            env
                            (lambda (val)
                              (value-of/k (list-exp (cdr exps))
                                          env
                                          (lambda (val2)
                                            (cont (pair-val val val2)))
                                          enp-cont))
                            enp-cont)))
           (var-exp
            (var)
            (cont (apply-env env var)))
           (diff-exp
            (exp1 exp2)
            (value-of/k exp1
                        env
                        (lambda (val1)
                          (value-of/k exp2
                                      env
                                      (lambda (val2)
                                        (let ((num1 (expval->num val1))
                                              (num2 (expval->num val2)))
                                          (cont (num-val (- num1 num2)))))
                                      enp-cont))
                        enp-cont))
           (zero?-exp
            (exp1)
            (value-of/k exp1
                        env
                        (lambda (val)
                          (cont (bool-val
                                 (zero? (expval->num val)))))
                        enp-cont))
           (if-exp
            (exp1 exp2 exp3)
            (value-of/k exp1
                        env
                        (lambda (val)
                          (if (expval->bool val)
                              (value-of/k exp2 env cont enp-cont)
                              (value-of/k exp3 env cont enp-cont)))
                        enp-cont))
           (let-exp
            (vars exps body)
            (if (null? exps)
                (value-of/k body env cont)
                (value-of/k (car exps)
                            env
                            (let-cont '() '() vars (cdr exps) body env cont enp-cont)
                            enp-cont)))
           (letrec-exp
            (p-names p-vars p-bodies letrec-body)
            (value-of/k letrec-body (extend-env-rec p-names p-vars p-bodies env) cont enp-cont))
           (proc-exp
            (var body)
            (cont (proc-val (procedure var body env))))
           (call-exp
            (rator rand)
            (value-of/k rator
                        env
                        (lambda (val)
                          (value-of/k rand
                                      env
                                      (lambda (val2)
                                        (let ((proc1 (expval->proc val)))
                                          (apply-procedure/k proc1 val2 cont enp-cont)))
                                      enp-cont))
                        enp-cont))
           (try-exp
            (exp1 var handler-exp)
            (value-of/k exp1
                        env
                        cont
                        (lambda (val)
                          (value-of/k handler-exp (extend-env var val env) cont enp-cont))))
           (raise-exp
            (exp1)
            (value-of/k exp1
                        env
                        enp-cont
                        enp-cont)))))
;; value-of-program : Program -> FinalAnswer
(define value-of-program
  (lambda (prog)
    (cases program prog
           (a-program
            (exp)
            (let ((val (value-of/k exp (empty-env) (end-cont) (end-enp-cont))))
              (if (exp-val? val)
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
                          (expval->pair val)))
                  val))))))

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
 (run "let x = 3 f = proc(x) -(x, -3) in (f 3)")
 6)
(check-eqv?
 (run "let x = 3 f = proc(x) -(x, -3) y = 4 in -(x, y)")
 -1)

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
(check-eqv?
 (run "let x = 30
         in let x = -(x,1)
                y = -(x,2)
           in -(x,y)")
 1)

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
   in raise ((index 5) list(2, 3))")
 #f)

(check-eqv?
 (run
  "try
     let x = 3 in
       -(3, -(2, raise x))
   catch (y)
     -(y, 3)")
 0)

(check-eqv?
 (run
  "let x = 3 in
   try raise raise 18
   catch (x)
    try
     raise -(x, raise x)
    catch (x)
     x")
 18)

(check-eqv?
 (run
  "let x = 3 in
    raise -(x, raise x)")
 #f)

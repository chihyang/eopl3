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
               (newref (proc-val (procedure saved-p-vars saved-p-body env)))))))
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

;;; ---------------------- Expval ----------------------
(define identifier? symbol?)
(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body expression?)
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
(define report-invalid-exp-value
  (lambda (type)
    (eopl:error
     'exp-val
     "Not a valid exp value of type ~s" type)))

;;; ---------------------- Store (from section 4.2) ----------------------
;; empty-store : () -> Sto
(define empty-store (lambda () '()))
;; usage : A scheme variable containing the current state of the
;; store. Initially set to a dummy value.
(define the-store 'uninitialized)
;; get-store : () -> Sto
(define get-store
  (lambda () the-store))
;; initialize-store! : () -> Unspecified
;; usage : (initialize-store!) sets the store to the empty store
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))
;; reference? : SchemeVal -> Bool
(define reference?
  (lambda (v)
    (integer? v)))
;; newref : ExpVal -> Ref
(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))
;; deref : Ref -> ExpVal
(define deref
  (lambda (ref)
    (list-ref the-store ref)))
;; setref! : Ref x ExpVal -> Unspecified
;; usage : sets the-store to a state like the original, but with position ref
;; containing val
(define setref!
  (lambda (ref val)
    (set! the-store
      (letrec ((setref-inner
                ;; usage : returns a list like store1, except that position ref1
                ;; contains val.
                (lambda (store1 ref1)
                  (cond [(null? store1)
                         (report-invalid-reference ref the-store)]
                        [(zero? ref1)
                         (cons val (cdr store1))]
                        [else
                         (cons (car store1)
                               (setref-inner (cdr store1) (- ref1 1)))]))))
        (setref-inner the-store ref)))))
(define report-invalid-reference
  (lambda (ref store)
    (eopl:error
     'exp-val
     "Not a valid reference ~a for store ~a" ref store)))

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
   (exps (list-of expression?))
   (env environment?)
   (cont continuation?))
  (rand-cont
   (val exp-val?)
   (saved-rands (list-of exp-val?))
   (cont-exps (list-of expression?))
   (env environment?)
   (cont continuation?))
  (set-rhs-cont
   (ref reference?)
   (cont continuation?)))
;; apply-cont : () -> FinalAnswer
;; usage      : reads registers
;;  cont      : Cont
;;  val       : ExpVal
(define apply-cont
  (lambda ()
    (cases continuation cont
           (end-cont
            ()
            (begin
              (eopl:printf "End of computation.~%")
              val))
           (zero1-cont
            (cont1)
            (set! cont cont1)
            (set! val (bool-val (zero? (expval->num val))))
            (apply-cont))
           (let-exp-cont
            (var body saved-env cont1)
            (set! cont cont1)
            (set! exp body)
            (set! env (extend-env var (newref val) saved-env))
            (value-of/k))
           (if-test-cont
            (exp2 exp3 env1 cont1)
            (set! cont cont1)
            (if (expval->bool val)
                (set! exp exp2)
                (set! exp exp3))
            (set! env env1)
            (value-of/k))
           (diff1-cont
            (exp2 env1 cont1)
            (set! cont (diff2-cont val cont1))
            (set! exp exp2)
            (set! env env1)
            (value-of/k))
           (diff2-cont
            (val1 cont1)
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val)))
              (set! cont cont1)
              (set! val (num-val (- num1 num2)))
              (apply-cont)))
           (rator-cont
            (exps env1 cont1)
            (if (null? exps)
                (begin
                  (set! cont cont1)
                  (set! proc1 (expval->proc val))
                  (set! val '())
                  (apply-procedure/k))
                (begin
                  (set! exp (car exps))
                  (set! env env1)
                  (set! cont (rand-cont val '() (cdr exps) env1 cont1))
                  (value-of/k))))
           (rand-cont
            (rator saved-vals cont-exps env1 cont1)
            (if (null? cont-exps)
                (begin
                  (set! cont cont1)
                  (set! proc1 (expval->proc rator))
                  (set! val (reverse (cons val saved-vals)))
                  (apply-procedure/k))
                (begin
                  (set! cont (rand-cont rator (cons val saved-vals) (cdr cont-exps) env1 cont1))
                  (set! exp (car cont-exps))
                  (set! env env1)
                  (value-of/k))))
           (set-rhs-cont
            (ref cont1)
            (begin
              (setref! ref val)
              (set! cont cont1)
              (set! val (num-val 27))
              (apply-cont))))))

;;; ---------------------- Syntax for the IMPLICIT-REFERENCE-CPS language ----------------------
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
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
                letrec-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression)
                proc-exp)
    (expression ("(" expression (arbno expression) ")")
                call-exp)
    (expression ("set" identifier "=" expression)
                assign-exp)))

;;; ---------------------- Evaluate expression ----------------------
;; apply-procedure/k : () -> FinalAnswer
;; usage             : reads on registers
;;  proc             : Exp
;;   val             : Listof(ExpVal)
;;  cont             : Cont
(define apply-procedure/k
  (lambda ()
    (cases proc proc1
           (procedure
            (vars body saved-env)
            (set! exp body)
            (set! env (extend-env* vars (map newref val) saved-env))
            (value-of/k)))))
;; value-of/k : () -> FinalAnswer
;; usage : reads on registers
;;   exp : Exp
;;   env : Env
;;  cont : Cont
(define value-of/k
  (lambda ()
    (cases expression exp
           (const-exp
            (num)
            (set! val (num-val num))
            (apply-cont))
           (var-exp
            (var)
            (set! val (deref (apply-env env var)))
            (apply-cont))
           (diff-exp
            (exp1 exp2)
            (set! cont (diff1-cont exp2 env cont))
            (set! exp exp1)
            (value-of/k))
           (zero?-exp
            (exp1)
            (set! cont (zero1-cont cont))
            (set! exp exp1)
            (value-of/k))
           (if-exp
            (exp1 exp2 exp3)
            (set! cont (if-test-cont exp2 exp3 env cont))
            (set! exp exp1)
            (value-of/k))
           (let-exp
            (var exp1 body)
            (set! exp exp1)
            (set! cont (let-exp-cont var body env cont))
            (value-of/k))
           (letrec-exp
            (p-names p-vars p-bodies letrec-body)
            (set! exp letrec-body)
            (set! env (extend-env-rec p-names p-vars p-bodies env))
            (value-of/k))
           (proc-exp
            (vars body)
            (set! val (proc-val (procedure vars body env)))
            (apply-cont))
           (call-exp
            (rator rand)
            (set! cont (rator-cont rand env cont))
            (set! exp rator)
            (value-of/k))
           (assign-exp
            (var exp1)
            (let ((ref (apply-env env var)))
              (set! cont (set-rhs-cont ref cont))
              (set! exp exp1)
              (value-of/k))))))
(define exp 'uninitialized)
(define env 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define proc1 'uninitialized)
;; value-of-program : Program -> FinalAnswer
(define value-of-program
  (lambda (prog)
    (initialize-store!)
    (cases program prog
           (a-program
            (exp1)
            (set! exp exp1)
            (set! env (empty-env))
            (set! cont (end-cont))
            (value-of/k)
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
                      (expval->pair val)))))))

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

;; tests from exercise 3.21
(eqv?
 (run "((proc (x) proc (y) -(y,-(0,x)) 3) 4)")
 7)
(eqv?
 (run "(proc (x, y) -(y,-(0,x)) 3 4)")
 7)

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

;; tests from exercise 4.17 and 4.18
(eqv?
 (run "let x = 22
        in let f = proc (z)
                    let zz = -(z,x)
                    in zz
           in -((f 66), (f 55))")
 11)
(eqv?
 (run "letrec times4(x) = if zero?(x) then 0
                         else -((times4 -(x,1)), -4)
      in (times4 3)")
 12)
(eqv?
 (run "let x = 0
      in letrec even()
                 = if zero? (x)
                   then 1
                   else let d = set x = -(x, 1) in
                         (odd)
                odd()
                 = if zero? (x)
                   then 0
                   else let d = set x = -(x, 1) in
                         (even)
      in let d = set x = 13
         in (odd)")
 1)

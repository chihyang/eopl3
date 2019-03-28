#lang eopl
;;; ---------------------- Environment (from section 3.2) ----------------------
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
(define extend-env*
  (lambda (vars vals env)
    (let ((duplicate (check-duplicates vars))
          (var-len (length vars))
          (val-len (length vals)))
      (cond [(not (null? duplicate))
             (report-duplicate-id duplicate)]
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
(define extend-env-rec
  (lambda (p-names p-vars p-bodys env)
    (let ((dup-name (check-duplicates p-names)))
      (if (null? dup-name)
          (list 'extend-env-rec (list p-names p-vars p-bodys) env)
          (report-duplicate-id dup-name)))))
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
  (lambda (var p-names p-vars p-bodys)
    (cond [(null? p-names) '()]
          [(eqv? var (car p-names))
           (list (car p-vars) (car p-bodys))]
          [else
           (apply-env-rec var (cdr p-names) (cdr p-vars) (cdr p-bodys))])))
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
            (value-of body (extend-env* vars vals saved-env))))))
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
   (val2 exp-val?))
  (ref-val
   (val number?)))
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
                    (proc-val (proc) proc)
                    (ref-val (ref) ref)
                    (pair-val (val3 val4) (expval->pair val1)))
             (cases exp-val val2
                    (num-val (num) num)
                    (bool-val (bool) bool)
                    (null-val () '())
                    (proc-val (proc) proc)
                    (ref-val (ref) ref)
                    (pair-val (val3 val4) (expval->pair val2)))))
           (else
            (report-invalid-exp-value 'pair)))))
(define expval->proc
  (lambda (value)
    (cases exp-val value
           (proc-val
            (proc1)
            proc1)
           (else
            (report-invalid-exp-value 'proc)))))
(define expval->ref
  (lambda (value)
    (cases exp-val value
           (ref-val
            (ref)
            ref)
           (else
            (report-invalid-exp-value 'ref)))))
(define report-invalid-exp-value
  (lambda (type)
    (eopl:error
     'exp-val
     "Not a valid exp value of type ~s" type)))

;;; ---------------------- Store (from section 4.2) ----------------------
;; empty-store : () -> Sto
(define empty-store (lambda () (cons 0 (make-vector 2))))
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
    (let ((next-ref (car the-store)))
      (if (>= next-ref (vector-length (cdr the-store)))
          (report-out-of-memory the-store)
          (begin
            (vector-set! (cdr the-store) next-ref val)
            (set! the-store (cons (+ next-ref 1) (cdr the-store)))
            next-ref)))))
;; deref : Ref -> ExpVal
(define deref
  (lambda (ref)
    (vector-ref (cdr the-store) ref)))
;; setref! : Ref x ExpVal -> Unspecified
;; usage : sets the-store to a state like the original, but with position ref
;; containing val
(define setref!
  (lambda (ref val)
    (cond [(< ref (car the-store))
           (vector-set! (cdr the-store) ref val)]
          [else
           (report-invalid-reference ref the-store)])))
(define report-invalid-reference
  (lambda (ref store)
    (eopl:error
     'exp-val
     "Not a valid reference ~a for store ~a" ref store)))
(define report-out-of-memory
  (lambda (store)
    (eopl:error
     'exp-val
     "Reference out of range for store ~a" store)))

;;; ---------------------- Syntax for the PROC language ----------------------
;;; Program    ::= Expression
;;;                a-program (exp1)
;;; Expression ::= Number
;;;                const-exp (num)
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
;;; Expression ::= list (Expression, Expression, ...)
;;;                pair-exp (exp1)
;;; Expression ::= null? (Expression)
;;;                null?-exp (exp1)
;;; Expression ::= -(Expression , Expression)
;;;                diff-exp (exp1 exp2)
;;; Expression ::= zero? (Expression)
;;;                zero?-exp (exp1)
;;; Expression ::= if Expression then Expression else Expression
;;;                if-exp (exp1 exp2 exp3)
;;; Expression ::= let Identifier = Expression in Expression
;;;                let-exp (var exp1 body)
;;; Expression ::= proc (Identifier*,) Expression
;;;                proc-exp (var body)
;;; Expression ::= letrec Identifier (Identifier*,) = Expression in Expression
;;;                letrec-exp (p-name b-var p-exp1 letrec-body)
;;; Expression ::= (Expression Expression*)
;;;                call-exp (rator rand)
;;; Expression ::= begin Expression {; Expression}* end
;;;                begin-exp (exp1 exps)
;;; Expression ::= newref (Expression)
;;;                newref-exp (exp1)
;;; Expression ::= deref (Expression)
;;;                deref-exp (exp1)
;;; Expression ::= setref (Expression , Expression)
;;;                setref-exp (exp1 exp2)
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
    (expression (identifier)
                var-exp)
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
    (expression ("let" identifier "=" expression "in" expression)
                let-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
                letrec-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression)
                proc-exp)
    (expression ("(" expression (arbno expression) ")")
                call-exp)
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)
    (expression ("newref" "(" expression ")")
                newref-exp)
    (expression ("deref" "(" expression ")")
                deref-exp)
    (expression ("setref" "(" expression "," expression ")")
                setref-exp)))

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
           (emptylist-exp
            ()
            (value-of-emptylist-exp env))
           (cons-exp
            (exp1 exp2)
            (value-of-cons-exp exp1 exp2 env))
           (car-exp
            (exp1)
            (value-of-car-exp exp1 env))
           (cdr-exp
            (exp1)
            (value-of-cdr-exp exp1 env))
           (null?-exp
            (exp1)
            (value-of-null?-exp exp1 env))
           (list-exp
            (exp1)
            (value-of-list-exp exp1 env))
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
           (letrec-exp
            (p-names p-vars p-bodys letrec-body)
            (value-of letrec-body (extend-env-rec p-names p-vars p-bodys env)))
           (proc-exp
            (vars body)
            (proc-val (procedure vars body env)))
           (call-exp
            (rator rand)
            (let ((proc (expval->proc (value-of rator env)))
                  (args (map (lambda (exp1) (value-of exp1 env)) rand)))
              (apply-procedure proc args)))
           (begin-exp
            (exp1 exps)
            (cond [(null? exps)
                   (value-of exp1 env)]
                  [else
                   (begin
                     (value-of exp1 env)
                     (value-of (begin-exp (car exps) (cdr exps)) env))]))
           (newref-exp
            (exp1)
            (let ((v1 (value-of exp1 env)))
              (ref-val (newref v1))))
           (deref-exp
            (exp1)
            (let ((v1 (value-of exp1 env)))
              (let ((ref1 (expval->ref v1)))
                (deref ref1))))
           (setref-exp
            (exp1 exp2)
            (let ((ref (expval->ref (value-of exp1 env))))
              (let ((val2 (value-of exp2 env)))
                (begin
                  (setref! ref val2)
                  (num-val 23))))))))
;; value-of-program : Program -> SchemeVal
(define value-of-program
  (lambda (prog)
    (initialize-store!)
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
                     (null-val
                      ()
                      '())
                     (pair-val
                      (val1 val2)
                      (expval->pair val))
                     (proc-val
                      (val)
                      val)
                     (ref-val
                      (val)
                      val)))))))
(define value-of-emptylist-exp
  (lambda (env)
    (null-val)))
(define value-of-cons-exp
  (lambda (car-val cdr-val env)
    (pair-val (value-of car-val env)
              (value-of cdr-val env))))
(define value-of-car-exp
  (lambda (exp env)
    (cases exp-val (value-of exp env)
           (pair-val
            (val1 val2)
            val1)
           (else (report-invalid-exp-value 'pair-val)))))
(define value-of-cdr-exp
  (lambda (exp env)
    (cases exp-val (value-of exp env)
           (pair-val
            (val1 val2)
            val2)
           (else (report-invalid-exp-value 'pair-val)))))
(define value-of-list-exp
  (lambda (exp env)
    (if (null? exp)
        (null-val)
        (pair-val (value-of (car exp) env)
                  (value-of (list-exp (cdr exp)) env)))))
(define value-of-null?-exp
  (lambda (exp env)
    (cases exp-val (value-of exp env)
           (null-val
            ()
            (bool-val #t))
           (else (bool-val #f)))))

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
(equal?
 (run "let x = 4 in cons(x, cons(cons(-(x,1), emptylist), emptylist))")
 '(4 (3)))
(eqv?
 (run "null?(cdr(let x = 4 in
               cons(x,
                 cons(cons(-(x,1), emptylist),
                   emptylist))))")
 #f)
(equal?
 (run "let x = 4 in list(x, -(x,1), -(x,3))")
 '(4 3 1))
(eqv?
 (run "begin
        letrec double (x) = if zero?(x) then 0
                            else -((double -(x,1)),-2)
        in (double 6);
        let x = 5 in -(x, 6)
       end")
 -1)
(eqv?
 (run "let x = newref (0)
      in letrec even(dummy)
                 = if zero? (deref(x))
                   then 1
                   else begin
                         setref(x, -(deref(x), 1));
                         (odd 888)
                        end
                odd(dummy)
                 = if zero? (deref(x))
                   then 0
                   else begin
                         setref(x, -(deref(x), 1));
                         (even 888)
                        end
      in begin setref(x,13); (odd 888) end")
 1)
(eqv?
 (run "let x = newref (0)
      in letrec even()
                 = if zero? (deref(x))
                   then 1
                   else begin
                         setref(x, -(deref(x), 1));
                         (odd)
                        end
                odd()
                 = if zero? (deref(x))
                   then 0
                   else begin
                         setref(x, -(deref(x), 1));
                         (even)
                        end
      in begin setref(x,13); (odd) end")
 1)
(equal?
 (run "let x = newref(12) in
         list(deref(x), -(deref(x),1), -(deref(x),3))")
 '(12 11 9))
(equal?
 (run "let x = newref(12) in
         list(deref(x),
              begin
               setref(x,-(deref(x),1));
               deref(x)
              end,
              begin
               setref(x,-(deref(x),2));
               deref(x)
              end)")
 '(12 11 9))
(equal?
 (run "let z = newref(cons(1,2)) in
       let y = newref(emptylist) in
       let x = newref(12) in
         list(deref(x),
              begin
               setref(x,-(deref(x),1));
               deref(x)
              end,
              begin
               setref(x,-(deref(x),2));
               deref(x)
              end)")
 '(12 11 9))

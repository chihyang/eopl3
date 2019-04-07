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

(define-datatype environment environment?
  (empty-env-inner)
  (extend-env-inner
   (vars (list-of identifier?))
   (vals vector?)
   (env environment?)))
;; empty-env : () -> Env
(define empty-env empty-env-inner)
;; extend-env : Id x ExpVal x Env -> Env
(define extend-env
  (lambda (var val env)
    (extend-env-inner (list var) (make-vector 1 val) env)))
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
             (extend-env-inner vars (list->vector vals) env)]))))
(define extend-env-rec
  (lambda (p-names b-vars bodies saved-env)
    (let ((vec (make-vector (length p-names))))
      (let ((new-env (extend-env-inner p-names vec saved-env)))
        (make-proc-vec! vec 0 b-vars bodies new-env)
        new-env))))
(define make-proc-vec!
  (lambda (vec n b-vars bodies env)
    (cond ([null? b-vars] vec)
          (else
           (vector-set!
            vec n (newref (proc-val (procedure (car b-vars) (car bodies) env))))
           (make-proc-vec! vec (+ n 1) (cdr b-vars) (cdr bodies) env)))))
(define apply-env
  (lambda (env search-var)
    (cases environment env
           (empty-env-inner
            ()
            (report-no-binding-found search-var))
           (extend-env-inner
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

;; init-env : () → Env
;; usage: (init-env) = [i= ^1^, v= ^5^, x= ^10^]
(define init-env
  (lambda ()
    (extend-env
     'i (newref (num-val 1))
     (extend-env
      'v (newref (num-val 5))
      (extend-env
       'x (newref (num-val 10))
       (empty-env))))))
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
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
           (procedure
            (vars body saved-env)
            (value-of body (extend-env* vars (map newref vals) saved-env))))))
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
                    (pair-val (val3 val4) (expval->pair val1)))
             (cases exp-val val2
                    (num-val (num) num)
                    (bool-val (bool) bool)
                    (null-val () '())
                    (proc-val (proc) proc)
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
;;; Expression ::= let {Identifier = Expression}* in Expression
;;;                let-exp (var exp1 body)
;;; Expression ::= proc (Identifier*,) Expression
;;;                proc-exp (var body)
;;; Expression ::= letrec {Identifier (Identifier*,)}* = Expression in Expression
;;;                letrec-exp (p-name b-var p-exp1 letrec-body)
;;; Expression ::= (Expression Expression*)
;;;                call-exp (rator rand)
;;; Expression ::= begin Expression {; Expression}* end
;;;                begin-exp (exp1 exps)
;;; Expression ::= set Identifier = Expression
;;;                assign-exp (var exp1)
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
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
                letrec-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression)
                proc-exp)
    (expression ("(" expression (arbno expression) ")")
                call-exp)
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)
    (expression ("set" identifier "=" expression)
                assign-exp)))

;;; ---------------------- Evaluate expression ----------------------
(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp
            (num)
            (num-val num))
           (var-exp
            (var)
            (deref (apply-env env var)))
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
            (vars exps body)
            (let ((vals (map (lambda (exp1) (value-of exp1 env)) exps)))
              (value-of body (extend-env* vars (map newref vals) env))))
           (letrec-exp
            (p-names p-vars p-bodies letrec-body)
            (value-of letrec-body (extend-env-rec p-names p-vars p-bodies env)))
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
           (assign-exp
            (var exp1)
            (begin
              (setref!
               (apply-env env var)
               (value-of exp1 env))
              (num-val 27))))))
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
 (run "let g =
        let counter = 0
        in proc (dummy)
             begin
               set counter = -(counter, -1);
               counter
             end
       in let a = (g 11)
          in let b = (g 11)
             in -(a,b)")
 -1)
(eqv?
 (run "let g = proc (dummy)
                 let counter = 0
                 in begin
                      set counter = -(counter, -1);
                      counter
                    end
       in let a = (g 11)
          in let b = (g 11)
             in -(a,b)")
 0)
(eqv?
 (run "let x = 0
      in letrec even(dummy)
                 = if zero? (x)
                   then 1
                   else begin
                         set x = -(x, 1);
                         (odd 888)
                        end
                odd(dummy)
                 = if zero? (x)
                   then 0
                   else begin
                         set x = -(x, 1);
                         (even 888)
                        end
      in begin set x = 13; (odd 888) end")
 1)
(eqv?
 (run "let x = 0
      in letrec even()
                 = if zero? (x)
                   then 1
                   else begin
                         set x = -(x, 1);
                         (odd)
                        end
                odd()
                 = if zero? (x)
                   then 0
                   else begin
                         set x = -(x, 1);
                         (even)
                        end
      in begin set x = 13; (odd) end")
 1)
(equal?
 (run "let x = 12 in
         list(x, -(x,1), -(x,3))")
 '(12 11 9))
(equal?
 (run "let x = 12 in
         list(x,
              begin
               set x = -(x,1);
               x
              end,
              begin
               set x = -(x,2);
               x
              end)")
 '(12 11 9))
(eqv?
 (run "let x = 22
        in let f = proc (z)
                    let zz = -(z,x)
                    in zz
           in -((f 66), (f 55))")
 11)
(eqv?
 (run "let f = proc (x)
                proc (y)
                  begin
                  set x = -(x,-1);
                  -(x,y)
                end
      in ((f 44) 33)")
 12)
(eqv?
 (run "letrec times4(x) = if zero?(x) then 0
                         else -((times4 -(x,1)), -4)
      in (times4 3)")
 12)
(eqv?
 (run "let times4 = 0
      in begin
           set times4 = proc (x)
                           if zero?(x) then 0
                           else -((times4 -(x,1)), -4);
           (times4 3)
         end")
 12)

;; tests from exercise 3.16
(eqv?
 (run "let x = 7 y = 2 in
         let y = let x = -(x, 1) in -(x, y)
           in -(-(x,8), y)")
 -5)
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
(eqv?
 (run "let x = 30
         in let x = -(x,1) y = -(x,2)
           in -(x,y)")
 1)

;; tests from exercise 3.35
(eqv?
 (run "letrec double (x) = if zero?(x) then 0
                           else -((double -(x,1)),-2)
       in (double 6)")
 12)
(eqv?
 (run "let a = 3
       in let p = proc (x) -(x,a)
              a = 5
          in -(a, (p 2))")
 6)
(eqv?
 (run "letrec f (x, y) = if zero?(x) then y
                         else
                            if zero?(y) then x
                            else -((f -(x,1) -(y,1)), -2)
       in (f 4 12)")
 16)
(eqv?
 (run "letrec
         even(x) = if zero?(x) then 1 else (odd -(x,1))
         odd(x)  = if zero?(x) then 0 else (even -(x,1))
       in (odd 13)")
 1)
;; scoping
(eqv?
 (run "let in
       let x = 1
           y = 2
       in letrec f() = -(x, -(0, y))
          in (f)")
 3)
(eqv?
 (run "let x = 3 in
       letrec
         even(x) = if zero?(x) then 1 else (odd -(x,1))
         odd(x)  = if zero?(x) then 0 else (even -(x,1))
       in let y = x in (even y)")
 0)

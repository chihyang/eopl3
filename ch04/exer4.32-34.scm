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
  (lambda (p-names p-vars p-bodies env)
    (let ((dup-name (check-duplicates p-names)))
      (if (null? dup-name)
          (list 'extend-env-rec (list p-names p-vars p-bodies) env)
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
               (newref (proc-val (procedure saved-p-vars saved-p-body env #f)))))))
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
   (saved-env environment?)
   (call-by-value? boolean?)))
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
           (procedure
            (vars body saved-env call-by-value)
            (value-of body (extend-env* vars vals saved-env))))))
;; mutpair? : SchemeVal -> Bool
(define mutpair?
  (lambda (v)
    (reference? v)))
;; make-pair : ExpVal x ExpVal -> MutPair
(define make-pair
  (lambda (val1 val2)
    (let ((ref1 (newref val1)))
      (let ((ref2 (newref val2)))
        ref1))))
;; left : MutPair -> ExpVal
(define left
  (lambda (p)
    (deref p)))
;; right : MutPair -> ExpVal
(define right
  (lambda (p)
    (deref (+ 1 p))))
;; setleft : MutPair x ExpVal -> Unspecified
(define setleft
  (lambda (p val)
    (setref! p val)))
;; setright : MutPair x ExpVal -> Unspecified
(define setright
  (lambda (p val)
    (setref! (+ 1 p) val)))
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
  (mutpair-val
   (val mutpair?)))
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
                    (pair-val (val3 val4) (expval->pair val1))
                    (mutpair-val (val) val))
             (cases exp-val val2
                    (num-val (num) num)
                    (bool-val (bool) bool)
                    (null-val () '())
                    (proc-val (proc) proc)
                    (pair-val (val3 val4) (expval->pair val2))
                    (mutpair-val (val) val))))
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
(define expval->mutpair
  (lambda (value)
    (cases exp-val value
           (mutpair-val
            (val)
            val)
           (else
            (report-invalid-exp-value 'mutpair)))))
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
;;; Expression ::= letref {Identifier = Expression}* in Expression
;;;                letref-exp (var exp1 body)
;;; Expression ::= proc (Identifier*,) Expression
;;;                proc-exp (var body)
;;; Expression ::= value-proc (Identifier*,) Expression
;;;                value-proc-exp (var body)
;;; Expression ::= letrec {Identifier (Identifier*,)}* = Expression in Expression
;;;                letrec-exp (p-name b-var p-exp1 letrec-body)
;;; Expression ::= (Expression Expression*)
;;;                call-exp (rator rand)
;;; Expression ::= begin Expression {; Expression}* end
;;;                begin-exp (exp1 exps)
;;; Expression ::= set Identifier = Expression
;;;                assign-exp (var exp1)
;;; Expression ::= pair(Expression, Expression)
;;;                pair-exp (exp1 exp2)
;;; Expression ::= left(Expression)
;;;                left-exp (exp1)
;;; Expression ::= right(Expression)
;;;                right-exp (exp1)
;;; Expression ::= setleft(Expression, Expression)
;;;                setleft-exp (exp1, exp2)
;;; Expression ::= setright(Expression, Expression)
;;;                setright-exp (exp1, exp2)
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
    (expression ("letref" (arbno identifier "=" expression) "in" expression)
                letref-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
                letrec-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression)
                proc-exp)
    (expression ("value-proc" "(" (separated-list identifier ",") ")" expression)
                value-proc-exp)
    (expression ("(" expression (arbno expression) ")")
                call-exp)
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)
    (expression ("set" identifier "=" expression)
                assign-exp)
    (expression ("pair" "(" expression "," expression ")")
                newpair-exp)
    (expression ("left" "(" expression ")")
                left-exp)
    (expression ("right" "(" expression ")")
                right-exp)
    (expression ("setleft" "(" expression "," expression ")")
                setleft-exp)
    (expression ("setright" "(" expression "," expression ")")
                setright-exp)))

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
           (letref-exp
            (vars exps body)
            (let ((vals (value-of-operands exps env #f)))
              (value-of body (extend-env* vars vals env))))
           (letrec-exp
            (p-names p-vars p-bodies letrec-body)
            (value-of letrec-body (extend-env-rec p-names p-vars p-bodies env)))
           (proc-exp
            (vars body)
            (proc-val (procedure vars body env #f)))
           (value-proc-exp
            (vars body)
            (proc-val (procedure vars body env #t)))
           (call-exp
            (rator rand)
            (let ((proc1 (expval->proc (value-of rator env))))
              (let ((args
                     (cases proc proc1
                            (procedure
                             (vars body saved-env call-by-value?)
                             (value-of-operands rand env call-by-value?)))))
                (apply-procedure proc1 args))))
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
              (num-val 27)))
           (newpair-exp
            (exp1 exp2)
            (mutpair-val
             (make-pair (value-of exp1 env)
                        (value-of exp2 env))))
           (left-exp
            (exp1)
            (left (expval->mutpair (value-of exp1 env))))
           (right-exp
            (exp1)
            (right (expval->mutpair (value-of exp1 env))))
           (setleft-exp
            (exp1 exp2)
            (begin
              (setleft (expval->mutpair (value-of exp1 env))
                       (value-of exp2 env))
              (num-val 82)))
           (setright-exp
            (exp1 exp2)
            (begin
              (setright (expval->mutpair (value-of exp1 env))
                        (value-of exp2 env))
              (num-val 83))))))
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
                     (mutpair-val
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
;; value-of-operands : Listof(Exp) x Env x Boolean -> Listof(Ref)
(define value-of-operands
  (lambda (exps env call-by-value?)
    (map (lambda (exp)
           (cases expression exp
                  (var-exp (var)
                           (let ((ref (apply-env env var)))
                             (if call-by-value?
                                 (newref (deref ref))
                                 ref)))
                  (else
                   (newref (value-of exp env)))))
         exps)))

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
;; tests from section 4.5.1
(eqv?
 (run "let glo = pair(11,22)
        in let f = proc (loc)
                     let d1 = setright(loc, left(loc))
                     in let d2 = setleft(glo, 99)
                        in -(left(loc),right(loc))
      in (f glo)")
 88)
(eqv?
 (run
       "let swap = proc (x) proc (y)
               let temp = x
               in begin
                 set x = y;
                 set y = temp
               end
  in let a = 33
    in let b = 44
       in begin
            ((swap a) b);
            -(a,b)
          end")
 11)
(eqv?
 (run "let b = 3
      in let p = proc (x) proc(y)
                   begin
                     set x = 4;
                     y
                   end
         in ((p b) b)")
 4)

;; tests from exercise 4.33
(eqv?
 (run "let swap = value-proc (x) proc (y)
               let temp = x
               in begin
                 set x = y;
                 set y = temp
               end
       in let a = 33
          in let b = 44
             in begin
                  ((swap a) b);
                  -(a,b)
                end")
 0)
(eqv?
 (run "let swap = proc (x) value-proc (y)
               let temp = x
               in begin
                 set x = y;
                 set y = temp
               end
       in let a = 33
          in let b = 44
             in begin
                  ((swap a) b);
                  -(a,b)
                end")
 0)
(eqv?
 (run "let swap = value-proc (x) value-proc (y)
               let temp = x
               in begin
                 set x = y;
                 set y = temp
               end
       in let a = 33
          in let b = 44
             in begin
                  ((swap a) b);
                  -(a,b)
                end")
 -11)
(eqv?
 (run "let b = 3
      in let p = value-proc (x) value-proc(y)
                   begin
                     set x = 4;
                     y
                   end
         in ((p b) b)")
 3)

;; test from exercise 4.34
(eqv?
 (run "let b = 3
       in letref a = b
          in let p = proc (x) proc(y)
                       begin
                         set x = 4;
                         y
                       end
             in begin
                  ((p a) a);
                  b
                end")
 4)

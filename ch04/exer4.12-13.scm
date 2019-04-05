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
  (lambda (proc1 vals store)
    (cases proc proc1
           (procedure
            (vars body saved-env)
            (value-of body (extend-env* vars vals saved-env) store)))))
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
(define-datatype answer answer?
  (an-answer
   (val exp-val?)
   (store store?)))
;;; ---------------------- Store (from section 4.2) ----------------------
;; store? : SchemeVal -> Bool
(define store?
  (lambda (v) ((list-of exp-val?) v)))
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
;; newref : ExpVal x Store -> Ref
(define newref
  (lambda (val store)
    (let ((next-ref (length store)))
      (set! store (append store (list val)))
      next-ref)))
;; deref : Ref -> ExpVal
(define deref
  (lambda (ref store)
    (list-ref store ref)))
;; setref! : Ref x ExpVal x Sto -> Sto
;; usage : sets store to a state like the original, but with position ref
;; containing val
(define setref!
  (lambda (ref val store)
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
        (setref-inner store ref))))
;; extend-store : Sto x ExpVal -> Sto
(define extend-store
  (lambda (store val)
    (append store (list val))))
;; apply-store : Sto x ExpVal -> ExpVal
(define apply-store
  (lambda (store expval)
    expval))
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
;; value-of : Exp x Env x Sto -> ExpVal
(define value-of
  (lambda (exp env store)
    (cases expression exp
           (const-exp
            (num)
            (an-answer (num-val num) store))
           (var-exp
            (var)
            (an-answer
             (apply-store store (apply-env env var))
             store))
           (emptylist-exp
            ()
            (value-of-emptylist-exp env store))
           (cons-exp
            (exp1 exp2)
            (value-of-cons-exp exp1 exp2 env store))
           (car-exp
            (exp1)
            (value-of-car-exp exp1 env store))
           (cdr-exp
            (exp1)
            (value-of-cdr-exp exp1 env store))
           (null?-exp
            (exp1)
            (value-of-null?-exp exp1 env store))
           (list-exp
            (exp1)
            (value-of-list-exp exp1 env store))
           (diff-exp
            (exp1 exp2)
            (let ((val1 (value-of exp1 env store)))
              (cases answer val1
                     (an-answer
                      (v1 store1)
                      (let ((val2 (value-of exp2 env store1)))
                        (cases answer val2
                               (an-answer
                                (v2 store2)
                                (an-answer (num-val (- (expval->num v1) (expval->num v2)))
                                           store2))))))))
           (zero?-exp
            (exp1)
            (cases answer (value-of exp1 env store)
                   (an-answer
                    (v1 new-store)
                    (an-answer
                     (bool-val (eqv? (expval->num v1) 0))
                     new-store))))
           (if-exp
            (exp1 exp2 exp3)
            (cases answer (value-of exp1 env store)
                   (an-answer
                    (val new-store)
                    (if (expval->bool val)
                        (value-of exp2 env new-store)
                        (value-of exp3 env new-store)))))
           (let-exp
            (var exp1 body)
            (cases answer (value-of exp1 env store)
                   (an-answer
                    (v1 new-store)
                    (value-of body (extend-env var v1 env) new-store))))
           (letrec-exp
            (p-names p-vars p-bodies letrec-body)
            (value-of letrec-body (extend-env-rec p-names p-vars p-bodies env) store))
           (proc-exp
            (vars body)
            (an-answer
             (proc-val (procedure vars body env))
             store))
           (call-exp
            (rator rand)
            (cases answer (value-of rator env store)
                   (an-answer
                    (v1 store1)
                    (let* ((vals (value-of-args rand env store1))
                           (args (car vals))
                           (store2 (cdr vals)))
                      (apply-procedure (expval->proc v1) args store2)))))
           (begin-exp
            (exp1 exps)
                (cases answer (value-of exp1 env store)
                       (an-answer
                        (v1 store1)
                        (if (null? exps)
                            (an-answer v1 store1)
                            (value-of (begin-exp (car exps) (cdr exps))
                                      env
                                      store1)))))
           (newref-exp
            (exp1)
            (cases answer (value-of exp1 env store)
                   (an-answer
                    (v1 new-store)
                    (let ((ref (newref v1 new-store)))
                      (an-answer (ref-val ref) (extend-store new-store v1))))))
           (deref-exp
            (exp1)
            (cases answer (value-of exp1 env store)
                   (an-answer (v1 new-store)
                    (let ((ref1 (expval->ref v1)))
                      (an-answer (deref ref1 new-store) new-store)))))
           (setref-exp
            (exp1 exp2)
            (cases answer (value-of exp1 env store)
                   (an-answer
                    (v1 store1)
                    (cases answer (value-of exp2 env store1)
                           (an-answer
                            (v2 store2)
                            (an-answer (num-val 23)
                                       (setref! (expval->ref v1) v2 store2))))))))))
;; value-of-program : Program -> SchemeVal
(define value-of-program
  (lambda (prog)
    (initialize-store!)
    (cases program prog
           (a-program
            (exp)
            (cases answer (value-of exp (empty-env) the-store)
                   (an-answer
                    (val new-store)
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
                            val))))))))
(define value-of-emptylist-exp
  (lambda (env store)
    (an-answer (null-val) store)))
(define value-of-cons-exp
  (lambda (car-val cdr-val env store)
    (cases answer (value-of car-val env store)
           (an-answer
            (v1 store1)
            (cases answer (value-of cdr-val env store1)
                   (an-answer
                    (v2 store2)
                    (an-answer
                     (pair-val v1 v2)
                     store2)))))))
(define value-of-car-exp
  (lambda (exp env store)
    (cases answer (value-of exp env store)
           (an-answer
            (v1 store1)
            (cases exp-val v1
                   (pair-val
                    (val1 val2)
                    (an-answer val1 store1))
                   (else (report-invalid-exp-value 'pair-val)))))))
(define value-of-cdr-exp
  (lambda (exp env store)
    (cases answer (value-of exp env store)
           (an-answer
            (v1 store1)
            (cases exp-val v1
                   (pair-val
                    (val1 val2)
                    (an-answer val2 store1))
                   (else (report-invalid-exp-value 'pair-val)))))))
(define value-of-list-exp
  (lambda (exp env store)
    (let* ((ans (value-of-args exp env store))
           (vals (car ans))
           (store1 (cdr ans)))
      (letrec ((list->pair-val
                (lambda (vals)
                  (if (null? vals)
                      (null-val)
                      (pair-val (car vals)
                                (list->pair-val (cdr vals)))))))
        (an-answer (list->pair-val vals)
                   store1)))))
(define value-of-null?-exp
  (lambda (exp env store)
    (cases answer (value-of exp env store)
           (an-answer
            (v1 store1)
            (cases exp-val v1
                   (null-val
                    ()
                    (an-answer (bool-val #t) store1))
                   (else (an-answer (bool-val #f) store1)))))))
;; value-of-args: Listof(ExpVal) x Env x Sto -> (Listof(Answer) . Sto)
(define value-of-args
  (lambda (exps env store)
    (letrec ((value-of-args-inner
              (lambda (exps env store answers)
                (if (null? exps)
                    (cons answers store)
                    (cases answer (value-of (car exps) env store)
                           (an-answer
                            (v1 new-store)
                            (value-of-args-inner
                             (cdr exps) env new-store (append answers (list v1)))))))))
      (value-of-args-inner exps env store '()))))

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
        let counter = newref(0)
        in proc (dummy)
             begin
               setref(counter, -(deref(counter), -1));
               deref(counter)
             end
       in let a = (g 11)
          in let b = (g 11)
             in -(a,b)")
 -1)
(eqv?
 (run "let g = proc (dummy)
                 let counter = newref(0)
                 in begin
                      setref(counter, -(deref(counter), -1));
                      deref(counter)
                    end
       in let a = (g 11)
          in let b = (g 11)
             in -(a,b)")
 0)
(eqv?
 (run "let x = newref(newref(0))
       in begin
           setref(deref(x), 11);
           deref(deref(x))
          end")
 11)
(eqv?
 (run "newref(3)")
 0)
(eqv?
 (run "deref(newref(3))")
 3)
(eqv?
 (run "setref(newref(3), 23)")
 23)
(eqv?
 (run "let x = newref(0) in begin setref(x, 13); deref(x) end")
 13)
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
(eqv?
 (run "let x = newref(22)
        in let f = proc (z)
                    let zz = newref(-(z,deref(x)))
                    in deref(zz)
           in -((f 66), (f 55))")
 11)
(eqv?
 (run "let m = newref(0)
         in letrec rdiff(x, y) = if zero?(x) then y
                                else (rdiff -(x,1) -(y, 1))
            in (rdiff begin setref(m, 3); deref(m) end
                      begin setref(m, 2); deref(m) end)")
 -1)

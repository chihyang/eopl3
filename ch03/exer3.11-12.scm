#lang eopl
;;; ---------------------- Environment(from section 3.2) ----------------------
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
(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))
(define-datatype exp-val exp-val?
  (num-val
   (val number?))
  (bool-val
   (val boolean?))
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
                    (pair-val (val3 val4) (expval->pair val1)))
             (cases exp-val val2
                    (num-val (num) num)
                    (bool-val (bool) bool)
                    (null-val () '())
                    (pair-val (val3 val4) (expval->pair val2)))))
           (else
            (report-invalid-exp-value 'pair)))))
(define report-invalid-exp-value
  (lambda (type)
    (eopl:error
     'exp-val
     "Not a valid exp value of type ~s" type)))

;;; ---------------------- Syntax for the LET language ----------------------
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
;;; Expression ::= - (Expression , Expression)
;;;                diff-exp (exp1 exp2)
;;; Expression ::= + (Expression , Expression)
;;;                add-exp (exp1 exp2)
;;; Expression ::= * (Expression , Expression)
;;;                mul-exp (exp1 exp2)
;;; Expression ::= / (Expression , Expression)
;;;                quoti-exp (exp1 exp2)
;;; Expression ::= minus (Expression)
;;;                minus-exp (exp1)
;;; Expression ::= zero? (Expression)
;;;                zero?-exp (exp1)
;;; Expression ::= equal? (Expression)
;;;                equal?-exp (exp1)
;;; Expression ::= greater? (Expression)
;;;                greater?-exp (exp1)
;;; Expression ::= less? (Expression)
;;;                less?-exp (exp1)
;;; Expression ::= if Expression then Expression else Expression
;;;                if-exp (exp1 exp2 exp3)
;;; Expression ::= let Identifier = Expression in Expression
;;;                let-exp (var exp1 body)
;;; Expression ::= cond {Expression ==> Expression}∗ end
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
    (expression ("+" "(" expression "," expression ")")
                add-exp)
    (expression ("*" "(" expression "," expression ")")
                mul-exp)
    (expression ("/" "(" expression "," expression ")")
                quot-exp)
    (expression ("minus" "(" expression ")")
                minus-exp)
    (expression ("zero?" "(" expression ")")
                zero?-exp)
    (expression ("equal?" "(" expression "," expression ")")
                equal?-exp)
    (expression ("greater?" "(" expression "," expression ")")
                greater?-exp)
    (expression ("less?" "(" expression "," expression ")")
                less?-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression ("let" identifier "=" expression "in" expression)
                let-exp)
    (expression ("cond" (arbno expression "==>" expression) "end")
                cond-exp)))

;;; ---------------------- Evaluate expression ----------------------
(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp
            (num)
            (value-of-num-exp num env))
           (var-exp
            (var)
            (value-of-var-exp var env))
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
            (value-of-diff-exp exp1 exp2 env))
           (add-exp
            (exp1 exp2)
            (value-of-add-exp exp1 exp2 env))
           (mul-exp
            (exp1 exp2)
            (value-of-mul-exp exp1 exp2 env))
           (quot-exp
            (exp1 exp2)
            (value-of-quot-exp exp1 exp2 env))
           (minus-exp
            (exp1)
            (value-of-minus-exp exp1 env))
           (zero?-exp
            (exp1)
            (value-of-zero?-exp exp1 env))
           (equal?-exp
            (exp1 exp2)
            (value-of-equal?-exp exp1 exp2 env))
           (greater?-exp
            (exp1 exp2)
            (value-of-greater?-exp exp1 exp2 env))
           (less?-exp
            (exp1 exp2)
            (value-of-less?-exp exp1 exp2 env))
           (if-exp
            (exp1 exp2 exp3)
            (value-of-if-exp exp1 exp2 exp3 env))
           (let-exp
            (var exp1 body)
            (value-of-let-exp var exp1 body env))
           (cond-exp
            (cond-list val-list)
            (value-of-cond-exp cond-list val-list env)))))
(define value-of-num-exp
  (lambda (num env)
    (num-val num)))
(define value-of-var-exp
  (lambda (var env)
    (apply-env env var)))
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
(define value-of-null?-exp
  (lambda (exp env)
    (cases exp-val (value-of exp env)
           (null-val
            ()
            (bool-val #t))
           (else (bool-val #f)))))
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
(define value-of-diff-exp
  (lambda (exp1 exp2 env)
    (num-val (- (expval->num (value-of exp1 env))
                (expval->num (value-of exp2 env))))))
(define value-of-add-exp
  (lambda (exp1 exp2 env)
    (num-val (+ (expval->num (value-of exp1 env))
                (expval->num (value-of exp2 env))))))
(define value-of-mul-exp
  (lambda (exp1 exp2 env)
    (num-val (* (expval->num (value-of exp1 env))
                (expval->num (value-of exp2 env))))))
(define value-of-quot-exp
  (lambda (exp1 exp2 env)
    (num-val (/ (expval->num (value-of exp1 env))
                (expval->num (value-of exp2 env))))))
(define value-of-minus-exp
  (lambda (exp env)
    (num-val (- (expval->num (value-of exp env))))))
(define value-of-zero?-exp
  (lambda (exp env)
    (bool-val (eqv? (expval->num (value-of exp env)) 0))))
(define value-of-equal?-exp
  (lambda (exp1 exp2 env)
    (bool-val (eqv? (expval->num (value-of exp1 env))
                    (expval->num (value-of exp2 env))))))
(define value-of-greater?-exp
  (lambda (exp1 exp2 env)
    (bool-val (> (expval->num (value-of exp1 env))
                 (expval->num (value-of exp2 env))))))
(define value-of-less?-exp
  (lambda (exp1 exp2 env)
    (bool-val (< (expval->num (value-of exp1 env))
                 (expval->num (value-of exp2 env))))))
(define value-of-if-exp
  (lambda (exp1 exp2 exp3 env)
    (if (expval->bool (value-of exp1 env))
        (value-of exp2 env)
        (value-of exp3 env))))
(define value-of-let-exp
  (lambda (var exp body env)
    (value-of body (extend-env var (value-of exp env) env))))
(define value-of-cond-exp
  (lambda (cond-list val-list env)
    (if (null? cond-list)
        (report-invalid-cond-exp)
        (if (expval->bool (value-of (car cond-list) env))
            (value-of (car val-list) env)
            (value-of-cond-exp (cdr cond-list) (cdr val-list) env)))))
(define value-of--program
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
                     (null-val
                      ()
                      '())
                     (pair-val
                      (val1 val2)
                      (expval->pair val))))))))
(define report-invalid-cond-exp
  (lambda ()
    (eopl:error
     'exp-val
     "Missing case evaluated to true")))

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
   value-of--program
   (sllgen:make-stream-parser let-scanner-spec let-grammar)))
(define run
  (lambda (exp)
    (value-of--program (scan&parse exp))))

;;; ---------------------- Test ----------------------
(eqv?
 (run "let x = 7 in
         let y = 2 in
           let y = let x = -(x, 1) in -(x, y)
             in -(-(x,8), y)")
 -5)
(eq?
 (run "equal?(1,let x = 1 in -(x,0))")
 #t)
(eq?
 (run "greater?(1,let x = 1 in -(x,0))")
 #f)
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
 (run "cond less?(1, 2) ==> 1 end")
 1)
(eqv?
 (run "let x = 3 in let y = 5 in
         cond
           less?(x, -(x, y)) ==> 1
           equal?(x, -(x, y)) ==> minus(y)
           greater?(x, -(x, y)) ==> /(y,x)
         end")
 5/3)
;;; error
(eqv?
 (run "let x = 3 in let y = 5 in
         cond
           less?(x, -(x, y)) ==> 1
           equal?(x, -(x, y)) ==> minus(y)
         end")
 5/3)

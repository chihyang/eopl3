#lang eopl
;;;; section 3.2
;;; Environment(from section 3.2)
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
;;; Expval
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
     "No a valid exp value of type ~s" type)))
;;; Syntax for the LET language
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
;;; Expression ::= Identifier
;;;                var-exp (var)
;;; Expression ::= let Identifier = Expression in Expression
;;;                let-exp (var exp1 body)
;;; Let program
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
    (expression (number)
                const-exp)
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
    (expression (identifier)
                var-exp)
    (expression ("let" identifier "=" expression "in" expression)
                let-exp)))
;;; Evaluate Expression
(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp
            (num)
            (num-val num))
           (emptylist-exp
            ()
            (null-val))
           (cons-exp
            (exp1 exp2)
            (pair-val (value-of exp1 env)
                      (value-of exp2 env)))
           (car-exp
            (exp1)
            (cases exp-val (value-of exp1 env)
                   (pair-val
                    (val1 val2)
                    val1)
                   (else (report-invalid-exp-value 'pair-val))))
           (cdr-exp
            (exp1)
            (cases exp-val (value-of exp1 env)
                   (pair-val
                    (val1 val2)
                    val2)
                   (else (report-invalid-exp-value 'pair-val))))
           (null?-exp
            (exp1)
            (let ((val (value-of exp1 env)))
              (cases exp-val val
                     (null-val
                      ()
                      (bool-val #t))
                     (else (bool-val #f)))))
           (list-exp
            (exp1)
            (if (null? exp1)
                (null-val)
                (pair-val (value-of (car exp1) env)
                          (value-of (list-exp (cdr exp1)) env))))
           (var-exp
            (var)
            (apply-env env var))
           (diff-exp
            (exp1 exp2)
            (num-val (- (expval->num (value-of exp1 env))
                        (expval->num (value-of exp2 env)))))
           (add-exp
            (exp1 exp2)
            (num-val (+ (expval->num (value-of exp1 env))
                        (expval->num (value-of exp2 env)))))
           (mul-exp
            (exp1 exp2)
            (num-val (* (expval->num (value-of exp1 env))
                        (expval->num (value-of exp2 env)))))
           (quot-exp
            (exp1 exp2)
            (num-val (/ (expval->num (value-of exp1 env))
                        (expval->num (value-of exp2 env)))))
           (minus-exp
            (exp1)
            (num-val (- (expval->num (value-of exp1 env)))))
           (zero?-exp
            (exp1)
            (bool-val (eqv? (expval->num (value-of exp1 env)) 0)))
           (equal?-exp
            (exp1 exp2)
            (bool-val (eqv? (expval->num (value-of exp1 env))
                            (expval->num (value-of exp2 env)))))
           (greater?-exp
            (exp1 exp2)
            (bool-val (> (expval->num (value-of exp1 env))
                         (expval->num (value-of exp2 env)))))
           (less?-exp
            (exp1 exp2)
            (bool-val (< (expval->num (value-of exp1 env))
                         (expval->num (value-of exp2 env)))))
           (if-exp
            (exp1 exp2 exp3)
            (if (expval->bool (value-of exp1 env))
                (value-of exp2 env)
                (value-of exp3 env)))
           (let-exp
            (var exp1 body)
            (value-of body (extend-env var (value-of exp1 env) env))))))
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
;; run : String -> Bool | Int
(define run
  (lambda (string)
    (value-of-program
     (scan&parse string))))

;;; ----- test -----
(eqv?
 (value-of-program
  (scan&parse "let x = 7 in
                 let y = 2 in
                   let y = let x = -(x, 1) in -(x, y)
                     in -(-(x,8), y)"))
 -5)
(eq?
 (value-of-program (scan&parse "equal?(1,let x = 1 in -(x,0))"))
 #t)
(eq?
 (value-of-program (scan&parse "greater?(1,let x = 1 in -(x,0))"))
 #f)
(equal?
 (value-of-program
  (scan&parse "let x = 4 in cons(x, cons(cons(-(x,1), emptylist), emptylist))"))
 '(4 (3)))
(eqv?
 #f
 (value-of-program
  (scan&parse "null?(
                cdr(let x = 4 in
                      cons(x,
                           cons(cons(-(x,1), emptylist),
                                emptylist))))")))
(equal?
 (value-of-program
  (scan&parse
   "let x = 4 in list(x, -(x,1), -(x,3))"))
 '(4 3 1))

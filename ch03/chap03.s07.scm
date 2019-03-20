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
     "No a valid exp value of type ~s" type)))

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
;;; Expression ::= %lexref number
;;;                nameless-var-exp (num)
;;; Expression ::= %let Expression in Expression
;;;                nameless-let-exp (exp1 body)
;;; Expression ::= %lexproc Expression
;;;                nameless-proc-exp (body)
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
                call-exp)
    (expression ("%lexref" number)
                nameless-var-exp)
    (expression ("%let" expression "in" expression)
                nameless-let-exp)
    (expression ("%lexproc" expression)
                nameless-proc-exp)))

;;; ------------ Static Environment(from section 3.7) ----------------
;; Senv = Listof(Sym)
;; Lexaddr = N
;; empty-senv : () -> Senv
(define empty-senv
  (lambda () '()))
;; extend-senv : Var x Senv -> Senv
(define extend-senv
  (lambda (var senv)
    (cons var senv)))
;; apply-senv : Senv x Var -> Lexaddr
(define apply-senv
  (lambda (senv var)
    (cond ((null? senv)
           (report-no-binding-found var))
          ((eqv? var (car senv))
           0)
          (else
           (+ 1 (apply-senv (cdr senv) var))))))

;;; --------------------- Translation of Program ---------------------
;; translation-of-program : Program -> Nameless-program
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (a-program
                       (translation-of exp1 (init-senv)))))))
;; translation-of : Exp x Senv -> Nameless-exp
(define translation-of
  (lambda (exp senv)
    (cases expression exp
           (const-exp
            (num)
            (const-exp num))
           (diff-exp
            (exp1 exp2)
            (diff-exp
             (translation-of exp1 senv)
             (translation-of exp2 senv)))
           (zero?-exp
            (exp1)
            (zero?-exp (translation-of exp1 senv)))
           (if-exp
            (exp1 exp2 exp3)
            (if-exp (translation-of exp1 senv)
                    (translation-of exp2 senv)
                    (translation-of exp3 senv)))
           (var-exp
            (var)
            (nameless-var-exp (apply-senv senv var)))
           (let-exp
            (var exp1 body)
            (nameless-let-exp
             (translation-of exp1 senv)
             (translation-of body (extend-senv var senv))))
           (proc-exp
            (var body)
            (nameless-proc-exp
             (translation-of body (extend-senv var senv))))
           (call-exp
            (rator rand)
            (call-exp
             (translation-of rator senv)
             (translation-of rand senv)))
           (else
            (report-invalid-source-expression exp)))))
;; translation-of : () -> Senv
(define init-senv
  (lambda ()
    (extend-senv 'i
                 (extend-senv 'v
                              (extend-senv 'x
                                           (empty-senv))))))
(define report-invalid-source-expression
  (lambda (exp)
    (eopl:error
     'expression
     "No a valid source exp ~a" exp)))

;;; ------------- Nameless Environment(from section 3.7) -------------
;; nameless-environment? : SchemeVal -> Bool
(define nameless-environment?
  (lambda (x)
    ((list-of exp-val?) x)))
;; empty-nameless-env : () -> Nameless-env
(define empty-nameless-env
  (lambda ()
    '()))
;; extend-nameless-env : Expval x Nameless-env -> Nameless-env
(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)))
;; apply-nameless-env : Nameless-env x Lexaddr -> DenVal
(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)))

;;; ------------------------ procedure value ------------------------
(define-datatype proc proc?
  (procedure
   (body expression?)
   (saved-nameless-env nameless-environment?)))
;; apply-procedure : Proc x ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
           (procedure
            (body saved-nameless-env)
            (value-of body (extend-nameless-env val saved-nameless-env))))))

;;; ---------------------- Evaluate expression ----------------------
(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
           (const-exp
            (num)
            (num-val num))
           (diff-exp
            (exp1 exp2)
            (num-val (- (expval->num (value-of exp1 nameless-env))
                        (expval->num (value-of exp2 nameless-env)))))
           (zero?-exp
            (exp1)
            (bool-val (eqv? (expval->num (value-of exp1 nameless-env)) 0)))
           (if-exp
            (exp1 exp2 exp3)
            (if (expval->bool (value-of exp1 nameless-env))
                (value-of exp2 nameless-env)
                (value-of exp3 nameless-env)))
           (call-exp
            (rator rand)
            (let ((proc (expval->proc (value-of rator nameless-env)))
                  (arg (value-of rand nameless-env)))
              (apply-procedure proc arg)))
           (nameless-var-exp
            (n)
            (apply-nameless-env nameless-env n))
           (nameless-let-exp
            (exp1 body)
            (let ((val (value-of exp1 nameless-env)))
              (value-of body (extend-nameless-env val nameless-env))))
           (nameless-proc-exp
            (body)
            (proc-val (procedure body nameless-env)))
           (else
            (report-invalid-tranlated-expression exp)))))
(define value-of-program
  (lambda (prog)
    (cases program prog
           (a-program
            (exp)
            (let ((val (value-of exp (empty-nameless-env))))
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
(define report-invalid-tranlated-expression
  (lambda (exp)
    (eopl:error
     'expression
     "No a valid translated exp ~a" exp)))

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
     (translation-of-program
      (scan&parse string)))))

;;; ---------------------- Test ----------------------
(run "let x = 37
      in proc (y)
           let z = -(y, x)
           in -(x, y)")
(eqv?
 (run "let x = 200 in
        let f = proc (z) -(z,x) in
          let x = 100 in
            let g = proc (z) -(z,x) in
              -((f 1), (g 1))")
 -100)

#lang eopl
;;; ---------------------- Syntax for the CPS-IN language ----------------------
;;; Program     ::= Expression
;;;                 a-program (exp1)
;;; Expression  ::= Number
;;;                 const-exp (num)
;;; Expression  ::= Identifier
;;;                 var-exp (var)
;;; Expression  ::= emptylist
;;;                 emptylist-exp ()
;;; Expression  ::= cons(Expression , Expression)
;;;                 cons-exp (exp1 exp2)
;;; Expression  ::= car (Expression)
;;;                 car-exp (exp1)
;;; Expression  ::= cdr (Expression)
;;;                 cdr-exp (exp1)
;;; Expression  ::= list ({Expression}{,}*)
;;;                 list-exp (exps)
;;; Expression  ::= null? (Expression)
;;;                 null?-exp (exp1)
;;; Expression  ::= add1 (Expression)
;;;                 add1-exp (exp1)
;;; Expression  ::= - (Expression, Expression)
;;;                 diff-exp (exp1, exp2)
;;; Expression  ::= * (Expression, Expression)
;;;                 mul-exp (exp1, exp2)
;;; Expression  ::= greater? (Expression, Expression)
;;;                 greater?-exp (exp1, exp2)
;;; Expression  ::= less? (Expression, Expression)
;;;                 less?-exp (exp1, exp2)
;;; Expression  ::= equal? (Expression, Expression)
;;;                 equal?-exp (exp1, exp2)
;;; Expression  ::= zero? (Expression)
;;;                 zero?-exp (exp1)
;;; Expression  ::= number? (Expression)
;;;                 number?-exp (exp1)
;;; Expression  ::= Expression
;;;                simple-exp->exp (simple-exp1)
;;; Expression  ::= if Expression then Expression else Expression
;;;                 if-exp (exp1 exp2 exp3)
;;; Expression  ::= let {Identifier = Expression}* in Expression
;;;                 let-exp (vars exps body)
;;; Expression  ::= letrec {Identifier (Identifier*,) = Expression}* in Expression
;;;                 letrec-exp (p-names b-vars p-bodies body)
;;; Expression  ::= (Expression Expression*)
;;;                 call-exp (rator rands)
;;; Parse Expression
(define let-scanner-spec
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    (number ((or (concat digit (arbno digit))
                 (concat "-" digit (arbno digit))
                 (concat (arbno digit) "." digit (arbno digit))
                 (concat "-" (arbno digit) "." digit (arbno digit))
                 (concat digit (arbno digit) "." (arbno digit))
                 (concat "-" digit (arbno digit) "." (arbno digit)))) number)
    (comment ("%" (arbno (not #\newline))) skip)))
(define let-grammar
  '((program (expression) a-program)
    (expression (number)
                const-exp)
    (expression (identifier)
                var-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression)
                proc-exp)
    (expression ("emptylist")
                emptylist-exp)
    (expression ("cons" "(" expression "," expression ")")
                cons-exp)
    (expression ("car" "(" expression ")")
                car-exp)
    (expression ("cdr" "(" expression ")")
                cdr-exp)
    (expression ("list" "(" (separated-list expression ",") ")")
                list-exp)
    (expression ("null?" "(" expression ")")
                null?-exp)
    (expression ("add1" "(" expression ")")
                add1-exp)
    (expression ("-" "(" expression "," expression ")")
                diff-exp)
    (expression ("*" "(" expression "," expression ")")
                mul-exp)
    (expression ("zero?" "(" expression ")")
                zero?-exp)
    (expression ("equal?" "(" expression "," expression ")")
                equal?-exp)
    (expression ("greater?" "(" expression "," expression ")")
                greater?-exp)
    (expression ("less?" "(" expression "," expression ")")
                less?-exp)
    (expression ("number?" "(" expression ")")
                number?-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
                letrec-exp)
    (expression ("(" expression (arbno expression) ")")
                call-exp)))

;;; ---------------------- Evaluate OUT expression ----------------------
;;; tail-form-operand? : Exp -> Bool
(define tail-form-operand?
  (lambda (exp)
    (cases expression exp
           (const-exp
            (num)
            #t)
           (var-exp
            (var)
            #t)
           (proc-exp
            (vars body)
            #t)
           (emptylist-exp
            ()
            #t)
           (car-exp
            (exp1)
            (tail-form-operand? exp1))
           (cdr-exp
            (exp1)
            (tail-form-operand? exp1))
           (cons-exp
            (exp1 exp2)
            (and (tail-form-operand? exp1)
                 (tail-form-operand? exp2)))
           (list-exp
            (exps)
            ((list-of tail-form-operand?) exps))
           (null?-exp
            (exp1)
            (tail-form-operand? exp1))
           (add1-exp
            (exp1)
            (tail-form-operand? exp1))
           (diff-exp
            (exp1 exp2)
            (and (tail-form-operand? exp1)
                 (tail-form-operand? exp2)))
           (mul-exp
            (exp1 exp2)
            (and (tail-form-operand? exp1)
                 (tail-form-operand? exp2)))
           (equal?-exp
            (exp1 exp2)
            (and (tail-form-operand? exp1)
                 (tail-form-operand? exp2)))
           (greater?-exp
            (exp1 exp2)
            (and (tail-form-operand? exp1)
                 (tail-form-operand? exp2)))
           (less?-exp
            (exp1 exp2)
            (and (tail-form-operand? exp1)
                 (tail-form-operand? exp2)))
           (zero?-exp
            (exp1)
            (tail-form-operand? exp1))
           (number?-exp
            (exp1)
            (tail-form-operand? exp1))
           (else #f))))
(define tail-form-exp?
  (lambda (exp)
    (cases expression exp
           (if-exp
            (exp1 exp2 exp3)
            (and (tail-form-operand? exp1)
                 (tail-form-exp? exp2)
                 (tail-form-exp? exp3)))
           (let-exp
            (vars exps body)
            (and ((list-of tail-form-operand?) exps)
                 (tail-form-exp? body)))
           (letrec-exp
            (p-names p-vars p-bodies letrec-body)
            ((list-of tail-form-exp?) (cons letrec-body p-bodies)))
           (call-exp
            (rator rands)
            ((list-of tail-form-operand?) (cons rator rands)))
           (else
            (tail-form-operand? exp)))))
;;; tail-form? : Program -> Bool
(define tail-form?
  (lambda (prog)
    (cases program prog
           (a-program
            (exp)
            (tail-form-exp? exp)))))

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
   tail-form?
   (sllgen:make-stream-parser let-scanner-spec let-grammar)))
(define run
  (lambda (exp)
    (tail-form? (scan&parse exp))))

(provide run)

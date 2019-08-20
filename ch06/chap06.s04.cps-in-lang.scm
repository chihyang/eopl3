#lang eopl
(provide (all-defined-out))
;;; ---------------------- Syntax for the CPS-IN language ----------------------
;;; Program     ::= Expression
;;;                 a-program (exp1)
;;; Expression  ::= Number
;;;                 const-exp (num)
;;; Expression  ::= Identifier
;;;                 var-exp (var)
;;; Expression  ::= + (Expression{,}*)
;;;                 sum-exp (exps)
;;; Expression  ::= - (Expression, Expression)
;;;                 diff-exp (exp1, exp2)
;;; Expression  ::= zero? (Expression)
;;;                 zero?-exp (exp1)
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
;;; Expression  ::= print(Expression)
;;;                 print-exp (exp)
;;; Expression  ::= newref (InpExp)
;;;                 newref-exp (exp1)
;;; Expression  ::= deref (InpExp)
;;;                 deref-exp (exp1)
;;; Expression  ::= setref (InpExp, InpExp)
;;;                 setref-exp (exp1 exp2)
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
    (expression ("+" "(" (separated-list expression ",") ")")
                sum-exp)
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
    (expression ("(" expression (arbno expression) ")")
                call-exp)
    (expression ("print" "(" expression ")")
                print-exp)
    (expression ("newref" "(" expression ")")
                newref-exp)
    (expression ("deref" "(" expression ")")
                deref-exp)
    (expression ("setref" "(" expression "," expression ")")
                setref-exp)))

(sllgen:make-define-datatypes let-scanner-spec let-grammar)
(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes let-scanner-spec let-grammar)))
(define just-scan
  (sllgen:make-string-scanner let-scanner-spec let-grammar))
(define scan&parse
  (sllgen:make-string-parser let-scanner-spec let-grammar))

(define unparse-exp
  (lambda (exp)
    (cases expression exp
           (const-exp (num) num)
           (var-exp (var) var)
           (proc-exp
            (vars body)
            `(lambda ,vars ,(unparse-exp body)))
           (sum-exp
            (exps)
            (append '(+) (map unparse-exp exps)))
           (diff-exp
            (exp1 exp2)
            `(- ,(unparse-exp exp1) ,(unparse-exp exp2)))
           (zero?-exp
            (exp1)
            `(zero? ,(unparse-exp exp1)))
           (if-exp
            (exp1 exp2 exp3)
            `(if ,(unparse-exp exp1) ,(unparse-exp exp2) ,(unparse-exp exp3)))
           (let-exp
            (vars exps body)
            `(let ,(map (lambda (v1 v2) (list v1 (unparse-exp v2)))
                        vars exps)
               ,(unparse-exp body)))
           (letrec-exp
            (p-names p-vars p-bodies body)
            `(letrec ,(map (lambda (v1 v2 v3)
                             (list v1
                                   `(lambda ,v2
                                      ,(unparse-exp v3))))
                        p-names p-vars p-bodies)
               ,(unparse-exp body)))
           (call-exp
            (rator rands)
            (map unparse-exp (cons rator rands)))
           (print-exp
            (exp1)
            `(print (unparse-exp exp1)))
           (newref-exp
            (exp1)
            `(newref (unparse-exp exp1)))
           (deref-exp
            (exp1)
            `(deref (unparse-exp exp1)))
           (setref-exp
            (exp1 exp2)
            `(setref (unparse-exp exp1) (unparse-exp exp2))))))

(define unparse-prgm
  (lambda (prgm)
    (cases program prgm
           (a-program
            (exp)
            (unparse-exp exp)))))

#lang eopl
(provide (all-defined-out))
;;; ---------------------- Syntax for the CPS-OUT language ----------------------
;;; TfProgram  ::= TfExp
;;;                cps-a-program (exp1)
;;; SimpleExp  ::= Number
;;;                cps-const-exp (num)
;;; SimpleExp  ::= Identifier
;;;                cps-var-exp (var)
;;; SimpleExp  ::= + (SimpleExp{,}*)
;;;                cps-sum-exp (exps)
;;; SimpleExp  ::= - (SimpleExp, SimpleExp)
;;;                cps-diff-exp (exp1, exp2)
;;; SimpleExp  ::= zero? (SimpleExp)
;;;                cps-zero?-exp (exp1)
;;; TfExp      ::= SimpleExp
;;;                simple-exp->exp (simple-exp1)
;;; TfExp      ::= if SimpleExp then TfExp else TfExp
;;;                cps-if-exp (exp1 exp2 exp3)
;;; TfExp      ::= let {Identifier = SimpleExp}* in TfExp
;;;                cps-let-exp (vars exps body)
;;; TfExp      ::= (SimpleExp SimpleExp*)
;;;                cps-call-exp (rator rands)
;;; TfExp      ::= printk (SimpleExp TfExp)
;;;                cps-printk-exp (rator body)
;;; TfExp      ::= newrefk (SimpleExp SimpleExp)
;;;                cps-newrefk-exp (exp1 exp2)
;;; TfExp      ::= derefk (SimpleExp SimpleExp)
;;;                cps-derefk-exp (exp1 exp2)
;;; Parse Expression
(define cps-scanner-spec
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    (number ((or (concat digit (arbno digit))
                 (concat "-" digit (arbno digit))
                 (concat (arbno digit) "." digit (arbno digit))
                 (concat "-" (arbno digit) "." digit (arbno digit))
                 (concat digit (arbno digit) "." (arbno digit))
                 (concat "-" digit (arbno digit) "." (arbno digit)))) number)
    (comment ("%" (arbno (not #\newline))) skip)))
(define cps-grammar
  '((cps-program (tf-exp) cps-a-program)
    (simple-exp (number)
                cps-const-exp)
    (simple-exp (identifier)
                cps-var-exp)
    (simple-exp ("proc" "(" (separated-list identifier ",") ")" tf-exp)
                cps-proc-exp)
    (simple-exp ("+" "(" (separated-list simple-exp ",") ")")
                cps-sum-exp)
    (simple-exp ("-" "(" simple-exp "," simple-exp ")")
                cps-diff-exp)
    (simple-exp ("zero?" "(" simple-exp ")")
                cps-zero?-exp)
    (tf-exp (simple-exp)
            simple-exp->exp)
    (tf-exp ("if" simple-exp "then" tf-exp "else" tf-exp)
            cps-if-exp)
    (tf-exp ("let" (arbno identifier "=" simple-exp) "in" tf-exp)
            cps-let-exp)
    (tf-exp ("(" simple-exp (arbno simple-exp) ")")
            cps-call-exp)
    (tf-exp ("printk" "(" simple-exp ")" ";" tf-exp)
            cps-printk-exp)
    (tf-exp ("newrefk" "(" simple-exp "," simple-exp ")")
            cps-newrefk-exp)
    (tf-exp ("derefk" "(" simple-exp "," simple-exp ")")
            cps-derefk-exp)
    (tf-exp ("setrefk" "(" simple-exp "," simple-exp ")" ";" tf-exp)
            cps-setrefk-exp)))

(sllgen:make-define-datatypes cps-scanner-spec cps-grammar)
(define cps-list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes cps-scanner-spec cps-grammar)))
(define cps-just-scan
  (sllgen:make-string-scanner cps-scanner-spec cps-grammar))
(define cps-scan&parse
  (sllgen:make-string-parser cps-scanner-spec cps-grammar))

(define cps-unparse-simple-exp
  (lambda (exp)
    (cases simple-exp exp
           (cps-const-exp (num) num)
           (cps-var-exp (var) var)
           (cps-proc-exp
            (vars body)
            `(lambda ,vars ,(cps-unparse-exp body)))
           (cps-sum-exp
            (exps)
            (append '(+) (map cps-unparse-simple-exp exps)))
           (cps-diff-exp
            (exp1 exp2)
            `(- ,(cps-unparse-simple-exp exp1) ,(cps-unparse-simple-exp exp2)))
           (cps-zero?-exp
            (exp1)
            `(zero? ,(cps-unparse-simple-exp exp1))))))

(define cps-unparse-exp
  (lambda (exp)
    (cases tf-exp exp
           (simple-exp->exp (simple) (cps-unparse-simple-exp simple))
           (cps-if-exp
            (exp1 exp2 exp3)
            `(if ,(cps-unparse-simple-exp exp1)
                 ,(cps-unparse-exp exp2)
                 ,(cps-unparse-exp exp3)))
           (cps-let-exp
            (vars exps body)
            `(let ,(map (lambda (v1 v2) (list v1 (cps-unparse-simple-exp v2)))
                        vars exps)
               ,(cps-unparse-exp body)))
           (cps-call-exp
            (rator rands)
            (map cps-unparse-simple-exp (cons rator rands)))
           (cps-printk-exp
            (simple-exp1 body)
            `((print ,(cps-unparse-simple-exp simple-exp1))
              ,(cps-unparse-exp body)))
           (cps-newrefk-exp
            (simple-exp1 simple-exp2)
            `(newrefk ,(cps-unparse-simple-exp simple-exp1)
                      ,(cps-unparse-simple-exp simple-exp2)))
           (cps-derefk-exp
            (simple-exp1 simple-exp2)
            `(derefk ,(cps-unparse-simple-exp simple-exp1)
                     ,(cps-unparse-simple-exp simple-exp2)))
           (cps-setrefk-exp
            (simple1 simple2 body)
            `((setref ,(cps-unparse-simple-exp simple1)
                      ,(cps-unparse-simple-exp simple2))
              ,(cps-unparse-exp body))))))

(define cps-unparse-prgm
  (lambda (prgm)
    (cases cps-program prgm
           (cps-a-program
            (exp)
            (cps-unparse-exp exp)))))

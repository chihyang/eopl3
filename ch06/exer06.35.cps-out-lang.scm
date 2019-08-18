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
;;; TfExp      ::= letrec {Identifier (Identifier*,) = TfExp}* in TfExp
;;;                cps-letrec-exp (p-names b-vars p-bodies body)
;;; TfExp      ::= (SimpleExp SimpleExp*)
;;;                cps-call-exp (rator rands)
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
  '((cps-program (cps-tf-exp) cps-a-program)
    (cps-simple-exp (number)
                    cps-const-exp)
    (cps-simple-exp (identifier)
                    cps-var-exp)
    (cps-simple-exp ("proc" "(" (separated-list identifier ",") ")" cps-tf-exp)
                    cps-proc-exp)
    (cps-simple-exp ("+" "(" (separated-list cps-simple-exp ",") ")")
                    cps-sum-exp)
    (cps-simple-exp ("-" "(" cps-simple-exp "," cps-simple-exp ")")
                    cps-diff-exp)
    (cps-simple-exp ("zero?" "(" cps-simple-exp ")")
                    cps-zero?-exp)
    (cps-simple-exp ("less?" "(" cps-simple-exp "," cps-simple-exp ")")
                    cps-less?-exp)
    (cps-tf-exp (cps-simple-exp)
                cps-simple-exp->exp)
    (cps-tf-exp ("if" cps-simple-exp "then" cps-tf-exp "else" cps-tf-exp)
                cps-if-exp)
    (cps-tf-exp ("let" (arbno identifier "=" cps-simple-exp) "in" cps-tf-exp)
                cps-let-exp)
    (cps-tf-exp ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" cps-tf-exp) "in" cps-tf-exp)
                cps-letrec-exp)
    (cps-tf-exp ("(" cps-simple-exp (arbno cps-simple-exp) ")")
                cps-call-exp)))

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
    (cases cps-simple-exp exp
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
            `(zero? ,(cps-unparse-simple-exp exp1)))
           (cps-less?-exp
            (exp1 exp2)
            `(< ,(cps-unparse-simple-exp exp1) ,(cps-unparse-simple-exp exp2))))))

(define cps-unparse-exp
  (lambda (exp)
    (cases cps-tf-exp exp
           (cps-simple-exp->exp (simple) (cps-unparse-simple-exp simple))
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
           (cps-letrec-exp
            (p-names p-vars p-bodies body)
            `(letrec ,(map (lambda (v1 v2 v3)
                             (list v1
                                   `(lambda ,v2
                                      ,(cps-unparse-exp v3))))
                           p-names p-vars p-bodies)
               ,(cps-unparse-exp body)))
           (cps-call-exp
            (rator rands)
            (map cps-unparse-simple-exp (cons rator rands))))))

(define cps-unparse-prgm
  (lambda (prgm)
    (cases cps-program prgm
           (cps-a-program
            (exp)
            (cps-unparse-exp exp)))))

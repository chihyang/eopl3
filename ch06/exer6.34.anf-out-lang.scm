#lang eopl
(provide (all-defined-out))
;;; ---------------------- Syntax for the ANF-OUT language ----------------------
;;; TfProgram  ::= TfExp
;;;                anf-a-program (exp1)
;;; SimpleExp  ::= Number
;;;                anf-const-exp (num)
;;; SimpleExp  ::= Identifier
;;;                anf-var-exp (var)
;;; SimpleExp  ::= + (SimpleExp{,}*)
;;;                anf-sum-exp (exps)
;;; SimpleExp  ::= - (SimpleExp, SimpleExp)
;;;                anf-diff-exp (exp1, exp2)
;;; SimpleExp  ::= zero? (SimpleExp)
;;;                anf-zero?-exp (exp1)
;;; SimpleExp  ::= less? (SimpleExp, SimpleExp)
;;;                anf-less?-exp (exp1, exp2)
;;; TfExp      ::= SimpleExp
;;;                simple-exp->exp (simple-exp1)
;;; TfExp      ::= if SimpleExp then TfExp else TfExp
;;;                anf-if-exp (exp1 exp2 exp3)
;;; TfExp      ::= let {Identifier = SimpleExp}* in TfExp
;;;                anf-let-exp (vars exps body)
;;; TfExp      ::= letrec {Identifier (Identifier*,) = TfExp}* in TfExp
;;;                anf-letrec-exp (p-names b-vars p-bodies body)
;;; TfExp      ::= (SimpleExp SimpleExp*)
;;;                anf-call-exp (rator rands)
;;; Parse Expression
(define anf-scanner-spec
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    (number ((or (concat digit (arbno digit))
                 (concat "-" digit (arbno digit))
                 (concat (arbno digit) "." digit (arbno digit))
                 (concat "-" (arbno digit) "." digit (arbno digit))
                 (concat digit (arbno digit) "." (arbno digit))
                 (concat "-" digit (arbno digit) "." (arbno digit)))) number)
    (comment ("%" (arbno (not #\newline))) skip)))
(define anf-grammar
  '((anf-program (tf-exp) anf-a-program)
    (simple-exp (number)
                anf-const-exp)
    (simple-exp (identifier)
                anf-var-exp)
    (simple-exp ("proc" "(" (separated-list identifier ",") ")" tf-exp)
                anf-proc-exp)
    (simple-exp ("+" "(" (separated-list simple-exp ",") ")")
                anf-sum-exp)
    (simple-exp ("-" "(" simple-exp "," simple-exp ")")
                anf-diff-exp)
    (simple-exp ("zero?" "(" simple-exp ")")
                anf-zero?-exp)
    (simple-exp ("less?" "(" simple-exp "," simple-exp ")")
                anf-less?-exp)
    (tf-exp (simple-exp)
            simple-exp->exp)
    (tf-exp ("if" simple-exp "then" tf-exp "else" tf-exp)
            anf-if-exp)
    (tf-exp ("let" (arbno identifier "=" tf-exp) "in" tf-exp)
            anf-let-exp)
    (tf-exp ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" tf-exp) "in" tf-exp)
            anf-letrec-exp)
    (tf-exp ("(" simple-exp (arbno simple-exp) ")")
            anf-call-exp)))

(sllgen:make-define-datatypes anf-scanner-spec anf-grammar)
(define anf-list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes anf-scanner-spec anf-grammar)))
(define anf-just-scan
  (sllgen:make-string-scanner anf-scanner-spec anf-grammar))
(define anf-scan&parse
  (sllgen:make-string-parser anf-scanner-spec anf-grammar))

(define anf-unparse-simple-exp
  (lambda (exp)
    (cases simple-exp exp
           (anf-const-exp (num) num)
           (anf-var-exp (var) var)
           (anf-proc-exp
            (vars body)
            `(lambda ,vars ,(anf-unparse-exp body)))
           (anf-sum-exp
            (exps)
            (append '(+) (map anf-unparse-simple-exp exps)))
           (anf-diff-exp
            (exp1 exp2)
            `(- ,(anf-unparse-simple-exp exp1) ,(anf-unparse-simple-exp exp2)))
           (anf-zero?-exp
            (exp1)
            `(zero? ,(anf-unparse-simple-exp exp1)))
           (anf-less?-exp
            (exp1 exp2)
            `(< ,(anf-unparse-simple-exp exp1) ,(anf-unparse-simple-exp exp2))))))

(define anf-unparse-exp
  (lambda (exp)
    (cases tf-exp exp
           (simple-exp->exp (simple) (anf-unparse-simple-exp simple))
           (anf-if-exp
            (exp1 exp2 exp3)
            `(if ,(anf-unparse-simple-exp exp1)
                 ,(anf-unparse-exp exp2)
                 ,(anf-unparse-exp exp3)))
           (anf-let-exp
            (vars exps body)
            `(let ,(map (lambda (v1 v2) (list v1 (anf-unparse-exp v2)))
                        vars exps)
               ,(anf-unparse-exp body)))
           (anf-letrec-exp
            (p-names p-vars p-bodies body)
            `(letrec ,(map (lambda (v1 v2 v3)
                             (list v1
                                   `(lambda ,v2
                                      ,(anf-unparse-exp v3))))
                        p-names p-vars p-bodies)
               ,(anf-unparse-exp body)))
           (anf-call-exp
            (rator rands)
            (map anf-unparse-simple-exp (cons rator rands))))))

(define anf-unparse-prgm
  (lambda (prgm)
    (cases anf-program prgm
           (anf-a-program
            (exp)
            (anf-unparse-exp exp)))))

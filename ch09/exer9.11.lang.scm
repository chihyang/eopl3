#lang eopl
(provide (all-defined-out))
(define identifier? symbol?)
;;; ---------------------- Syntax for the CLASS language ----------------------
;;; Program    ::= {ClassDecl}* Expression
;;;                a-program (class-decls body)
;;; ClassDecl  ::= class Identifier extends Identifier {field Identifier}* {MethodDecl}*
;;;                a-class-decl (class-name super-name field-names method-decls)
;;; MethodDecl ::= Property method Identifier ({Identifier}*(,)) Expression
;;;                a-method-decl (method-name property vars body)
;;; Property   ::= private
;;;                private-prop ()
;;; Property   ::= protected
;;;                protected-prop ()
;;; Property   ::= public
;;;                public-prop ()
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
;;; Expression ::= +(Expression , Expression)
;;;                sum-exp (exp1 exp2)
;;; Expression ::= zero? (Expression)
;;;                zero?-exp (exp1)
;;; Expression ::= if Expression then Expression else Expression
;;;                if-exp (exp1 exp2 exp3)
;;; Expression ::= let {Identifier = Expression}* in Expression
;;;                let-exp (var exp1 body)
;;; Expression ::= proc (Identifier*,) Expression
;;;                proc-exp (var body)
;;; Expression ::= letrec {Identifier (Identifier*,)}* = Expression in Expression
;;;                letrec-exp (p-name b-var p-exp1 letrec-body)
;;; Expression ::= (Expression Expression*)
;;;                call-exp (rator rand)
;;; Expression ::= begin Expression {; Expression}* end
;;;                begin-exp (exp1 exps)
;;; Expression ::= set Identifier = Expression
;;;                assign-exp (var exp1)
;;; Expression ::= new Identifier ({Expression}*(,))
;;;                new-object-exp (class-name rands)
;;; Expression ::= send Expression Identifier ({Expression}*(,))
;;;                method-call-exp (obj-exp method-name rands)
;;; Expression ::= super Identifier ({Expression}*(,))
;;;                super-call-exp (method-name rands)
;;; Expression ::= self
;;;                self-exp ()
;;; Parse Expression
(define let-scanner-spec
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit "-" "_" "?"))) symbol)
    (number ((or (concat digit (arbno digit))
                 (concat "-" digit (arbno digit))
                 (concat (arbno digit) "." digit (arbno digit))
                 (concat "-" (arbno digit) "." digit (arbno digit))
                 (concat digit (arbno digit) "." (arbno digit))
                 (concat "-" digit (arbno digit) "." (arbno digit)))) number)
    (comment ("%" (arbno (not #\newline))) skip)))
(define let-grammar
  '((program ((arbno class-decl) expression) a-program)
    (class-decl ("class" identifier "extends" identifier
                 (arbno "field" identifier)
                 (arbno method-decl))
                a-class-decl)
    (method-decl ("method" identifier "(" (separated-list identifier ",") ")" expression)
                 default-method-decl)
    (method-decl (property "method" identifier "(" (separated-list identifier ",") ")" expression)
                 a-method-decl)
    (property ("private")
              private-prop)
    (property ("protected")
              protected-prop)
    (property ("public")
              public-prop)
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
                sum-exp)
    (expression ("zero?" "(" expression ")")
                zero?-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
                letrec-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression)
                proc-exp)
    (expression ("(" expression (arbno expression) ")")
                call-exp)
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)
    (expression ("set" identifier "=" expression)
                assign-exp)
    (expression ("new" identifier "(" (separated-list expression ",") ")")
                new-object-exp)
    (expression ("send" expression identifier "(" (separated-list expression ",") ")")
                method-call-exp)
    (expression ("super" identifier "(" (separated-list expression ",") ")")
                super-call-exp)
    (expression ("self")
                self-exp)))

;;; ---------------------- Sllgen operations ----------------------
(sllgen:make-define-datatypes let-scanner-spec let-grammar)
(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes let-scanner-spec let-grammar)))
(define just-scan
  (sllgen:make-string-scanner let-scanner-spec let-grammar))
(define scan&parse
  (sllgen:make-string-parser let-scanner-spec let-grammar))

(require (only-in racket/base with-handlers exn:fail?))

;;; test-scan&parse : String -> Bool | 'error
(define test-scan&parse
  (lambda (str)
    (with-handlers
        [(exn:fail? (lambda (en) 'error))]
      (scan&parse str)
      #t)))

;;; checked-scan&parse : String -> Program | 'error
(define checked-scan&parse
  (lambda (str)
    (with-handlers
        [(exn:fail? (lambda (en) 'error))]
      (scan&parse str))))

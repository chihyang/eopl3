#lang eopl
(provide (all-defined-out))
(define identifier? symbol?)
;;; ---------------------- Syntax for the CLASS language ----------------------
;;; Program    ::= {ClassDecl}* Expression
;;;                a-program (class-decls body)
;;; ClassDecl  ::= class Identifier extends Identifier
;;;                 {implements Identifier}*
;;;                 {field Identifier}*
;;;                 {MethodDecl}*
;;;                a-class-decl (c-name s-name i-names
;;;                              f-types f-names m-decls)
;;; ClassDecl  ::= interface Identifier {AbstractMethodDecl}*
;;;                an-interface-decl (i-name abs-m-decls)
;;; MethodDecl ::= method Type Identifier ({Identifier : Type}*(,)) Expression
;;;                a-method-decl (res-type m-name vars var-types body)
;;; AbstractMethodDecl
;;;            ::= method Type Identifier ({Identifier : Type}*(,))
;;;                an-abstract-method-decl (result-type m-vars m-var-types)
;;; Expression ::= Number
;;;                const-exp (num)
;;; Expression ::= Identifier
;;;                var-exp (var);
;;; Expression ::= emptylist (Type)
;;;                emptylist-exp (ty1)
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
;;; Expression ::= proc ({Identifier : Type}*(,)) Expression
;;;                proc-exp (var body)
;;; Expression ::= letrec {Type Identifier ({Identifier : Type}*,)}* = Expression in Expression
;;;                letrec-exp (p-result-types p-names b-varss b-vars-types p-exp1 letrec-body)
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
;;; Expression ::= cast Expression Expression
;;;                cast-exp (exp c-name)
;;; Expression ::= instanceof Expression Identifier
;;;                instanceof-exp (exp c-name)
;;; Type       ::= void
;;;                void-type ()
;;; Type       ::= Identifier
;;;                class-type (class-name)
;;; Type       ::= listof Type
;;;                list-type (type1)
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
                 (arbno "implements" identifier)
                 (arbno "field" type identifier)
                 (arbno method-decl))
                a-class-decl)
    (class-decl ("interface" identifier (arbno abstract-method-decl))
                an-interface-decl)
    (method-decl ("method" type identifier "(" (separated-list identifier ":" type ",") ")" expression)
                 a-method-decl)
    (abstract-method-decl ("method" type identifier "(" (separated-list identifier ":" type ",") ")")
                          an-abstract-method-decl)
    (expression (number)
                const-exp)
    (expression (identifier)
                var-exp)
    (expression ("emptylist" "(" type ")")
                emptylist-exp)
    (expression ("cons" "(" expression "," expression ")")
                cons-exp)
    (expression ("car" "(" expression ")")
                car-exp)
    (expression ("cdr" "(" expression ")")
                cdr-exp)
    (expression ("null?" "(" expression ")")
                null?-exp)
    (expression ("list" "(" expression (arbno "," expression) ")")
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
    (expression ("letrec" (arbno type identifier "(" (separated-list identifier ":" type ",") ")" "=" expression) "in" expression)
                letrec-exp)
    (expression ("proc" "(" (separated-list identifier ":" type ",") ")" expression)
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
                self-exp)
    (expression ("cast" expression expression)
                cast-exp)
    (expression ("instanceof" expression identifier)
                instanceof-exp)
    (type ("int")
          int-type)
    (type ("bool")
          bool-type)
    (type ("(" type "->" type ")")
          proc-type)
    (type ("void")
          void-type)
    (type (identifier)
          class-type)
    (type ("listof" type)
          list-type)))

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

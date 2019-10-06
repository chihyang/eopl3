#lang eopl
(provide (all-defined-out))
(define identifier? symbol?)
;;; ---------------------- Syntax for the CHECKED language ----------------------
;;; Program    ::= {ModuleDefn}* Expression
;;;                a-program (m-defs body)
;;; ModuleDefn ::= module Identifier interface Iface body ModuleBody
;;;                a-module-definition (m-name expected-iface m-body)
;;; Iface      ::= [ {Decl}* ]
;;;                simple-iface (decls)
;;; Decl       ::= Identifier : Type
;;;                val-decl (var-name ty)
;;; ModuleBody ::= [ {Defn}* ]
;;;                defns-module-body (defns)
;;; Defn       ::= Identifier = Expression
;;;                val-defn (var-name exp)
;;; Expression ::= from Identifier take Identifier
;;;                qualified-var-exp (m-name var-name)
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
;;; Expression ::= proc (Identifier : Type) Expression
;;;                proc-exp (var ty body)
;;; Expression ::= letrec Type (Identifier : Type) = Expression in Expression
;;;                letrec-exp (p-result-type p-name b-var b-var-type p-body letrec-body)
;;; Expression ::= (Expression Expression)
;;;                call-exp (rator rand)
;;; Parse Expression
(define let-scanner-spec
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number ((or (concat digit (arbno digit))
                 (concat "-" digit (arbno digit))
                 (concat (arbno digit) "." digit (arbno digit))
                 (concat "-" (arbno digit) "." digit (arbno digit))
                 (concat digit (arbno digit) "." (arbno digit))
                 (concat "-" digit (arbno digit) "." (arbno digit)))) number)
    (comment ("%" (arbno (not #\newline))) skip)))
(define let-grammar
  '((program ((arbno module-defn) expression) a-program)
    (module-defn ("module" identifier "interface" iface "body" module-body)
                 a-module-definition)
    (iface ("[" (arbno decl) "]")
           simple-iface)
    (decl (identifier ":" type)
          val-decl)
    (module-body ("[" (arbno defn) "]")
                 defns-module-body)
    (defn (identifier "=" expression) val-defn)
    (expression ("from" identifier "take" identifier)
                qualified-var-exp)
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
    (expression ("proc" "(" identifier ":" type ")" expression)
                proc-exp)
    (expression ("letrec" type identifier "(" identifier ":" type ")" "=" expression "in" expression)
                letrec-exp)
    (expression ("(" expression expression ")")
                call-exp)
    (type ("int")
          int-type)
    (type ("bool")
          bool-type)
    (type ("(" type "->" type ")")
          proc-type)))

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
;;; checked-scan&parse : String -> Bool | 'error
(define checked-scan&parse
  (lambda (str)
    (with-handlers
        [(exn:fail? (lambda (en) 'error))]
      (scan&parse str)
      #t)))

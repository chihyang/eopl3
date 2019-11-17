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
;;; Iface      ::= (({Identifier : Iface}*) => Iface)
;;;                proc-iface (m-name iface m-names ifaces iface)
;;; Decl       ::= Identifier : Type
;;;                val-decl (var-name ty)
;;; Decl       ::= opaque Identifier
;;;                opaque-type-decl (t-name)
;;; Decl       ::= transparent Identifier = Type
;;;                transparent-type-decl (t-name ty)
;;; ModuleBody ::= [ {Defn}* ]
;;;                defns-module-body (defns)
;;; ModuleBody ::= module-proc ({Identifier : Iface}*) ModuleBody
;;;                proc-module-body (m-name iface defns)
;;; ModuleBody ::= Identifier
;;;                var-module-body (m-name)
;;; ModuleBody ::= (Identifier {Identifier}*)
;;;                app-module-body (rator rands)
;;; Defn       ::= Identifier = Expression
;;;                val-defn (var-name exp)
;;; Defn       ::= type Identifier = Type
;;;                type-defn (name exp)
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
    (identifier (letter (arbno (or letter digit "-" "?"))) symbol)
    (number ((or (concat digit (arbno digit))
                 (concat "-" digit (arbno digit))
                 (concat (arbno digit) "." digit (arbno digit))
                 (concat "-" (arbno digit) "." digit (arbno digit))
                 (concat digit (arbno digit) "." (arbno digit))
                 (concat "-" digit (arbno digit) "." (arbno digit)))) number)
    (comment ("%" (arbno (not #\newline))) skip)))
(define let-grammar
  '((program ((arbno module-defn) expression) a-program)
    (module-defn ("module" identifier "interface" interface "body" module-body)
                 a-module-definition)
    (interface ("[" (arbno declaration) "]") simple-iface)
    (interface ("(" "(" (arbno identifier ":" interface) ")" "=>" interface ")") proc-iface)
    (declaration (identifier ":" type)
                 val-decl)
    (declaration ("opaque" identifier)
                 opaque-type-decl)
    (declaration ("transparent" identifier "=" type)
                 transparent-type-decl)
    (module-body ("[" (arbno definition) "]")
                 defns-module-body)
    (module-body ("module-proc" "(" (arbno identifier ":" interface) ")" module-body)
                 proc-module-body)
    (module-body (identifier)
                 var-module-body)
    (module-body ("(" identifier (arbno identifier) ")")
                 app-module-body)
    (definition (identifier "=" expression) val-defn)
    (definition ("type" identifier "=" type) type-defn)
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
          proc-type)
    (type (identifier)
          named-type)
    (type ("from" identifier "take" identifier)
          qualified-type)))

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

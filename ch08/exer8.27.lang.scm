#lang eopl
(provide (all-defined-out))
(define identifier? symbol?)
;;; ---------------------- Syntax for the CHECKED language ----------------------
;;; Program    ::= {NamedIface}* {ModuleDefn}* ProgramSpec
;;;                a-program (m-defs p-spec)
;;; NamedInterface ::= interface name = [ {Decl}* ]
;;;                    simple-named-iface (i-name decls)
;;; NamedInterface ::= interface name = ((Identifier : Iface) => Iface)
;;;                    proc-named-iface (i-name m-name iface iface)
;;; ModuleDefn ::= module Identifier interface Iface body ModuleSpec
;;;                a-module-definition (m-name expected-iface m-spec)
;;; Iface      ::= [ {Decl}* ]
;;;                simple-iface (decls)
;;; Iface      ::= ((Identifier : Iface) => Iface)
;;;                proc-iface (m-name iface iface)
;;; Decl       ::= Identifier : Type
;;;                val-decl (var-name ty)
;;; Decl       ::= opaque Identifier
;;;                opaque-type-decl (t-name)
;;; Decl       ::= transparent Identifier = Type
;;;                transparent-type-decl (t-name ty)
;;; ModuleSpec ::= ModuleImports ModuleBody
;;;                import-module-spec (m-spec m-body)
;;; ModuleSpec ::= ModuleBody
;;;                simple-module-spec (m-body)
;;; ModuleBody ::= [ {Defn}* ]
;;;                defns-module-body (defns)
;;; ModuleBody ::= module-proc (Identifier: Iface ) ModuleBody
;;;                proc-module-body (m-name iface defns)
;;; ModuleBody ::= Identifier
;;;                var-module-body (m-name)
;;; ModuleBody ::= (Identifier Identifier)
;;;                app-module-body (rator rand)
;;; Defn       ::= Identifier = Expression
;;;                val-defn (var-name exp)
;;; Defn       ::= type Identifier = Type
;;;                type-defn (name exp)
;;; ModuleImports ::= import var {var}*
;;;                   simple-module-imports (var vars)
;;; ProgramSpec ::= ModuleImports Expression
;;;                 import-program-spec (m-spec exp)
;;; ProgramSpec ::= Expression
;;;                 simple-program-spec (exp)
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
  '((program ((arbno named-interface) (arbno module-defn) program-spec) a-program)
    (named-interface ("interface" identifier "=" interface-defn)
                     a-named-iface)
    (interface-defn ("[" (arbno declaration) "]")
                    simple-iface-defn)
    (interface-defn ("(" "(" identifier ":" interface ")" "=>" interface ")")
                    proc-iface-defn)
    (module-defn ("module" identifier "interface" interface "body" module-spec)
                 a-module-definition)
    (interface ("[" (arbno declaration) "]") simple-iface)
    (interface ("(" "(" identifier ":" interface ")" "=>" interface ")") proc-iface)
    (interface (identifier) var-iface)
    (declaration (identifier ":" type)
                 val-decl)
    (declaration ("opaque" identifier)
                 opaque-type-decl)
    (declaration ("transparent" identifier "=" type)
                 transparent-type-decl)
    (module-spec (module-imports module-body)
                 imports-module-body)
    (module-spec (module-body)
                 simple-module-body)
    (module-body ("[" (arbno definition) "]")
                 defns-module-body)
    (module-body ("module-proc" "(" identifier ":" interface ")" module-body)
                 proc-module-body)
    (module-body (identifier)
                 var-module-body)
    (module-body ("(" identifier identifier ")")
                 app-module-body)
    (definition (identifier "=" expression) val-defn)
    (definition ("type" identifier "=" type) type-defn)
    (module-imports ("import" identifier (arbno "," identifier))
                    simple-module-imports)
    (program-spec (module-imports expression)
                  imports-program-spec)
    (program-spec (expression)
                  simple-program-spec)
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

#lang eopl
(provide (all-defined-out))
(define identifier? symbol?)
;;; ---------------------- Syntax for the CHECKED language ----------------------
(define let-scanner-spec
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit "-"))) symbol)
    (qualified-identifier (letter (arbno (or letter digit "-")) "." (arbno (or letter digit "-"))) symbol)
    (number ((or (concat digit (arbno digit))
                 (concat "-" digit (arbno digit))
                 (concat (arbno digit) "." digit (arbno digit))
                 (concat "-" (arbno digit) "." digit (arbno digit))
                 (concat digit (arbno digit) "." (arbno digit))
                 (concat "-" digit (arbno digit) "." (arbno digit)))) number)
    (comment ("%" (arbno (not #\newline))) skip)))
(define let-grammar
  '((program ((arbno module-defn) program-spec) a-program)
    (module-defn ("module" identifier "interface" interface "body" module-spec)
                 a-module-definition)
    (interface ("[" (arbno declaration) "]") simple-iface)
    (declaration (identifier ":" type)
                 val-decl)
    (module-spec (module-depends module-body)
                 depends-module-body)
    (module-spec (module-body)
                 simple-module-body)
    (module-body ("[" (arbno definition) "]")
                 defns-module-body)
    (module-body ("let" (arbno identifier "=" expression) "in" module-body)
                 let-module-body)
    (module-body ("letrec" (arbno type identifier "(" (arbno identifier ":" type) ")" "=" expression) "in" module-body)
                 letrec-module-body)
    (definition (identifier "=" expression) val-defn)
    (module-depends ("depends-on" (separated-list identifier ","))
                    simple-module-depends)
    (program-spec (module-depends "in" expression)
                  depends-program-spec)
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
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    (expression ("proc" "(" (arbno identifier ":" type) ")" expression)
                proc-exp)
    (expression ("letrec" (arbno type identifier "(" (arbno identifier ":" type) ")" "=" expression) "in" expression)
                letrec-exp)
    (expression ("(" expression (arbno expression) ")")
                call-exp)
    (type ("int")
          int-type)
    (type ("bool")
          bool-type)
    (type ("(" (arbno type) "->" type ")")
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

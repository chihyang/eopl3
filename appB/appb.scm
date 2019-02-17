#lang eopl

(define scanner-spec-a
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))
(define grammar-a1
  '((statement
     ("{" statement ";" statement "}")
     compund-statement)
    (statement
     ("while" expression "do" statement)
     while-statement)
    (statement
     (identifier ":=" expression)
     assign-statement)
    (expression
     (identifier)
     var-exp)
    (expression
     ("(" expression "-" expression ")")
     diff-exp)))
;; (sllgen:make-define-datatypes scanner-spec-a grammar-a1)
;; (define scan&parse1
;;   (sllgen:make-string-parser scanner-spec-a grammar-a1))
;; (scan&parse1 "{ while a do b := c; c := d }")

(define grammar-a2
  '((statement
     ("{" (arbno statement ";") "}")
     compund-statement)
    (statement
     ("while" expression "do" statement)
     while-statement)
    (statement
     (identifier ":=" expression)
     assign-statement)
    (expression
     (identifier)
     var-exp)
    (expression
     ("(" expression "-" expression ")")
     diff-exp)))
;; (sllgen:make-define-datatypes scanner-spec-a grammar-a2)
;; (define scan&parse2
;;   (sllgen:make-string-parser scanner-spec-a grammar-a2))
;; (scan&parse2 "{ while a do b := c; c := d; }")
;; (scan&parse2 "{ x := foo; y := bar; z := uu;}")

(define grammar-a3
  '((expression (identifier) var-exp)
    (expression
     ("let" (arbno identifier "=" expression) "in" expression)
     let-exp)))
;; (sllgen:make-define-datatypes scanner-spec-a grammar-a3)
;; (define scan&parse3
;;   (sllgen:make-string-parser scanner-spec-a grammar-a3))
;; (scan&parse3 "let x = y u = v in z")

(define grammar-a4
  '((statement
     ("{" (separated-list statement ";") "}")
     compund-statement)
    (statement
     ("while" expression "do" statement)
     while-statement)
    (statement
     (identifier ":=" expression)
     assign-statement)
    (expression
     (identifier)
     var-exp)
    (expression
     ("(" expression "-" expression ")")
     diff-exp)))
;; (sllgen:make-define-datatypes scanner-spec-a grammar-a4)
;; (define scan&parse4
;;   (sllgen:make-string-parser scanner-spec-a grammar-a4))
;; (scan&parse4 "{ while a do b := c; c := d }")
;; (scan&parse4 "{ x := foo; y := bar; z := uu }")

(define grammar-a5
  '((statement
     ("{"
      (separated-list
       (separated-list identifier ",")
       ":="
       (separated-list expression ",")
       ";")
      "}")
     compund-statement)
    (expression
     (number)
     lit-exp)
    (expression
     (identifier)
     var-exp)))
(sllgen:make-define-datatypes scanner-spec-a grammar-a5)
(define scan&parse5
  (sllgen:make-string-parser scanner-spec-a grammar-a5))
(scan&parse5 "{ b := c; x, y, z := 1, 2, 3 }")

(define grammar-a6
  '((statement
     ("{"
      (arbno
       (separated-list identifier ",")
       ":="
       (separated-list expression ",")
       ";")
      "}")
     compund-statement)
    (expression
     (number)
     lit-exp)
    (expression
     (identifier)
     var-exp)))
;; (sllgen:make-define-datatypes scanner-spec-a grammar-a6)
;; (define scan&parse6
;;   (sllgen:make-string-parser scanner-spec-a grammar-a6))
;; (scan&parse6 "{ b := c; x, y, z := 1, 2, 3; }")

#lang eopl

(define error eopl:error)
;;; Environment(from section 3.2)
(define empty-env
  (lambda () (list 'empty-env)))
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))
(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env)
       (report-no-binding-found search-var))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
      (else
       (report-invalid-env env)))))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))

(define arith-scanner-spec-a
  '((white-sp
     (whitespace)
     skip)
    (number
     ((concat digit (arbno digit)))
     number)
    (identifier
     (letter (arbno (or letter digit)))
     symbol)))
(define arith-grammar
  '((arith-expr
     ;; here we cannot use separated-list, think about the expression below
     ;; "()", it should be illegal, because "every arithmetic expression is the
     ;; sum of a NON-EMPTY sequence of terms"
     (arith-term (arbno additive-op arith-term))
     arith-additive)
    (arith-term
     (arith-factor (arbno multiplicative-op arith-factor))
     arith-multiplicative)
    (arith-factor
     (number)
     arith-number)
    (arith-factor
     ("-" number)
     arith-minus)
    (arith-factor
     (identifier)
     arith-var)
    (arith-factor
     ("(" arith-expr ")")
     arith-group)
    (multiplicative-op
     ("*")
     mul)
    (multiplicative-op
     ("/")
     div)
    (additive-op
     ("+")
     add)
    (additive-op
     ("-")
     sub)))

(define value-of--exp
  (lambda (exp env)
    (cases arith-expr exp
           (arith-additive
            (term operator-lst term-lst)
            (if (and (null? operator-lst)
                     (null? term-lst))
                (value-of--term term env)
                (value-of--lst (value-of--term term env)
                               operator-lst
                               term-lst
                               env))))))

(define value-of--term
  (lambda (term env)
    (cases arith-term term
           (arith-multiplicative
            (factor operator-lst factor-lst)
            (if (and (null? operator-lst)
                     (null? factor-lst))
                (value-of--factor factor env)
                (value-of--lst (value-of--factor factor env)
                               operator-lst
                               factor-lst
                               env))))))

(define value-of--factor
  (lambda (factor env)
    (cases arith-factor factor
           (arith-number
            (num)
            num)
           (arith-minus
            (num)
            (- num))
           (arith-var
            (var)
            (apply-env env var))
           (arith-group
            (exp)
            (value-of--exp exp env)))))

(define value-of--op
  (lambda (op)
    (cond ((multiplicative-op? op)
           (value-of--multiplicative-op op))
          ((additive-op? op)
           (value-of--additive-op op))
          (else (eopl:error 'value-of--op "Unknown operation ~s" op)))))

(define value-of--multiplicative-op
  (lambda (op)
    (cases multiplicative-op op
           (mul () *)
           (div () /))))

(define value-of--additive-op
  (lambda (op)
    (cases additive-op op
           (add () +)
           (sub () -))))

(define value-of--lst
  (lambda (value operator-lst operand-lst env)
    (cond ((null? operand-lst) value)
          ((arith-term? (car operand-lst))
           (value-of--lst
            ((value-of--op (car operator-lst))
             value
             (value-of--term (car operand-lst) env))
            (cdr operator-lst)
            (cdr operand-lst)
            env))
          ((arith-factor? (car operand-lst))
           (value-of--lst
            ((value-of--op (car operator-lst))
             value
             (value-of--factor (car operand-lst) env))
            (cdr operator-lst)
            (cdr operand-lst)
            env))
          (else (eopl:error value-of--exp "Unknown type of operand ~s"
                       (car operand-lst))))))

(sllgen:make-define-datatypes arith-scanner-spec-a arith-grammar)
(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes arith-scanner-spec-a arith-grammar)))
(define just-scan
  (sllgen:make-string-scanner arith-scanner-spec-a arith-grammar))
(define scan&parse
  (sllgen:make-string-parser arith-scanner-spec-a arith-grammar))

;;; tests
(eq? (value-of--exp (scan&parse "3+2*66-5") (empty-env)) 130)
(eq? (value-of--exp (scan&parse "3+(2*66)-5") (empty-env)) 130)
(eq? (value-of--exp (scan&parse "3+(a*66)-5") (extend-env 'a 2 (empty-env))) 130)

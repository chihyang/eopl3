#lang eopl
;;; Representation 1:
;;; represent lambda as ((λ . Identifier) . lambda-exp)
;;; represent app as (lambda-exp . lambda-exp)
;;; Var -> Lc-exp
(define var-exp
  (lambda (var)
    var))
;;; Var x Lc-exp -> Lc-exp
(define lambda-exp
  (lambda (var lc-exp)
    (cons (cons 'λ var) lc-exp)))
;;; Lc-exp x Lc-exp -> Lc-exp
(define app-exp
  (lambda (lc-exp1 lc-exp2)
    (cons lc-exp1 lc-exp2)))
;;; Lc-exp -> Bool
(define var-exp?
  (lambda (lc-exp)
    (symbol? lc-exp)))
;;; Lc-exp -> Bool
(define lambda-exp?
  (lambda (lc-exp)
    (if (pair? lc-exp)
        (if (pair? (car lc-exp))
            (eqv? (caar lc-exp) 'λ)
            #f)
        #f)))
;;; Lc-exp -> Bool
(define app-exp?
  (lambda (lc-exp)
    (if (pair? lc-exp)
        (if (pair? (car lc-exp))
            (not (eq? (caar lc-exp) 'λ))
            #t)
        #f)))
;;; Lc-exp -> Val
(define var-exp->var
  (lambda (lc-exp)
    lc-exp))
;;; Lc-exp -> Val
(define lambda-exp->bound-var
  (lambda (lc-exp)
    (cdar lc-exp)))
;;; Lc-exp -> Lc-exp
(define lambda-exp->body
  (lambda (lc-exp)
    (cdr lc-exp)))
;;; Lc-exp -> Lc-exp
(define app-exp->rator
  (lambda (lc-exp)
    (car lc-exp)))
;;; Lc-exp -> Lc-exp
(define app-exp->rand
  (lambda (lc-exp)
    (cdr lc-exp)))

;;; Representation 2:
;;; represent lambda as ((λ Identifier) lambda-exp)
;;; represent app as (lambda-exp lambda-exp)
;;; Var -> Lc-exp
(define var-exp
  (lambda (var)
    var))
;;; Var x Lc-exp -> Lc-exp
(define lambda-exp
  (lambda (var lc-exp)
    (list (list 'λ var) lc-exp)))
;;; Lc-exp x Lc-exp -> Lc-exp
(define app-exp
  (lambda (lc-exp1 lc-exp2)
    (list lc-exp1 lc-exp2)))
;;; Lc-exp -> Bool
(define var-exp?
  (lambda (lc-exp)
    (symbol? lc-exp)))
;;; Lc-exp -> Bool
(define lambda-exp?
  (lambda (lc-exp)
    (if (list? lc-exp)
        (if (list? (car lc-exp))
            (eqv? (caar lc-exp) 'λ)
            #f)
        #f)))
;;; Lc-exp -> Bool
(define app-exp?
  (lambda (lc-exp)
    (if (list? lc-exp)
        (if (list? (car lc-exp))
            (not (eqv? (caar lc-exp) 'λ))
            #t)
        #t)))
;;; Lc-exp -> Val
(define var-exp->var
  (lambda (lc-exp)
    lc-exp))
;;; Lc-exp -> Val
(define lambda-exp->bound-var
  (lambda (lc-exp)
    (cadar lc-exp)))
;;; Lc-exp -> Lc-exp
(define lambda-exp->body
  (lambda (lc-exp)
    (cadr lc-exp)))
;;; Lc-exp -> Lc-exp
(define app-exp->rator
  (lambda (lc-exp)
    (car lc-exp)))
;;; Lc-exp -> Lc-exp
(define app-exp->rand
  (lambda (lc-exp)
    (cadr lc-exp)))

;;; Val x Lc-exp -> Bool
(define occur-free?
  (lambda (search-var exp)
    (cond ((var-exp? exp) (eq? search-var (var-exp->var exp)))
          ((lambda-exp? exp)
           (and (not (eq? search-var (lambda-exp->bound-var exp)))
                (occur-free? search-var (lambda-exp->body exp))))
          (else
           (or (occur-free? search-var (app-exp->rator exp))
               (occur-free? search-var (app-exp->rand exp)))))))
;;; ---- test ----
(eq? (occur-free? 'x (var-exp 'y)) #f)
(eq? (occur-free? 'x (var-exp 'x)) #t)
(eq? (occur-free? 'x (lambda-exp 'x (var-exp 'y))) #f)
(eq? (occur-free? 'y (lambda-exp 'x (var-exp 'y))) #t)
(eq? (occur-free? 'z (lambda-exp 'x (var-exp 'y))) #f)
(eq? (occur-free? 'x (app-exp (lambda-exp 'x (var-exp 'y)) (var-exp 'z))) #f)
(eq? (occur-free? 'y (app-exp (lambda-exp 'x (var-exp 'y)) (var-exp 'z))) #t)
(eq? (occur-free? 'z (app-exp (lambda-exp 'x (var-exp 'y)) (var-exp 'z))) #t)

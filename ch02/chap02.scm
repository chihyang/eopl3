#lang eopl
;;; rep 1
(define zero (lambda () '()))
(define is-zero? (lambda (n) (null? n)))
(define successor (lambda (n) (cons #t n)))
(define predecessor (lambda (n) (cdr n)))
;;; rep 2
(define zero (lambda () 0))
(define is-zero? (lambda (n) (zero? n)))
(define successor (lambda (n) (+ n 1)))
(define predecessor (lambda (n) (- n 1)))
;;; env
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
;;; () -> Env
(define empty-env
  (lambda ()
    (lambda (search-var)
      (report-no-binding-found search-var))))
;;; Var x SchemeVal x Env -> Env
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))
;;; Env x Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (env search-var)))
;;; another version of occur-free?
(define occur-free?
  (lambda (search-var exp)
    (cond ((search-var-exp? exp) (eq? search-var (search-var-exp->search-var exp)))
          ((lambda-exp? exp)
           (and (not (eq? search-var (lambda-exp->bound-search-var exp)))
                (occur-free? search-var (lambda-exp->body exp))))
          (else
           (or (occur-free? search-var (app-exp->rator exp))
               (occur-free? search-var (app-exp->rand exp)))))))
;;; tool for defining recursive data types
(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))
;;; occurs-free? : Sym x LcExp -> Bool
(define occurs-free?
  (lambda (search-var exp)
    (cases lc-exp exp
           (var-exp
            (var)
            (eqv? var search-var))
           (lambda-exp
            (bound-var body)
            (and (not (eqv? search-var bound-var))
                 (occurs-free? search-var body)))
           (app-exp
            (rator rand)
            (or (occurs-free? search-var rator)
                (occurs-free? search-var rand))))))
;;; S-list ::= ( {S-exp}* )
(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
   (first s-exp?)
   (rest s-list?)))
;;; S-exp ::= Symbol | S-list
(define-datatype s-exp s-exp?
  (symbol-s-exp
   (sym symbol?))
  (s-list-s-exp
   (slst s-list?)))
;;; another way
(define-datatype s-list s-list?
  (an-s-list
   (sexps (list-of s-exp?))))
(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))
;;; section 2.5
;; parse-expression : SchemeVal -> LcExp
(define parse-expression
  (lambda (datum)
    (cond
     ((symbol? datum) (var-exp datum))
     ((pair? datum)
      (if (eqv? (car datum) 'lambda)
          (lambda-exp
           (car (cadr datum))
           (parse-expression (caddr datum)))
          (app-exp
           (parse-expression (car datum))
           (parse-expression (cadr datum)))))
     (else (report-invalid-concrete-syntax datum)))))
;; unparse-lc-exp : LcExp -> SchemeVal
(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
           (var-exp (var) var)
           (lambda-exp
            (bound-var body)
            (list 'lambda
                  (list bound-var)
                  (unparse-lc-exp body)))
           (app-exp
            (rator rand)
            (list (unparse-lc-exp rator)
                  (unparse-lc-exp rand))))))

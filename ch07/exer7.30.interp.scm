#lang eopl
(require "exer7.30.lang.scm")
(require "exer7.30.env.scm")
(require "exer7.30.store.scm")
(require "exer7.30.infer.scm")
(provide value-of-program value-of run checked-run)

;;; ---------------------- Evaluate expression ----------------------
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
           (procedure
            (var body saved-env)
            (value-of body (extend-env var val saved-env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp
            (num)
            (num-val num))
           (var-exp
            (var)
            (apply-env env var))
           (diff-exp
            (exp1 exp2)
            (num-val (- (expval->num (value-of exp1 env))
                        (expval->num (value-of exp2 env)))))
           (zero?-exp
            (exp1)
            (bool-val (eqv? (expval->num (value-of exp1 env)) 0)))
           (if-exp
            (exp1 exp2 exp3)
            (if (expval->bool (value-of exp1 env))
                (value-of exp2 env)
                (value-of exp3 env)))
           (let-exp
            (var exp1 body)
            (let ((val (value-of exp1 env)))
              (value-of body (extend-env var val env))))
           (letrec-exp
            (p-name p-var p-body letrec-body)
            (value-of letrec-body (extend-env-rec (list p-name) (list p-var) (list p-body) env)))
           (proc-exp
            (vars body)
            (proc-val (procedure vars body env)))
           (call-exp
            (rator rand)
            (let ((proc (expval->proc (value-of rator env)))
                  (arg (value-of rand env)))
              (apply-procedure proc arg)))
           (list-exp
            (exp1 exps)
            (let ((val1 (value-of exp1 env)))
              (if (null? exps)
                  (pair-val val1 (null-val))
                  (pair-val val1
                            (value-of (list-exp (car exps) (cdr exps)) env)))))
           (cons-exp
            (exp1 exp2)
            (let ((val1 (value-of exp1 env))
                  (val2 (value-of exp2 env)))
              (pair-val val1 val2)))
           (car-exp
            (exp1)
            (let ((val1 (value-of exp1 env)))
              (cases exp-val val1
                     (pair-val
                      (first rest)
                      first)
                     (else
                      (report-invalid-exp-value 'list)))))
           (cdr-exp
            (exp1)
            (let ((val1 (value-of exp1 env)))
              (cases exp-val val1
                     (pair-val
                      (first rest)
                      rest)
                     (else
                      (report-invalid-exp-value 'list)))))
           (null-exp
            (exp1)
            (let ((val1 (value-of exp1 env)))
              (cases exp-val val1
                     (null-val
                      ()
                      (bool-val #t))
                     (else
                      (bool-val #f)))))
           (emptylist-exp
            ()
            (null-val))
           (pair-exp
            (exp1 exp2)
            (pair-val (value-of exp1 env)
                      (value-of exp2 env)))
           (newref-exp
            (exp1)
            (ref-val (newref (value-of exp1 env))))
           (deref-exp
            (exp1)
            (deref (expval->ref (value-of exp1 env))))
           (setref-exp
            (exp1 exp2)
            (let ((ref (expval->ref (value-of exp1 env))))
              (setref! ref (value-of exp2 env))
              (num-val 27)))
           (begin-exp
            (exp1 exps)
            (if (null? exps)
                (value-of exp1 env)
                (begin
                  (value-of exp1 env)
                  (value-of (begin-exp (car exps) (cdr exps)) env))))
           )))

;; value-of-program : Program -> SchemeVal
(define value-of-program
  (lambda (prog)
    (initialize-store!)
    (cases program prog
           (a-program
            (exp)
            (let ((val (value-of exp (empty-env))))
              (expval->schemeval val))))))

(define run
  (lambda (prgm)
    (value-of-program prgm)))

;;; checked-run : String -> Int | Bool | Proc | String (for exception)
(require (only-in racket/base with-handlers exn:fail?))
(define checked-run
  (lambda (prgm)
    (with-handlers
        [(exn:fail? (lambda (en) 'error))]
      (run prgm))))

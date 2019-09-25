#lang eopl
(require "exer7.23.lang.scm")
(require "exer7.23.env.scm")
(require "exer7.23.infer.scm")
(provide value-of-program value-of run checked-run)

;;; ---------------------- Evaluate expression ----------------------
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
           (procedure
            (var body saved-env)
            (value-of body (extend-env* var (list val) saved-env))))))

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
            (p-result-type p-name p-var p-arg-type p-body letrec-body)
            (value-of letrec-body (extend-env-rec (list p-name) (list (list p-var)) (list p-body) env)))
           (proc-exp
            (var var-type body)
            (proc-val (procedure (list var) body env)))
           (call-exp
            (rator rand)
            (let ((proc (expval->proc (value-of rator env)))
                  (arg (value-of rand env)))
              (apply-procedure proc arg)))
           (pair-exp
            (exp1 exp2)
            (pair-val (value-of exp1 env) (value-of exp2 env)))
           (unpair-exp
            (var1 var2 exp1 body)
            (let ((pv (value-of exp1 env)))
              (cases exp-val pv
                     (pair-val
                      (val1 val2)
                      (value-of
                       body
                       (extend-env*
                        (list var1 var2)
                        (list val1 val2)
                        env)))
                     (else
                      (report-invalid-exp-value 'pair))))))))

;; value-of-program : Program -> SchemeVal
(define value-of-program
  (lambda (prog)
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

#lang eopl
(require "exer7.11.lang.scm")
(require "exer7.11.store.scm")
(require "exer7.11.env.scm")
(require "exer7.11.checker.scm")
(provide value-of-program value-of run checked-run)

;;; ---------------------- Evaluate expression ----------------------
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
           (procedure
            (vars body saved-env)
            (value-of body (extend-env* vars (map newref vals) saved-env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp
            (num)
            (num-val num))
           (var-exp
            (var)
            (deref (apply-env env var)))
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
            (vars exps body)
            (let ((vals (map (lambda (e) (value-of e env)) exps)))
              (value-of body (extend-env* vars (map newref vals) env))))
           (letrec-exp
            (p-result-types p-names p-vars p-arg-types p-bodies letrec-body)
            (value-of letrec-body (extend-env-rec p-names p-vars p-bodies env)))
           (proc-exp
            (vars var-types body)
            (proc-val (procedure vars body env)))
           (call-exp
            (rator rands)
            (let ((proc (expval->proc (value-of rator env)))
                  (args (map (lambda (e) (value-of e env)) rands)))
              (apply-procedure proc args)))
           (begin-exp
            (exp1 exps)
            (if (null? exps)
                (value-of exp1 env)
                (begin
                  (value-of exp1 env)
                  (value-of (begin-exp (car exps) (cdr exps)) env))))
           (assign-exp
            (var exp1)
            (let ((ref (apply-env env var))
                  (val (value-of exp1 env)))
              (setref! ref val)
              (num-val 27)))
           (newpair-exp
            (exp1 exp2)
            (let ((val1 (value-of exp1 env))
                  (val2 (value-of exp2 env)))
              (mutpair-val (make-pair val1 val2))))
           (left-exp
            (exp1)
            (let ((val1 (value-of exp1 env)))
              (let ((pair (expval->mutpair val1)))
                (left pair))))
           (right-exp
            (exp1)
            (let ((val1 (value-of exp1 env)))
              (let ((pair (expval->mutpair val1)))
                (right pair))))
           (setleft-exp
            (exp1 exp2)
            (let ((val1 (value-of exp1 env))
                  (val2 (value-of exp2 env)))
              (let ((pair (expval->mutpair val1)))
                (setleft pair val2)
                (num-val 82))))
           (setright-exp
            (exp1 exp2)
            (let ((val1 (value-of exp1 env))
                  (val2 (value-of exp2 env)))
              (let ((pair (expval->mutpair val1)))
                (setright pair val2)
                (num-val 83)))))))

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

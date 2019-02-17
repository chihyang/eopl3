#lang eopl
;;; 1. binary tree
;;; env : () | (var val env)
;;; Env = (empty-env) | (extend-env Var Env Val)
;;; Var = Sym
(define empty-env (lambda () '()))
(define extend-env
  (lambda (var val env)
    (list var env val)))
(define apply-env
  (lambda (env search-var)
    (cond
      ((null? env)
       (report-no-binding-found search-var))
      ((eqv? (car env) search-var)
       (caddr env))
      (else
       (apply-env (cadr env) search-var)))))

;;; 2. list of lists
;;; env = () | (var val env)
;;; Env = (empty-env) | (extend-env Var Env Val)
;;; Var = Sym
(define empty-env (lambda () '()))
(define extend-env
  (lambda (var val env)
    (cons var
          (cons val env))))
(define apply-env
  (lambda (env search-var)
    (cond
      ((null? env)
       (report-no-binding-found search-var))
      ((eqv? (car env) search-var)
       (cadr env))
      (else
       (apply-env (cddr env) search-var)))))

;;; 3. env like a tree
;;; env : (main () ()) | (env-name (env) (var val var val ...))
;;; Env = (empty-env) | (extend-env Var Env Val)
;;; Var = Sym
(define empty-env (lambda () '(main () ())))
(define add-to-current-env
  (lambda (var val env)
    (if (is-element? var (caddr env))
        (report-redefined-variable env var)
        (list (car env)
              (cadr env)
              (cons var
                    (cons val
                          (caddr env)))))))
(define is-element?
  (lambda (var var-list)
    (if (null? var-list)
        #f
        (if (eqv? var (car var-list))
            #t
            (is-element? var (cddr var-list))))))
(define report-redefined-variable
  (lambda (env var)
    (eopl:error 'add-to-current-env "Redefined variable ~s in env ~s" var env)))
(define extend-env
  (lambda (var val name env)
    (list name env (list var val))))
(define apply-env
  (lambda (env search-var)
    (search-env (cadr env) (caddr env) search-var)))
(define search-env
  (lambda (sub-env current-env search-var)
    (cond ((null? current-env)
           (if (null? sub-env)
               (report-no-binding-found search-var)
               (apply-env sub-env search-var)))
          ((eqv? (car current-env) search-var)
           (cadr current-env))
          (else
           (foo sub-env (cddr current-env) search-var)))))

;;; tests
(add-to-current-env 'a 1 (empty-env))
(add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env)))
(add-to-current-env 'a 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env))))
(add-to-current-env 'a 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env))))
(add-to-current-env 'a 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env))))
(extend-env 'c 24 'foo (add-to-current-env 'a 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env)))))
(extend-env 'c 24 'foo (add-to-current-env 'c 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env)))))
(extend-env 'c 24 'foo (add-to-current-env 'c 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env)))))
(extend-env 'c 24 'foo (add-to-current-env 'c 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env)))))
(add-to-current-env
 'x
 '()
 (extend-env 'c 24 'foo (add-to-current-env 'c 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env))))))
(apply-env
 (add-to-current-env
  'x '()
  (extend-env 'c 24 'foo (add-to-current-env 'c 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env))))))
 'c)
(apply-env
 (add-to-current-env
  'b '()
  (extend-env 'c 24 'foo (add-to-current-env 'c 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env))))))
 'c)
(apply-env
 (add-to-current-env
  'c '()
  (extend-env 'c 24 'foo (add-to-current-env 'c 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env))))))
 'b)
(apply-env
 (add-to-current-env
  'x '()
  (extend-env 'c 24 'foo (add-to-current-env 'c 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env))))))
 'b)
(apply-env
 (add-to-current-env
  'x '()
  (extend-env 'c 24 'foo (add-to-current-env 'c 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env))))))
 'm)
(apply-env
 (add-to-current-env
  'x '()
  (extend-env 'c 24 'foo (add-to-current-env 'c 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env))))))
 '海外徒闻更九州)
(apply-env
 (add-to-current-env
  'x '()
  (extend-env 'c 24 'foo (add-to-current-env 'c 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env))))))
 'bar)
(apply-env
 (add-to-current-env
  'x '()
  (extend-env 'c 24 'foo (add-to-current-env 'c 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env))))))
 '1)
(apply-env
 (add-to-current-env
  'x '()
  (extend-env 'c 24 'foo (add-to-current-env 'c 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env))))))
 'adsf)
(apply-env
 (add-to-current-env
  'x '()
  (extend-env 'c 24 'foo (add-to-current-env 'c 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env))))))
 'y)
(apply-env
 (add-to-current-env
  'x '()
  (extend-env 'c 24 'foo (add-to-current-env 'c 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env))))))
 'x)
(apply-env
 (add-to-current-env
  'x (quote ())
  (extend-env 'c 24 'foo (add-to-current-env 'c 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env))))))
 'x)
(apply-env
 (add-to-current-env
  'x (quote ())
  (extend-env 'c 24 'foo (add-to-current-env 'c 3 (add-to-current-env 'b 2 (add-to-current-env 'a 1 (empty-env))))))
 'a)

#lang eopl
(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))
(define-datatype lc-exp lc-exp?
  (var-exp
   (var one-var?))
  (lambda-exp
   (bound-vars (list-of one-var?))
   (body lc-exp?))
  (app-exp
   (rand lc-exp?)
   (rator (list-of lc-exp?))))
(define-datatype one-var one-var?
  (one-var-item
   (v symbol?)))
(define parse-expression
  (lambda (datum)
    (cond
     ((null? datum)
      (eopl:error 'parse-expression
                  "Empty lambda expression ~s!" datum))
     ((symbol? datum) (var-exp (one-var-item datum)))
     ((pair? datum)
      (if (eqv? (car datum) 'lambda)
          (cond ((null? (cdr datum))
                 (eopl:error 'parse-expression
                             "Parse error ~s: bound var list and body does not exist!"
                             datum))
                ((null? (cddr datum))
                 (eopl:error 'parse-expression
                             "Parse error ~s: body does not exist!"
                             datum))
                ((null? (caddr datum))
                 (eopl:error 'parse-expression
                             "Parse error ~s: lambda body is empty!"
                             datum))
                ((not ((list-of symbol?) (cadr datum)))
                 (eopl:error 'parse-expression
                             "Parse error ~s: invalid bound var list!"
                             datum))
                (else
                 (lambda-exp
                  (map (lambda (var) (one-var-item var)) (cadr datum))
                  (parse-expression (caddr datum)))))
          (app-exp
           (parse-expression (car datum))
           (map (lambda (exp) (parse-expression exp)) (cdr datum)))))
     (else (report-invalid-concrete-syntax datum)))))
(define report-invalid-concrete-syntax
  (lambda (exp)
    (eopl:error 'parse-expression "Invalid lambda exp ~s." exp)))
;;; ----- test -----
(parse-expression 'a)
(parse-expression '(lambda () c))
(parse-expression '(lambda (a b) c))
(parse-expression '(a (lambda (a b) c)))
(parse-expression '(a (lambda (a b) c) (a (lambda (a b) c))))
(parse-expression '(lambda))
(parse-expression '(lambda ()))
(parse-expression '(lambda () x))
(parse-expression '(lambda (x y z) ()))
(parse-expression '(lambda ((x) y z) ()))
(parse-expression '((lambda ((x) y z) ())))

#lang eopl
(require racket/match)

(define cps
  (lambda (exp)
    (cps-of exp '(lambda (v0) v0))))

(define cps-of
  (lambda (exp k)
    (match exp
      [var
       #:when (or (symbol? var) (integer? var))
       (list k var)]
      [(list 'lambda (list vars ...) body)
       (cps-of-lambda vars body 'k)]
      [(list 'if exp1 exp2 exp3)
       (cps-of-if exp1 exp2 exp3 k)]
      [_
       (cps-of-exps exp '(0) k)])))

(define cps-of-if
  (lambda (exp1 exp2 exp3 k)
    (if (simple? exp1)
        `(if ,exp1
             ,(cps-of exp2 k)
             ,(cps-of exp3 k))
        (cps-of exp1
                `(lambda (v)
                   (if v
                       ,(cps-of exp2 k)
                       ,(cps-of exp3 k)))))))

(define cps-of-lambda
  (lambda (vars body k)
    `(lambda ,(append vars (list k))
       ,(cps-of body k))))

(define cps-of-exps
  (lambda (exps n k)
    (if (null? n)
        (make-result exps k)
        (let ((exp (current-exp exps (reverse n))))
          (cond [(null? exp)
                 (if (null? (cdr n))
                     (make-result exps k)
                     (cps-of-exps exps
                                  (cons (+ (cadr n) 1)
                                        (cddr n))
                                  k))]
                [(lambda? exp)
                 (cps-of-exps
                  (make-exps exps
                             (reverse n)
                             (cps-of exp 'k))
                  (cons (+ (car n) 1)
                        (cdr n))
                  k)]
                [(simple? exp)
                 (cps-of-exps exps
                              (cons (+ (car n) 1)
                                    (cdr n))
                              k)]
                [(almost-simple? exp)
                 (cps-of-exps exps
                              (cons 1 n)
                              k)]
                [else
                 (cps-of exp
                         `(lambda (,(make-var (car n)))
                            ,(cps-of-exps
                              (make-exps exps (reverse n) (make-var (car n)))
                              (cons (+ (car n) 1) (cdr n))
                              k)))])))))

(define make-result
  (lambda (exps k)
    (match exps
      [(list (or '+ '- '* '/ 'cons 'car 'cdr 'null? 'list) ops ...)
       (list k exps)]
      [_
       (append exps (list k))])))

(define simple?
  (lambda (exp)
    (match exp
      [var
       #:when (or (symbol? var) (integer? var))
       #t]
      [(list 'lambda (list vars ...) body)
       #t]
      [(list (or '+ '- '* '/ 'cons 'car 'cdr 'null? 'list) ops ...)
       ((list-of simple?) ops)]
      [(list 'if exp1 exp2 exp3)
       (and (simple? exp1)
            (simple? exp2)
            (simple? exp3))]
      [_
       #f])))

(define lambda?
  (lambda (exp)
    (match exp
      [(list 'lambda (list vars ...) body)
       #t]
      [_
       #f])))

(define almost-simple?
  (lambda (exp)
    (match exp
      [(list (or '+ '- '* '/ 'cons 'car 'cdr 'null? 'list) ops ...)
       #t]
      [_
       #f])))

(define current-exp
  (lambda (exps pos)
    (cond [(null? pos) exps]
          [(>= (car pos) (length exps))
           '()]
          [else
           (current-exp (list-ref exps (car pos)) (cdr pos))])))

(define make-var
  (lambda (i)
    (string->symbol (string-append "v" (number->string i)))))

(define make-exps
  (lambda (exps n var)
    (cond [(null? n)
           var]
          [(= 0 (car n))
           (cons (make-exps (car exps) (cdr n) var)
                 (cdr exps))]
          [else
           (cons
            (car exps)
            (make-exps (cdr exps)
                       (cons (- (car n) 1) (cdr n))
                       var))])))

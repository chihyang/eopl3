#lang eopl
(require "exer06.34.anf-in-lang.scm")
(require "exer06.34.anf-out-lang.scm")
(require "exer06.34.scm")
(require "exer06.35.cps-out-lang.scm")
(provide cps-compile)

;;; list-index : pred x list -> Int | #f
;;;
;;; usage: return the 0-based position of the first element of lst that
;;; satisfies the predicate pred. If no element of lst satisfies the predicate,
;;; then returns #f
(define list-index
  (lambda (pred lst)
    (letrec ((list-index-iter
              (lambda (pred lst n)
                (if (null? lst)
                    #f
                    (if (pred (car lst))
                        n
                        (list-index-iter pred (cdr lst) (+ n 1)))))))
      (list-index-iter pred lst 0))))

;;; list-set : list x n x x -> list
;;;
;;; usage: return a list with nth-element in lst replaced by x
(define list-set
  (lambda (lst n x)
    (if (null? lst)
        '()
        (if (eq? n 0)
            (cons x (cdr lst))
            (cons (car lst)
                  (list-set (cdr lst) (- n 1) x))))))

;;; ---------------------- CPS ----------------------
(define cps-of-program
  (lambda (prgm)
    (set! n 0)
    (cases anf-program prgm
           (anf-a-program
            (exp)
            (cps-a-program
             (cps-of-exp exp
                         (cps-proc-exp '(v0)
                                       (cps-simple-exp->exp (cps-var-exp 'v0)))))))))
(define cps-compile cps-of-program)

(define cps-of-exp
  (lambda (exp k)
    (cases tf-exp exp
           (simple-exp->exp
            (simple)
            (make-send-to-cont k (cps-of-simple-exp simple)))
           (anf-if-exp
            (exp1 exp2 exp3)
            (cps-if-exp
             (cps-of-simple-exp exp1)
             (cps-of-exp exp2 k)
             (cps-of-exp exp3 k)))
           (anf-let-exp
            (vars exp1 body)
            (let ((var-len (length vars)))
              (cond [(eq? var-len 0)
                     (cps-of-exp body k)]
                    [(eq? var-len 1)
                     (if (inp-exp-simple? (car exp1))
                         (cps-of-exp
                          (anf-call-exp
                           (anf-proc-exp vars body)
                           (list (inp-exp-get-simple (car exp1))))
                          k)
                         (cps-of-exp
                          (car exp1)
                          (cps-proc-exp
                           vars
                           (cps-of-exp body k))))]
                    [else
                     (cps-of-exps
                      exp1
                      (lambda (simples)
                        (let ((var (fresh-identifier '%v)))
                          (cps-call-exp
                           (cps-proc-exp
                            (append vars (list var))
                            (cps-of-exp body (cps-var-exp var)))
                           (append simples (list k))))))])))
           (anf-letrec-exp
            (p-names p-vars p-bodies body)
            (cps-letrec-exp
             p-names
             (map (lambda (vars) (append vars (list 'k)))
                  p-vars)
             (map (lambda (body) (cps-of-exp body (cps-var-exp 'k)))
                  p-bodies)
             (cps-of-exp body k)))
           (anf-call-exp
            (rator rands)
            (let ((r (map cps-of-simple-exp (cons rator rands))))
              (cps-call-exp (car r) (append (cdr r) (list k))))))))

(define cps-of-simple-exp
  (lambda (simple)
    (cases simple-exp simple
           (anf-const-exp
            (num)
            (cps-const-exp num))
           (anf-var-exp
            (var)
            (cps-var-exp var))
           (anf-proc-exp
            (vars body)
            (cps-proc-exp
             (append vars (list 'k%00))
             (cps-of-exp body (cps-var-exp 'k%00))))
           (anf-zero?-exp
            (exp1)
            (cps-zero?-exp (cps-of-simple-exp exp1)))
           (anf-diff-exp
            (exp1 exp2)
            (cps-diff-exp
             (cps-of-simple-exp exp1)
             (cps-of-simple-exp exp2)))
           (anf-sum-exp
            (exps)
            (cps-sum-exp
             (map cps-of-simple-exp exps)))
           (anf-less?-exp
            (exp1 exp2)
            (cps-less?-exp (cps-of-simple-exp exp1)
                           (cps-of-simple-exp exp1))))))

;;; cps-of-exps: Listof(InpExp) x (Listof(InpExp) -> TfExp) -> TfExp
(define cps-of-rest
  (lambda (exps builder)
    (let ((pos (list-index
                (lambda (exp)
                  (not (inp-exp-simple? exp)))
                exps)))
      (if (not pos)
          (builder (map (lambda (e) (cps-of-simple-exp
                                     (inp-exp-get-simple e)))
                        exps))
          (let ((var (fresh-identifier 'v)))
            (cps-of-exp
             (list-ref exps pos)
             (cps-proc-exp (list var)
                           (cps-of-rest
                            (list-set exps pos (simple-exp->exp (anf-var-exp var)))
                            builder))))))))
(define cps-of-exps
  (lambda (exps builder)
    (cps-of-rest exps builder)))

;;; inp-exp-simple? : InpExp -> Bool
(define inp-exp-simple?
  (lambda (exp)
    (cases tf-exp exp
           (simple-exp->exp (s) #t)
           (else #f))))

(define inp-exp-get-simple
  (lambda (exp)
    (cases tf-exp exp
           (simple-exp->exp (s) s)
           (else
            (report-invalid-exp-to-cps-of-simple-exp exp)))))

;;; make-send-to-cont : SimpleExp x SimpleExp -> TfExp
(define make-send-to-cont
  (lambda (k-exp s-exp)
    (cps-call-exp k-exp (list s-exp))))

(define n 'uninitiated)
(define fresh-identifier
  (lambda (v)
    (set! n (+ n 1))
    (string->symbol (string-append (symbol->string v) (number->string n)))))

(define report-invalid-exp-to-cps-of-simple-exp
  (lambda (exp)
    (eopl:error "invalid exp to cps: ~a~%" exp)))

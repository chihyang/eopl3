#lang eopl
(require "cps-in-lang.scm")
(require "cps-out-lang.scm")
(provide compile)

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
    (cases program prgm
           (a-program
            (exp)
            (cps-a-program
             (cps-of-exp exp
                         (cps-proc-exp '(v0)
                                       (simple-exp->exp (cps-var-exp 'v0)))))))))
(define compile cps-of-program)

(define cps-of-exp
  (lambda (exp k)
    (cases expression exp
           (const-exp
            (num)
            (make-send-to-cont k (cps-const-exp num)))
           (var-exp
            (var)
            (make-send-to-cont k (cps-var-exp var)))
           (proc-exp
            (vars body)
            (make-send-to-cont k (cps-proc-exp
                                  (append vars (list 'k))
                                  (cps-of-exp body (cps-var-exp 'k)))))
           (zero?-exp
            (exp1)
            (cps-of-exp/ctx
             exp1
             (lambda (simple)
               (make-send-to-cont k (cps-zero?-exp simple)))))
           (diff-exp
            (exp1 exp2)
            (cps-of-exp/ctx
             exp1
             (lambda (simple1)
               (cps-of-exp/ctx
                exp2
                (lambda (simple2)
                  (make-send-to-cont k (cps-diff-exp simple1 simple2)))))))
           (sum-exp
            (exps)
            (cps-of-exps
             exps
             (lambda (simples)
               (make-send-to-cont k (cps-sum-exp simples)))))
           (if-exp
            (exp1 exp2 exp3)
            (cps-of-exp/ctx
             exp1
             (lambda (simple1)
               (cps-if-exp
                simple1
                (cps-of-exp exp2 k)
                (cps-of-exp exp3 k)))))
           (let-exp
            (vars exp1 body)
            (cps-of-exps
             exp1
             (lambda (simples)
               (let ((var (fresh-identifier '%v)))
                 (cps-let-exp (append vars (list var))
                              (append simples (list k))
                              (cps-of-exp body (cps-var-exp var)))))))
           (letrec-exp
            (p-names p-vars p-bodies body)
            (cps-letrec-exp
             p-names
             (map (lambda (vars) (append vars (list 'k)))
                  p-vars)
             (map (lambda (body) (cps-of-exp body (cps-var-exp 'k)))
                  p-bodies)
             (cps-of-exp body k)))
           (call-exp
            (rator rands)
            (cps-of-exp/ctx
             rator
             (lambda (simple1)
               (cps-of-exps rands
                            (lambda (simples)
                              (cps-call-exp simple1
                                            (append simples (list k)))))))))))

;;; cps-of-exp/ctx: InpExp x (SimpleExp -> TfExp) -> TfExp
(define cps-of-exp/ctx
  (lambda (exp ctx)
    (if (inp-exp-simple? exp)
        (ctx (cps-of-simple-exp exp))
        (let ((var (fresh-identifier 'v)))
          (cps-of-exp exp
                      (cps-proc-exp (list var)
                                    (ctx (cps-var-exp var))))))))

;;; cps-of-rest: Listof(InpExp) x (Listof(InpExp) -> TfExp) -> TfExp
(define cps-of-rest
  (lambda (exps builder)
    (let ((pos (list-index
                (lambda (exp)
                  (not (inp-exp-simple? exp)))
                exps)))
      (if (not pos)
          (builder (map cps-of-simple-exp exps))
          (let ((var (fresh-identifier 'v)))
            (cps-of-exp
             (list-ref exps pos)
             (cps-proc-exp (list var)
                           (cps-of-rest
                            (list-set exps pos (var-exp var))
                            builder))))))))

;;; cps-of-rest: Listof(InpExp) x (Listof(InpExp) -> TfExp) -> TfExp
(define cps-of-exps
  (lambda (exps builder)
    (cps-of-rest exps builder)))

;;; inp-exp-simple? : InpExp -> Bool
(define inp-exp-simple?
  (lambda (exp)
    (cases expression exp
           (const-exp (num) #t)
           (var-exp (var) #t)
           (diff-exp
            (exp1 exp2)
            (and (inp-exp-simple? exp1)
                 (inp-exp-simple? exp2)))
           (zero?-exp
            (exp1)
            (inp-exp-simple? exp1))
           (proc-exp
            (ids exp)
            #t)
           (sum-exp
            (exps)
            ((list-of inp-exp-simple?) exps))
           (else #f))))

;;; cps-of-simple-exp : InpExp -> SimpleExp
;;; usage: assumes (inp-exp-simple exp).
(define cps-of-simple-exp
  (lambda (exp)
    (cases expression exp
           (const-exp (num) (cps-const-exp num))
           (var-exp (var) (cps-var-exp var))
           (diff-exp (exp1 exp2)
                     (cps-diff-exp
                      (cps-of-simple-exp exp1)
                      (cps-of-simple-exp exp2)))
           (zero?-exp (exp1)
                      (cps-zero?-exp (cps-of-simple-exp exp1)))
           (proc-exp (ids exp)
                     (cps-proc-exp
                      (append ids (list 'k%00))
                      (cps-of-exp exp (cps-var-exp 'k%00))))
           (sum-exp (exps)
                    (cps-sum-exp (map cps-of-simple-exp exps)))
           (else
            (report-invalid-exp-to-cps-of-simple-exp exp)))))

;;; make-send-to-cont : SimpleExp x SimpleExp -> TfExp
(define make-send-to-cont
  (lambda (k-exp simple-exp)
    (cps-call-exp k-exp (list simple-exp))))

(define n 'uninitiated)
(define fresh-identifier
  (lambda (v)
    (set! n (+ n 1))
    (string->symbol (string-append (symbol->string v) (number->string n)))))

(define report-invalid-exp-to-cps-of-simple-exp
  (lambda (exp)
    (eopl:error "invalid exp to cps: ~a~%" exp)))

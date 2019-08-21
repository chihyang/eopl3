#lang eopl
(require "exer6.34.anf-in-lang.scm")
(require "exer6.34.anf-out-lang.scm")
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

;;; ---------------------- ANF ----------------------
(define anf-of-program
  (lambda (prgm)
    (set! n 0)
    (cases program prgm
           (a-program
            (exp)
            (anf-a-program
             (anf-of-exp exp))))))

(define compile anf-of-program)

(define anf-of-exp
  (lambda (exp)
    (cases expression exp
           (const-exp
            (num)
            (simple-exp->exp (anf-const-exp num)))
           (var-exp
            (var)
            (simple-exp->exp (anf-var-exp var)))
           (proc-exp
            (vars body)
            (simple-exp->exp (anf-proc-exp
                              vars
                              (anf-of-exp body))))
           (zero?-exp
            (exp1)
            (anf-of-exps
             (list exp1)
             (lambda (simples)
               (simple-exp->exp (anf-zero?-exp (car simples))))))
           (less?-exp
            (exp1 exp2)
            (anf-of-exps
             (list exp1 exp2)
             (lambda (simples)
               (simple-exp->exp (anf-less?-exp (car simples) (cadr simples))))))
           (diff-exp
            (exp1 exp2)
            (anf-of-exps
             (list exp1 exp2)
             (lambda (simples)
               (simple-exp->exp (anf-diff-exp (car simples) (cadr simples))))))
           (sum-exp
            (exps)
            (anf-of-exps
             exps
             (lambda (simples)
               (simple-exp->exp (anf-sum-exp simples)))))
           (if-exp
            (exp1 exp2 exp3)
            (anf-of-exps (list exp1)
                         (lambda (simples)
                           (anf-if-exp
                            (car simples)
                            (anf-of-exp exp2)
                            (anf-of-exp exp3)))))
           (let-exp
            (vars exp1 body)
            (anf-let-exp
             vars
             (map anf-of-exp exp1)
             (anf-of-exp body)))
           (letrec-exp
            (p-names p-vars p-bodies body)
            (anf-letrec-exp
             p-names
             p-vars
             (map anf-of-exp p-bodies)
             (anf-of-exp body)))
           (call-exp
            (rator rands)
            (anf-of-exps (cons rator rands)
                         (lambda (simples)
                           (anf-call-exp (car simples)
                                         (cdr simples))))))))

;;; anf-of-exps: Listof(InpExp) x (Listof(InpExp) -> TfExp) -> TfExp
(define anf-of-rest
  (lambda (exps builder)
    (let ((pos (list-index
                (lambda (exp)
                  (not (inp-exp-simple? exp)))
                exps)))
      (if (not pos)
          (builder (map anf-of-simple-exp exps))
          (let ((var (fresh-identifier 'v)))
            (anf-let-exp
             (list var)
             (list (anf-of-exp (list-ref exps pos)))
             (anf-of-rest
              (list-set exps pos (var-exp var))
              builder)))))))

(define anf-of-exps
  (lambda (exps builder)
    (anf-of-rest exps builder)))

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
           (less?-exp
            (exp1 exp2)
            (and (inp-exp-simple? exp1)
                 (inp-exp-simple? exp2)))
           (proc-exp
            (ids exp)
            #t)
           (sum-exp
            (exps)
            ((list-of inp-exp-simple?) exps))
           (else #f))))

;;; anf-of-simple-exp : InpExp -> SimpleExp
;;; usage: assumes (inp-exp-simple exp).
(define anf-of-simple-exp
  (lambda (exp)
    (cases expression exp
           (const-exp (num) (anf-const-exp num))
           (var-exp (var) (anf-var-exp var))
           (diff-exp (exp1 exp2)
                     (anf-diff-exp
                      (anf-of-simple-exp exp1)
                      (anf-of-simple-exp exp2)))
           (zero?-exp (exp1)
                      (anf-zero?-exp (anf-of-simple-exp exp1)))
           (less?-exp (exp1 exp2)
                      (anf-less?-exp
                       (anf-of-simple-exp exp1)
                       (anf-of-simple-exp exp2)))
           (proc-exp (ids exp)
                     (anf-proc-exp
                      ids
                      (anf-of-exp exp)))
           (sum-exp (exps)
                    (anf-sum-exp (map anf-of-simple-exp exps)))
           (else
            (report-invalid-exp-to-anf-of-simple-exp exp)))))

(define n 'uninitiated)
(define fresh-identifier
  (lambda (v)
    (set! n (+ n 1))
    (string->symbol (string-append (symbol->string v) (number->string n)))))

(define report-invalid-exp-to-anf-of-simple-exp
  (lambda (exp)
    (eopl:error "invalid exp to anf: ~a~%" exp)))

(define report-invalid-cont-exp
  (lambda (exp)
    (eopl:error "invalid cont exp: ~a~%" exp)))

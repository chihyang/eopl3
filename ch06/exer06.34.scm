#lang eopl
(require "exer06.34.anf-in-lang.scm")
(require "exer06.34.anf-out-lang.scm")
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
(define end-cont
  (lambda (v0) v0))

(define anf-of-program
  (lambda (prgm)
    (set! n 0)
    (cases program prgm
           (a-program
            (exp)
            (anf-a-program
             (anf-of-exp exp end-cont))))))

(define compile anf-of-program)

(define anf-of-exp
  (lambda (exp k)
    (cases expression exp
           (const-exp
            (num)
            (make-send-to-cont k (anf-const-exp num)))
           (var-exp
            (var)
            (make-send-to-cont k (anf-var-exp var)))
           (proc-exp
            (vars body)
            (make-send-to-cont k (anf-proc-exp
                                  vars
                                  (anf-of-exp body end-cont))))
           (zero?-exp
            (exp1)
            (anf-of-exps
             (list exp1)
             (lambda (simples)
               (make-send-to-cont k (anf-zero?-exp (car simples))))))
           (less?-exp
            (exp1 exp2)
            (anf-of-exps
             (list exp1 exp2)
             (lambda (simples)
               (make-send-to-cont k (anf-less?-exp (car simples) (cadr simples))))))
           (diff-exp
            (exp1 exp2)
            (anf-of-exps
             (list exp1 exp2)
             (lambda (simples)
               (make-send-to-cont k (anf-diff-exp (car simples) (cadr simples))))))
           (sum-exp
            (exps)
            (anf-of-exps
             exps
             (lambda (simples)
               (make-send-to-cont k (anf-sum-exp simples)))))
           (if-exp
            (exp1 exp2 exp3)
            (anf-of-exps (list exp1)
                         (lambda (simples)
                           (anf-if-exp
                            (car simples)
                            (anf-of-exp exp2 k)
                            (anf-of-exp exp3 k)))))
           (let-exp
            (vars exp1 body)
            (anf-of-exp
             (call-exp
              (proc-exp vars body)
              exp1)
             k))
           (letrec-exp
            (p-names p-vars p-bodies body)
            (anf-letrec-exp
             p-names
             p-vars
             (map (lambda (e) (anf-of-exp e k)) p-bodies)
             (anf-of-exp body k)))
           (call-exp
            (rator rands)
            (anf-of-exps (cons rator rands)
                         (lambda (simples)
                           (make-call-send-to-cont
                            k
                            (anf-a-call-exp (car simples)
                                            (cdr simples)))))))))

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
            (anf-of-exp
             (list-ref exps pos)
             (anf-proc-exp (list var)
                           (anf-of-rest
                            (list-set exps pos (var-exp var))
                            builder))))))))

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
                      (anf-of-exp exp end-cont)))
           (sum-exp (exps)
                    (anf-sum-exp (map anf-of-simple-exp exps)))
           (else
            (report-invalid-exp-to-anf-of-simple-exp exp)))))

;;; make-send-to-cont : SimpleExp x SimpleExp -> TfExp
(define make-send-to-cont
  (lambda (k-exp s-exp)
    (if (eq? k-exp end-cont)
        (simple-exp->exp (anf-simple-exp->exp s-exp))
        (cases simple-exp k-exp
               (anf-proc-exp
                (vars body)
                (anf-let-exp
                 (list (car vars))
                 (list (anf-simple-exp->exp s-exp))
                 body))
               (else
                (report-invalid-cont-exp k-exp))))))

;;; make-call-send-to-cont : SimpleExp x CallExp -> TfExp
(define make-call-send-to-cont
  (lambda (k-exp c-exp)
    (if (eq? k-exp end-cont)
        (simple-exp->exp (anf-call-exp->exp c-exp))
        (cases simple-exp k-exp
               (anf-proc-exp
                (vars body)
                (anf-let-exp
                 (list (car vars))
                 (list (anf-call-exp->exp c-exp))
                 body))
               (else
                (report-invalid-cont-exp k-exp))))))

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

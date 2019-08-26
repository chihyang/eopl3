#lang eopl
(require "exer6.37.cps-in-lang.scm")
(require "exer6.37.cps-out-lang.scm")
(provide compile explicit-of-exp)

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

;;; ---------------------- Mut Environment ----------------------
(define-datatype mut-env mut-env?
  (empty-mut-env)
  (extend-mut-env
   (var symbol?)
   (mut? boolean?)
   (env mut-env?))
  (extend-mut-env*
   (vars (list-of symbol?))
   (mut?s (list-of boolean?))
   (env mut-env?)))

(define init-mut-env
  (lambda () (empty-mut-env)))

;;; apply-mut-env : MutEnv x Var -> Bool
(define apply-mut-env
  (lambda (menv var)
    (cases mut-env menv
           (empty-mut-env
            ()
            #t)
           (extend-mut-env
            (saved-var saved-mut? saved-env)
            (if (eq? var saved-var)
                saved-mut?
                (apply-mut-env saved-env var)))
           (extend-mut-env*
            (saved-vars saved-mut?s saved-env)
            (if (null? saved-vars)
                (apply-mut-env saved-env var)
                (if (eq? var (car saved-vars))
                    (car saved-mut?s)
                    (apply-mut-env
                     (extend-mut-env*
                      (cdr saved-vars)
                      (cdr saved-mut?s)
                      saved-env)
                     var)))))))

;;; ---------------------- Explicit ----------------------
;;; immutable? : Sym x Exp -> Bool
(define immutable?
  (lambda (var exp)
    (cases expression exp
           (const-exp
            (num)
            #t)
           (var-exp
            (saved-var)
            #t)
           (proc-exp
            (vars body)
            (if (memq var vars)
                #t
                (immutable? var body)))
           (zero?-exp
            (exp1)
            (immutable? var exp1))
           (diff-exp
            (exp1 exp2)
            (and (immutable? var exp1)
                 (immutable? var exp2)))
           (sum-exp
            (exps)
            ((list-of (lambda (e) (immutable? var e))) exps))
           (if-exp
            (exp1 exp2 exp3)
            (and
             (immutable? var exp1)
             (immutable? var exp2)
             (immutable? var exp3)))
           (let-exp
            (vars exps body)
            (if ((list-of (lambda (e) (immutable? var e))) exps)
                (if (memq var vars)
                    #t
                    (immutable? var body))
                #f))
           (letrec-exp
            (p-names p-vars p-bodies body)
            (if (memq var p-names)
                #t
                ((list-of (lambda (e) (immutable? var e)))
                 (cons body (map (lambda (vs bd) (proc-exp vs bd)) p-vars p-bodies)))))
           (call-exp
            (rator rands)
            ((list-of (lambda (e) (immutable? var e))) (cons rator rands)))
           (print-exp
            (exp1)
            (immutable? var exp1))
           (begin-exp
            (exp1 exps)
            ((list-of (lambda (e) (immutable? var e))) (cons exp1 exps)))
           (assign-exp
            (saved-var exp1)
            (not (eq? saved-var var)))
           (else
            (report-invalid-implicit-ref-exp exp)))))

(define explicit-of-prgm
  (lambda (prgm)
    (cases program prgm
           (a-program
            (exp)
            (a-program
             (explicit-of-exp exp (init-mut-env)))))))

(define explicit-of-exp
  (lambda (exp env)
    (cases expression exp
           (const-exp
            (num)
            exp)
           (var-exp
            (var)
            (if (apply-mut-env env var)
                exp
                (ideref-exp exp)))
           (proc-exp
            (vars body)
            (proc-exp
             vars
             (explicit-of-exp
              body
              (extend-mut-env* vars (map (lambda (v) #f) vars) env))))
           (zero?-exp
            (exp1)
            (zero?-exp (explicit-of-exp exp1 env)))
           (diff-exp
            (exp1 exp2)
            (diff-exp
             (explicit-of-exp exp1 env)
             (explicit-of-exp exp2 env)))
           (sum-exp
            (exps)
            (sum-exp (map (lambda (e) (explicit-of-exp e env)) exps)))
           (if-exp
            (exp1 exp2 exp3)
            (if-exp
             (explicit-of-exp exp1 env)
             (explicit-of-exp exp2 env)
             (explicit-of-exp exp3 env)))
           (let-exp
            (vars exps body)
            (let ((mut-properties (map (lambda (v) (immutable? v body)) vars)))
              (let-exp
               vars
               (map (lambda (imut? e)
                      (if imut?
                          (explicit-of-exp e env)
                          (inewref-exp (explicit-of-exp e env))))
                    mut-properties
                    exps)
               (explicit-of-exp body (extend-mut-env* vars mut-properties env)))))
           (letrec-exp
            (p-names p-vars p-bodies body)
            (let ((mut-properties (map (lambda (v) #f) p-names)))
              (let ((rec-env (extend-mut-env* p-names mut-properties env)))
                (letrec-exp
                 p-names p-vars
                 (explicit-of-letrec-p-bodies p-vars p-bodies rec-env)
                 (explicit-of-exp body rec-env)))))
           (call-exp
            (rator rands)
            (call-exp (explicit-of-exp rator env)
                      (map (lambda (e) (inewref-exp (explicit-of-exp e env))) rands)))
           (print-exp
            (exp1)
            (print-exp (explicit-of-exp exp1 env)))
           (begin-exp
            (exp1 exps)
            (begin-exp
             (explicit-of-exp exp1 env)
             (map (lambda (e) (explicit-of-exp e env)) exps)))
           (assign-exp
            (var exp1)
            (assign-exp
             var
             (explicit-of-exp exp1 env)))
           (else
            (report-invalid-implicit-ref-exp exp)))))

(define explicit-of-letrec-p-bodies
  (lambda (p-vars p-bodies env)
    (if (null? p-vars)
        '()
        (let ((new-env (extend-mut-env* (car p-vars)
                                        (map (lambda (v) #f) (car p-vars))
                                        env)))
          (cons (explicit-of-exp (car p-bodies) new-env)
                (explicit-of-letrec-p-bodies (cdr p-vars) (cdr p-bodies) env))))))

(define report-invalid-implicit-ref-exp
  (lambda (exp)
    (eopl:error "invalid exp to explicit ref exp: ~s~%" exp)))

;;; ---------------------- CPS ----------------------
(define cps-of-program
  (lambda (prgm)
    (set! n 0)
    (cases program prgm
           (a-program
            (exp)
            (cps-a-program
             (cps-of-exp (explicit-of-exp exp (init-mut-env))
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
            (cps-of-exps
             (list exp1)
             (lambda (simples)
               (make-send-to-cont k (cps-zero?-exp (car simples))))))
           (diff-exp
            (exp1 exp2)
            (cps-of-exps
             (list exp1 exp2)
             (lambda (simples)
               (make-send-to-cont k (cps-diff-exp (car simples) (cadr simples))))))
           (sum-exp
            (exps)
            (cps-of-exps
             exps
             (lambda (simples)
               (make-send-to-cont k (cps-sum-exp simples)))))
           (if-exp
            (exp1 exp2 exp3)
            (cps-of-exps (list exp1)
                         (lambda (simples)
                           (cps-if-exp
                            (car simples)
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
            (cps-of-exp
             (let-exp
              p-names
              (map (lambda (v) (inewref-exp (const-exp 156))) p-names)
              (if (null? p-names)
                  body
                  (let ((binds
                         (map (lambda (p-name p-vars p-body)
                                (assign-exp p-name (proc-exp p-vars p-body)))
                              p-names p-vars p-bodies)))
                    (begin-exp
                     (car binds)
                     (append (cdr binds) (list body))))))
             k))
           (call-exp
            (rator rands)
            (cps-of-exps (cons rator rands)
                         (lambda (simples)
                           (cps-call-exp (car simples)
                                         (append (cdr simples) (list k))))))
           (print-exp
            (exp1)
            (cps-of-exps (list exp1)
                         (lambda (simples)
                           (cps-printk-exp
                            (car simples)
                            (make-send-to-cont k (cps-const-exp 38))))))
           (inewref-exp
            (exp1)
            (cps-of-exps (list exp1)
                         (lambda (simples)
                           (cps-newrefk-exp
                            (car simples)
                            k))))
           (ideref-exp
            (exp1)
            (cps-of-exps (list exp1)
                         (lambda (simples)
                           (cps-derefk-exp
                            (car simples)
                            k))))
           (assign-exp
            (var exp1)
            (cps-of-exps (list exp1)
                         (lambda (simples)
                           (cps-setrefk-exp
                            (cps-var-exp var)
                            (car simples)
                            (make-send-to-cont k (cps-const-exp 27))))))
           (begin-exp
            (exp1 exps)
            (if (null? exps)
                (cps-of-exps (list exp1)
                             (lambda (simples)
                               (make-send-to-cont k (car simples))))
                (cps-of-exps (list exp1)
                             (lambda (simples)
                               (cps-of-exp (begin-exp (car exps) (cdr exps)) k)))))
           (else
            (report-invalid-exp-to-cps-of-simple-exp exp)))))

;;; cps-of-exps: Listof(InpExp) x (Listof(InpExp) -> TfExp) -> TfExp
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
    (eopl:error "invalid exp to cps: ~s~%" exp)))

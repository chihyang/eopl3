#lang eopl
;;; ---------------------- Utility ----------------------
(define member?
  (lambda (sym lst)
    (if (null? lst)
        #f
        (or (eqv? sym (car lst))
            (member? sym (cdr lst))))))
(define check-duplicates
  (lambda (lst)
    (cond ((null? lst) '())
          ((member? (car lst) (cdr lst)) (car lst))
          (else (check-duplicates (cdr lst))))))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))

;;; ---------------------- Expval ----------------------
(define identifier? symbol?)
(define-datatype exp-val exp-val?
  (num-val
   (val number?))
  (bool-val
   (val boolean?))
  (null-val)
  (pair-val
   (val1 exp-val?)
   (val2 exp-val?))
  (proc-val
   (val proc?)))
(define expval->num
  (lambda (value)
    (cases exp-val value
           (num-val
            (number)
            number)
           (else
            (report-invalid-exp-value 'num)))))
(define expval->bool
  (lambda (value)
    (cases exp-val value
           (bool-val
            (boolean)
            boolean)
           (else
            (report-invalid-exp-value 'bool)))))
(define expval->null
  (lambda (value)
    (cases exp-val value
           (null-val
            ()
            '())
           (else
            (report-invalid-exp-value 'null)))))
(define expval->pair
  (lambda (value)
    (cases exp-val value
           (pair-val
            (val1 val2)
            (cons
             (cases exp-val val1
                    (num-val (num) num)
                    (bool-val (bool) bool)
                    (null-val () '())
                    (pair-val (val3 val4) (expval->pair val1))
                    (proc-val (proc) proc))
             (cases exp-val val2
                    (num-val (num) num)
                    (bool-val (bool) bool)
                    (null-val () '())
                    (pair-val (val3 val4) (expval->pair val2))
                    (proc-val (proc) proc))))
           (else
            (report-invalid-exp-value 'pair)))))
(define expval->proc
  (lambda (value)
    (cases exp-val value
           (proc-val
            (proc1)
            proc1)
           (else
            (report-invalid-exp-value 'proc)))))
(define report-invalid-exp-value
  (lambda (type)
    (eopl:error
     'exp-val
     "No a valid exp value of type ~s" type)))

;;; ---------------------- Syntax for the PROC language ----------------------
;;; Program    ::= Expression
;;;                a-program (exp1)
;;; Expression ::= Number
;;;                const-exp (num)
;;; Expression ::= emptylist
;;;                emptylist-exp
;;; Expression ::= cons (Expression, Expression)
;;;                cons-exp (exp1 exp2)
;;; Expression ::= car (Expression)
;;;                car-exp (exp1)
;;; Expression ::= cdr (Expression)
;;;                cdr-exp (exp1)
;;; Expression ::= list (Expression, Expression, ...)
;;;                pair-exp (exp1)
;;; Expression ::= null? (Expression)
;;;                null?-exp (exp1)
;;; Expression ::= -(Expression , Expression)
;;;                diff-exp (exp1 exp2)
;;; Expression ::= + (Expression , Expression)
;;;                add-exp (exp1 exp2)
;;; Expression ::= * (Expression , Expression)
;;; Expression ::= / (Expression , Expression)
;;;                quoti-exp (exp1 exp2)
;;; Expression ::= minus (Expression)
;;;                minus-exp (exp1)
;;; Expression ::= zero? (Expression)
;;;                zero?-exp (exp1)
;;; Expression ::= equal? (Expression)
;;;                equal?-exp (exp1)
;;; Expression ::= greater? (Expression)
;;;                greater?-exp (exp1)
;;; Expression ::= less? (Expression)
;;;                less?-exp (exp1)
;;; Expression ::= if Expression then Expression else Expression
;;;                if-exp (exp1 exp2 exp3)
;;; Expression ::= Identifier
;;;                var-exp (var)
;;; Expression ::= let {Identifier = Expression}* in Expression
;;;                let-exp (var exp1 body)
;;; Expression ::= letrec Identifier (Identifier) = Expression in Expression
;;;                letrec-exp (p-name b-var p-exp1 letrec-body)
;;; Expression ::= proc (Identifier) Expression
;;;                proc-exp (var body)
;;; Expression ::= (Expression Expression)
;;;                call-exp (rator rand)
;;; Expression ::= cond {Expression ==> Expression}∗ end
;;;                cond-exp Listof(exp1) Listof(exp2)
;;; Expression ::= unpack {Identifier}* = Expression in Expression
;;;                unpack-exp (var exp1 body)
;;; Expression ::= let* {Identifier = Expression}* in Expression
;;;                let*-exp (var exp1 body)
;;; Expression ::= %lexref number
;;;                nameless-var-exp (num)
;;; Expression ::= %let Expression in Expression
;;;                nameless-let-exp (exp1 body)
;;; Expression ::= %lexproc Expression
;;;                nameless-proc-exp (body)
;;; Expression ::= %letrec-exp {Expression}* in Expression
;;;                nameless-letrec-exp (p-bodies letrec-body)
;;; Parse Expression
(define let-scanner-spec
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number ((or (concat digit (arbno digit))
                 (concat "-" digit (arbno digit))
                 (concat (arbno digit) "." digit (arbno digit))
                 (concat "-" (arbno digit) "." digit (arbno digit))
                 (concat digit (arbno digit) "." (arbno digit))
                 (concat "-" digit (arbno digit) "." (arbno digit)))) number)))
(define let-grammar
  '((program (expression) a-program)
    (expression (number)
                const-exp)
    (expression ("emptylist")
                emptylist-exp)
    (expression ("cons" "(" expression "," expression ")")
                cons-exp)
    (expression ("car" "(" expression ")")
                car-exp)
    (expression ("cdr" "(" expression ")")
                cdr-exp)
    (expression ("null?" "(" expression ")")
                null?-exp)
    (expression ("list" "(" (separated-list expression ",") ")")
                list-exp)
    (expression ("-" "(" expression "," expression ")")
                diff-exp)
    (expression ("+" "(" expression "," expression ")")
                add-exp)
    (expression ("*" "(" expression "," expression ")")
                mul-exp)
    (expression ("/" "(" expression "," expression ")")
                quot-exp)
    (expression ("minus" "(" expression ")")
                minus-exp)
    (expression ("zero?" "(" expression ")")
                zero?-exp)
    (expression ("equal?" "(" expression "," expression ")")
                equal?-exp)
    (expression ("greater?" "(" expression "," expression ")")
                greater?-exp)
    (expression ("less?" "(" expression "," expression ")")
                less?-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression (identifier)
                var-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    (expression ("letrec" (arbno identifier "(" identifier ")" "=" expression) "in" expression)
                letrec-exp)
    (expression ("cond" (arbno expression "==>" expression) "end")
                cond-exp)
    (expression ("proc" "(" identifier ")" expression)
                proc-exp)
    (expression ("(" expression expression ")")
                call-exp)
    (expression ("unpack" (arbno identifier) "=" expression "in" expression)
                unpack-exp)
    (expression ("let*" (arbno identifier "=" expression) "in" expression)
                let*-exp)
    (expression ("%lexref" number)
                nameless-var-exp)
    (expression ("%let" (arbno expression) "in" expression)
                nameless-let-exp)
    (expression ("%lexproc" expression)
                nameless-proc-exp)
    (expression ("%unpack" number "of" expression "in" expression)
                nameless-unpack-exp)
    (expression ("%letrec" (arbno expression) "in" expression)
                nameless-letrec-exp)
    (expression ("%letrecxref" number number)
                nameless-letrec-var-exp)))

;;; ------------ Static Environment(from section 3.7) ----------------
(define-datatype var-dec var-dec?
  (normal-dec)
  (letrec-dec
   (pos number?)))
(define-datatype var-ref var-ref?
  (ref
   (dec var-dec?)
   (pos number?)))
;; Senv = Listof(VarDec)
;; Lexaddr = N
;; empty-senv : () -> Senv
(define empty-senv
  (lambda () '()))
;; extend-senv : Var x Senv -> Senv
(define extend-senv
  (lambda (var senv)
    (cons (cons var (normal-dec)) senv)))
;; extend-senv : Var x Senv -> Senv
(define extend-letrec-senv
  (lambda (var senv)
    (cons (cons var (letrec-dec 0)) senv)))
;; extend-senv : Listof(VarDec) x Senv -> Senv
(define extend-senv*
  (lambda (vars senv)
    (append (reverse
             (map (lambda (var) (cons var (normal-dec)))
                  vars))
            senv)))
;; extend-letrec-senv : Listof(VarDec) x Senv -> Senv
(define extend-letrec-senv*
  (lambda (vars senv)
    (extend-letrec-senv*-inner vars (length vars) senv)))
;; extend-letrec-senv : Listof(VarDec) x Num x Senv -> Senv
(define extend-letrec-senv*-inner
  (lambda (var pos senv)
    (if (null? var)
        senv
        (extend-letrec-senv*-inner
         (cdr var)
         (- pos 1)
         (cons (cons (car var) (letrec-dec (- pos 1))) senv)))))
;; apply-senv : Senv x Var -> VarRef
(define apply-senv
  (lambda (senv var)
    (apply-senv-iter senv var 0)))
;; apply-senv : Senv x Var x Num -> VarRef
(define apply-senv-iter
  (lambda (senv var pos)
    (cond ((null? senv)
           (report-no-binding-found var))
          ((eqv? var (caar senv))
           (ref (cdar senv) pos))
          (else
           (apply-senv-iter (cdr senv) var (+ pos 1))))))
(define report-invalid-var-ref
  (lambda (var)
    (eopl:error "Invalid variable reference: ~a" var)))

;;; --------------------- Translation of Program ---------------------
;; translation-of-program : Program -> Nameless-program
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (a-program
                       (translation-of exp1 (init-senv)))))))
;; translation-of : Exp x Senv -> Nameless-exp
(define translation-of
  (lambda (exp senv)
    (cases expression exp
           (const-exp
            (num)
            (const-exp num))
           (emptylist-exp
            ()
            (emptylist-exp))
           (cons-exp
            (first second)
            (cons-exp
             (translation-of first senv)
             (translation-of second senv)))
           (car-exp
            (exp1)
            (car-exp (translation-of exp1 senv)))
           (cdr-exp
            (exp1)
            (cdr-exp (translation-of exp1 senv)))
           (null?-exp
            (exp1)
            (null?-exp (translation-of exp1 senv)))
           (list-exp
            (exps)
            (list-exp
             (map (lambda (exp) (translation-of exp senv)) exps)))
           (diff-exp
            (exp1 exp2)
            (diff-exp
             (translation-of exp1 senv)
             (translation-of exp2 senv)))
           (add-exp
            (exp1 exp2)
            (add-exp
             (translation-of exp1 senv)
             (translation-of exp2 senv)))
           (mul-exp
            (exp1 exp2)
            (mul-exp
             (translation-of exp1 senv)
             (translation-of exp2 senv)))
           (quot-exp
            (exp1 exp2)
            (quot-exp
             (translation-of exp1 senv)
             (translation-of exp2 senv)))
           (zero?-exp
            (exp1)
            (zero?-exp (translation-of exp1 senv)))
           (minus-exp
            (exp1)
            (minus-exp (translation-of exp1 senv)))
           (equal?-exp
            (exp1 exp2)
            (equal?-exp
             (translation-of exp1 senv)
             (translation-of exp2 senv)))
           (greater?-exp
            (exp1 exp2)
            (greater?-exp
             (translation-of exp1 senv)
             (translation-of exp2 senv)))
           (less?-exp
            (exp1 exp2)
            (less?-exp
             (translation-of exp1 senv)
             (translation-of exp2 senv)))
             (if-exp
            (exp1 exp2 exp3)
            (if-exp (translation-of exp1 senv)
                    (translation-of exp2 senv)
                    (translation-of exp3 senv)))
           (var-exp
            (var)
            (let ((a-ref (apply-senv senv var)))
              (cases
               var-ref a-ref
               (ref (dec pos)
                    (cases var-dec dec
                           (normal-dec
                            ()
                            (nameless-var-exp pos))
                           (letrec-dec
                            (in-pos)
                            (nameless-letrec-var-exp pos in-pos)))))))
           (let-exp
            (vars vals body)
             (let ((dup (check-duplicates vars)))
               (if (null? dup)
                   (nameless-let-exp
                    (map (lambda (exp) (translation-of exp senv))
                         vals)
                    (translation-of body (extend-senv* vars senv)))
                   (report-duplicate-id dup))))
           (cond-exp
            (cond-list val-list)
            (cond-exp
             (map (lambda (val) (translation-of val senv))
                  cond-list)
             (map (lambda (val) (translation-of val senv))
                  val-list)))
           (proc-exp
            (var body)
            (nameless-proc-exp
             (translation-of body (extend-senv var senv))))
           (call-exp
            (rator rand)
            (call-exp
             (translation-of rator senv)
             (translation-of rand senv)))
           (let*-exp
            (vars vals body)
            (if (null? vars)
                (translation-of body senv)
                (nameless-let-exp
                 (list (translation-of (car vals) senv))
                 (translation-of
                  (let*-exp (cdr vars) (cdr vals) body)
                  (extend-senv (car vars) senv)))))
           (unpack-exp
            (vars vals body)
            (let ((dup (check-duplicates vars)))
              (if (null? dup)
                  (nameless-unpack-exp
                   (length vars)
                   (translation-of vals senv)
                   (translation-of body (extend-senv* vars senv)))
                  (report-duplicate-id dup))))
           (letrec-exp
            (p-names p-vars p-bodies letrec-body)
            (let ((dup (check-duplicates p-names)))
              (if (null? dup)
                  (let ((letrec-env (extend-letrec-senv* p-names senv)))
                    (nameless-letrec-exp
                     (map (lambda (p-var p-body)
                            (translation-of
                             p-body
                             (extend-senv p-var letrec-env)))
                          p-vars
                          p-bodies)
                     (translation-of letrec-body letrec-env)))
                  (report-duplicate-id dup))))
           (else
            (report-invalid-source-expression exp)))))
;; translation-of : () -> Senv
(define init-senv
  (lambda ()
    (empty-senv)))
(define report-invalid-source-expression
  (lambda (exp)
    (eopl:error
     'expression
     "No a valid source exp ~a" exp)))
(define report-duplicate-id
  (lambda (sym)
    (eopl:error 'extend-env "Duplicate identifier ~s" sym)))

;;; ------------- Nameless Environment(from section 3.7) -------------
;; nameless-environment? : SchemeVal -> Bool
(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))
(define nameless-environment?
  (lambda (x)
    ((list-of exp-val?) x)))
;; empty-nameless-env : () -> Nameless-env
(define empty-nameless-env
  (lambda ()
    '()))
;; extend-nameless-env : Expval x Nameless-env -> Nameless-env
(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)))
;; extend-nameless-env : Listof(Expval) x Nameless-env -> Nameless-env
(define extend-nameless-env*
  (lambda (vals nameless-env)
    (append (reverse vals) nameless-env)))
;; apply-nameless-env : Nameless-env x Lexaddr -> DenVal
(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)))
(define apply-nameless-env-rec
  (lambda (nameless-env n in-pos)
    (let ((a-proc (list-ref nameless-env n))
          (saved-env (list-tail nameless-env (- n in-pos))))
      (cases
       exp-val a-proc
       (proc-val
        (in-proc)
        (cases
         proc in-proc
         (procedure
          (body env)
          (proc-val (procedure body saved-env)))))
       (else
        (report-invalid-exp-value 'proc))))))

;;; ------------------------ procedure value ------------------------
(define-datatype proc proc?
  (procedure
   (body expression?)
   (saved-nameless-env nameless-environment?)))
;; apply-procedure : Proc x ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
           (procedure
            (body saved-nameless-env)
            (value-of body (extend-nameless-env val saved-nameless-env))))))

;;; ---------------------- Evaluate expression ----------------------
(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
           (const-exp
            (num)
            (num-val num))
           (emptylist-exp
            ()
            (null-val))
           (cons-exp
            (first second)
            (pair-val
             (value-of first nameless-env)
             (value-of second nameless-env)))
           (car-exp
            (exp1)
            (cases exp-val (value-of exp1 nameless-env)
                   (pair-val (val1 val2) val1)
                   (else (report-invalid-exp-value 'pair-val))))
           (cdr-exp
            (exp1)
            (cases exp-val (value-of exp1 nameless-env)
                   (pair-val (val1 val2) val2)
                   (else (report-invalid-exp-value 'pair-val))))
           (null?-exp
            (exp1)
            (cases exp-val (value-of exp1 nameless-env)
                   (null-val () (bool-val #t))
                   (else (bool-val #f))))
           (list-exp
            (exps)
            (if (null? exps)
                (null-val)
                (pair-val (value-of (car exps) nameless-env)
                          (value-of (list-exp (cdr exps)) nameless-env))))
           (diff-exp
            (exp1 exp2)
            (num-val (- (expval->num (value-of exp1 nameless-env))
                        (expval->num (value-of exp2 nameless-env)))))
           (add-exp
            (exp1 exp2)
            (value-of-add-exp exp1 exp2 nameless-env))
           (mul-exp
            (exp1 exp2)
            (value-of-mul-exp exp1 exp2 nameless-env))
           (quot-exp
            (exp1 exp2)
            (value-of-quot-exp exp1 exp2 nameless-env))
           (minus-exp
            (exp1)
            (value-of-minus-exp exp1 nameless-env))
           (zero?-exp
            (exp1)
            (bool-val (eqv? (expval->num (value-of exp1 nameless-env)) 0)))
           (equal?-exp
            (exp1 exp2)
            (value-of-equal?-exp exp1 exp2 nameless-env))
           (greater?-exp
            (exp1 exp2)
            (value-of-greater?-exp exp1 exp2 nameless-env))
           (less?-exp
            (exp1 exp2)
            (value-of-less?-exp exp1 exp2 nameless-env))
           (if-exp
            (exp1 exp2 exp3)
            (if (expval->bool (value-of exp1 nameless-env))
                (value-of exp2 nameless-env)
                (value-of exp3 nameless-env)))
           (call-exp
            (rator rand)
            (let ((proc (expval->proc (value-of rator nameless-env)))
                  (arg (value-of rand nameless-env)))
              (apply-procedure proc arg)))
           (cond-exp
            (cond-list val-list)
            (value-of-cond-exp cond-list val-list nameless-env))
           (nameless-var-exp
            (n)
            (apply-nameless-env nameless-env n))
           (nameless-let-exp
            (exp1 body)
            (let ((vals (map (lambda (val) (value-of val nameless-env))
                             exp1)))
              (value-of body (extend-nameless-env* vals nameless-env))))
           (nameless-proc-exp
            (body)
            (proc-val (procedure body nameless-env)))
           (nameless-unpack-exp
            (var-length vals body)
            (value-of-nameless-unpack-exp var-length vals body nameless-env))
           (nameless-letrec-exp
            (p-bodies letrec-body)
            (value-of
             letrec-body
             (extend-nameless-env*
              (map (lambda (body) (proc-val (procedure body nameless-env)))
                   p-bodies)
              nameless-env)))
           (nameless-letrec-var-exp
            (n in-pos)
            (apply-nameless-env-rec nameless-env n in-pos))
           (else
            (report-invalid-tranlated-expression exp)))))
(define value-of-program
  (lambda (prog)
    (cases program prog
           (a-program
            (exp)
            (let ((val (value-of exp (empty-nameless-env))))
              (cases exp-val val
                     (num-val
                      (num)
                      num)
                     (bool-val
                      (bool)
                      bool)
                     (null-val
                      ()
                      '())
                     (pair-val
                      (val1 val2)
                      (expval->pair val))
                     (proc-val
                      (val)
                      val)))))))
(define value-of-add-exp
  (lambda (exp1 exp2 env)
    (num-val (+ (expval->num (value-of exp1 env))
                (expval->num (value-of exp2 env))))))
(define value-of-mul-exp
  (lambda (exp1 exp2 env)
    (num-val (* (expval->num (value-of exp1 env))
                (expval->num (value-of exp2 env))))))
(define value-of-quot-exp
  (lambda (exp1 exp2 env)
    (num-val (/ (expval->num (value-of exp1 env))
                (expval->num (value-of exp2 env))))))
(define value-of-minus-exp
  (lambda (exp env)
    (num-val (- (expval->num (value-of exp env))))))
(define value-of-equal?-exp
  (lambda (exp1 exp2 env)
    (bool-val (eqv? (expval->num (value-of exp1 env))
                    (expval->num (value-of exp2 env))))))
(define value-of-greater?-exp
  (lambda (exp1 exp2 env)
    (bool-val (> (expval->num (value-of exp1 env))
                 (expval->num (value-of exp2 env))))))
(define value-of-less?-exp
  (lambda (exp1 exp2 env)
    (bool-val (< (expval->num (value-of exp1 env))
                 (expval->num (value-of exp2 env))))))
(define value-of-cond-exp
  (lambda (cond-list val-list env)
    (if (null? cond-list)
        (report-invalid-cond-exp)
        (if (expval->bool (value-of (car cond-list) env))
            (value-of (car val-list) env)
            (value-of-cond-exp (cdr cond-list) (cdr val-list) env)))))
(define value-of-nameless-unpack-exp
  (lambda (var-length exp body env)
    (let ((vals (pair->list (value-of exp env))))
      (if (eqv? var-length (length vals))
          (value-of body (extend-nameless-env* vals env))
          (report-invalid-unpack-exp)))))
(define pair->list
  (lambda (val)
    (pair->list-iter val '())))
(define pair->list-iter
  (lambda (val lst)
    (cases exp-val val
           (null-val
            ()
            lst)
           (pair-val
            (val1 val2)
            (pair->list-iter val2 (append lst (list val1))))
           (else
            (report-invalid-exp-value 'list-val)))))
(define report-invalid-tranlated-expression
  (lambda (exp)
    (eopl:error
     'expression
     "No a valid translated exp ~a" exp)))
(define report-invalid-cond-exp
  (lambda ()
    (eopl:error
     'exp-val
     "Missing case evaluated to true")))
(define report-invalid-unpack-exp
  (lambda ()
    (eopl:error
     'exp-val
     "Unpack value expressions' number is not equal to identifiers'")))

;;; ---------------------- Sllgen operations ----------------------
(sllgen:make-define-datatypes let-scanner-spec let-grammar)
(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes let-scanner-spec let-grammar)))
(define just-scan
  (sllgen:make-string-scanner let-scanner-spec let-grammar))
(define scan&parse
  (sllgen:make-string-parser let-scanner-spec let-grammar))
(define read-eval-print
  (sllgen:make-rep-loop
   "--> "
   value-of-program
   (sllgen:make-stream-parser let-scanner-spec let-grammar)))

;; run : String -> Bool | Int
(define run
  (lambda (string)
    (value-of-program
     (translation-of-program
      (scan&parse string)))))

;;; ---------------------- Test ----------------------
(eqv?
 (run "let x = 7 y = 2 in
         let y = let x = -(x, 1) in -(x, y)
           in -(-(x,8), y)")
 -5)
(eq?
 (run "equal?(1,let x = 1 in -(x,0))")
 #t)
(eq?
 (run "greater?(1,let x = 1 in -(x,0))")
 #f)
(equal?
 (run "let x = 4 in cons(x, cons(cons(-(x,1), emptylist), emptylist))")
 '(4 (3)))
(equal?
 (run "let x = 4 in list(x, -(x,1), -(x,3))")
 '(4 3 1))
(eqv?
 (run "let x = 30
         in let x = -(x,1)
                y = -(x,2)
           in -(x,y)")
 1)
;; error
(run "let x = 30
      in let x = -(x,1)
                x = -(x,2)
         in +(x,x)")
(eqv?
 (run "let x = 30
         in let* x = -(x,1) y = -(x,2)
           in -(x,y)")
 2)
(eqv?
 (run "let x = 30
         in let x = -(x,1) y = -(x,2)
           in -(x,y)")
 1)
(eqv?
 (run "let u = 7
         in unpack x y = cons(u,cons(3,emptylist))
           in -(x,y)")
 4)
;; error
(run "let u = 7
        in unpack x y = cons(u,cons(2,cons(3,emptylist)))
           in -(x,y)")
(eqv?
 (run "cond less?(1, 2) ==> 1 end")
 1)
(eqv?
 (run "let x = 3 in let y = 5 in
         cond
           less?(x, -(x, y)) ==> 1
           equal?(x, -(x, y)) ==> minus(y)
           greater?(x, -(x, y)) ==> /(y,x)
         end")
 5/3)
;; error
(run "let x = 3 in let y = 5 in
         cond
           less?(x, -(x, y)) ==> 1
           equal?(x, -(x, y)) ==> minus(y)
         end")
(eqv?
 (run "letrec double (x) = if zero?(x) then 0
                           else -((double -(x,1)),-2)
       in (double 6)")
 12)
(eqv?
 (run "letrec
         even(x) = if zero?(x) then 1 else (odd -(x,1))
         odd(x)  = if zero?(x) then 0 else (even -(x,1))
       in (odd 13)")
 1)
(eqv?
 (run "let z = 20
       in let m = 32
          in let x = -(z, m)
             in let f = proc (y) -(y,x)
                in let g = f
                   in (proc (func) (func m) g)")
 44)
(eqv?
 (run "let x = 3 in
         let f = proc (y) proc (y) -(y,x) in
           ((f 13) x)")
 0)

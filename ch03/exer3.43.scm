#lang eopl
;;; ---------------------- Utility ----------------------
(define member?
  (lambda (sym lst)
    (if (null? lst)
        #f
        (or (eqv? sym (car lst))
            (member? sym (cdr lst))))))
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
     "Not a valid exp value of type ~s" type)))

;;; ---------------------- Syntax for the PROC language ----------------------
;;; Program    ::= Expression
;;;                a-program (exp1)
;;; Expression ::= Number
;;;                const-exp (num)
;;; Expression ::= -(Expression , Expression)
;;;                diff-exp (exp1 exp2)
;;; Expression ::= zero? (Expression)
;;;                zero?-exp (exp1)
;;; Expression ::= if Expression then Expression else Expression
;;;                if-exp (exp1 exp2 exp3)
;;; Expression ::= Identifier
;;;                var-exp (var)
;;; Expression ::= let Identifier = Expression in Expression
;;;                let-exp (var exp1 body)
;;; Expression ::= proc (Identifier) Expression
;;;                proc-exp (var body)
;;; Expression ::= (Expression Expression)
;;;                call-exp (rator rand)
;;; Expression ::= %lexref number
;;;                nameless-var-exp (num)
;;; Expression ::= %let Expression in Expression
;;;                nameless-let-exp (exp1 body)
;;; Expression ::= %lexproc Expression (number*)
;;;                nameless-proc-exp (body var-pos)
;;; Expression ::= %lexproc-inline Expression
;;;                nameless-inline-proc-exp (body)
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
    (expression (identifier)
                var-exp)
    (expression ("-" "(" expression "," expression ")")
                diff-exp)
    (expression ("zero?" "(" expression ")")
                zero?-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression ("let" identifier "=" expression "in" expression)
                let-exp)
    (expression ("proc" "(" identifier ")" expression)
                proc-exp)
    (expression ("(" expression expression ")")
                call-exp)
    (expression ("%lexref" number)
                nameless-var-exp)
    (expression ("%let" expression "in" expression)
                nameless-let-exp)
    (expression ("%lexproc" expression "in" "(" (arbno number) ")")
                nameless-proc-exp)
    (expression ("%lexproc-inline" expression)
                nameless-inline-proc-exp)
    (expression ("%void")
                void-exp)))

;;; ---------------------- Extract Free Variables ----------------------
;; member-var? : Identifier x Senv -> Boolean
(define member-var?
  (lambda (var senv)
    (cond [(empty-senv? senv) #f]
          [(eqv? var (caar senv)) #t]
          [else
           (member-var? var (cdr senv))])))
;; extract-free-vars : Senv x Exp x Listof(Symbol) -> Senv
(define extract-free-vars
  (lambda (senv exp vars)
    (let ((free-vars (extract-free-vars-iter senv exp vars 0 (cons '() '()))))
      (cons (reverse (car free-vars))
            (reverse (cdr free-vars))))))
;; extract-free-vars-iter : Senv x Exp x Listof(Symbol) x Num x (Senv . Listof(number)) -> (Senv . Listof(number))
(define extract-free-vars-iter
  (lambda (senv exp vars pos trimmed-senv)
    (cond [(empty-senv? senv) trimmed-senv]
          [(member-var? (caar senv) (car trimmed-senv))
           (extract-free-vars-iter (cdr senv) exp vars (+ pos 1) trimmed-senv)]
          [(and (not (member? (caar senv) vars))
                (occurs-free? (caar senv) exp))
           (extract-free-vars-iter
            (cdr senv)
            exp
            vars
            (+ pos 1)
            (cons (cons (car senv) (car trimmed-senv))
                  (cons pos (cdr trimmed-senv))))]
          [else (extract-free-vars-iter (cdr senv) exp vars (+ pos 1) trimmed-senv)])))
;; occurs-free? : Var x Exp -> Boolean
(define occurs-free?
  (lambda (var exp)
    (cases expression exp
           (const-exp
            (num)
            #f)
           (var-exp
            (var1)
            (eqv? var var1))
           (diff-exp
            (exp1 exp2)
            (or (occurs-free? var exp1)
                (occurs-free? var exp2)))
           (zero?-exp
            (exp1)
            (occurs-free? var exp1))
           (if-exp
            (exp1 exp2 exp3)
            (or (occurs-free? var exp1)
                (occurs-free? var exp2)
                (occurs-free? var exp3)))
           (let-exp
            (let-var let-val body)
            (and (not (equal? let-var var))
                 (occurs-free? var body)))
           (proc-exp
            (b-var body)
            (and (not (eqv? var b-var))
                 (occurs-free? var body)))
           (call-exp
            (rator rand)
            (or (occurs-free? var rator)
                (occurs-free? var rand)))
           (else
            (report-invalid-source-expression exp)))))
;; no-free-vars? : Exp x Num -> Boolean
(define no-free-vars?
  (lambda (body param-num)
    (cases expression body
           (const-exp
            (num)
            #t)
           (nameless-var-exp
            (addr)
            (< addr param-num))
           (diff-exp
            (exp1 exp2)
            (and (no-free-vars? exp1 param-num)
                 (no-free-vars? exp2 param-num)))
           (zero?-exp
            (exp1)
            (no-free-vars? exp1 param-num))
           (if-exp
            (exp1 exp2 exp3)
            (and (no-free-vars? exp1 param-num)
                 (no-free-vars? exp2 param-num)
                 (no-free-vars? exp3 param-num)))
           (nameless-let-exp
            (exp1 body)
            (and (no-free-vars? exp1 param-num)
                 (no-free-vars? body param-num)))
           (nameless-proc-exp
            (body var-pos)
            #f)
           (nameless-inline-proc-exp
            (body)
            #t)
           (call-exp
            (rator rand)
            (and (no-free-vars? rator param-num)
                 (no-free-vars? rand param-num)))
           (else
            (report-invalid-tranlated-expression exp)))))

;;; ------------ Static Environment(from section 3.7) ----------------
;; Senv = Listof(Sym)
;; Lexaddr = N
;; empty-senv : () -> Senv
(define empty-senv
  (lambda () '()))
;; empty-senv? : () -> Boolean
(define empty-senv? null?)
;; extend-senv : Var x Exp x Senv -> Senv
(define extend-senv
  (lambda (var exp senv)
    (cons (cons var exp) senv)))
;; apply-senv : Senv x Var -> Lexaddr
(define apply-senv
  (lambda (senv var)
    (cond ((null? senv)
           (report-no-binding-found var))
          ((eqv? var (caar senv))
           0)
          (else
           (+ 1 (apply-senv (cdr senv) var))))))

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
           (diff-exp
            (exp1 exp2)
            (diff-exp
             (translation-of exp1 senv)
             (translation-of exp2 senv)))
           (zero?-exp
            (exp1)
            (zero?-exp (translation-of exp1 senv)))
           (if-exp
            (exp1 exp2 exp3)
            (if-exp (translation-of exp1 senv)
                    (translation-of exp2 senv)
                    (translation-of exp3 senv)))
           (var-exp
            (var)
            (let* ((addr (apply-senv senv var))
                   (val-exp (cdr (list-ref senv addr))))
              (cases expression val-exp
                     (void-exp
                      ()
                      (nameless-var-exp addr))
                     (nameless-proc-exp
                      (body var-pos)
                      (nameless-var-exp addr))
                     (else
                      val-exp))))
           (let-exp
            (var exp1 body)
            (let* ((val-exp (translation-of exp1 senv))
                   (let-body (translation-of body (extend-senv var val-exp senv))))
              (cases expression val-exp
                     (nameless-inline-proc-exp
                      (body)
                      let-body)
                     (else
                      (nameless-let-exp val-exp let-body)))))
           (proc-exp
            (var body)
            (let* ((free-vars (extract-free-vars senv body (list var)))
                   (saved-env (car free-vars))
                   (var-pos (cdr free-vars))
                   (translated-body (translation-of
                                     body
                                     (extend-senv var (void-exp) saved-env))))
              (if (no-free-vars? translated-body 1)
                  (nameless-inline-proc-exp translated-body)
                  (nameless-proc-exp translated-body var-pos))))
           (call-exp
            (rator rand)
            (call-exp
             (translation-of rator senv)
             (translation-of rand senv)))
           (else
            (report-invalid-source-expression exp)))))
;; translation-of : () -> Senv
(define init-senv
  (lambda () (empty-senv)))
(define report-invalid-source-expression
  (lambda (exp)
    (eopl:error
     'expression
     "Not a valid source exp ~a" exp)))

;;; ------------- Nameless Environment(from section 3.7) -------------
;; nameless-environment? : SchemeVal -> Bool
(define nameless-environment?
  (lambda (x)
    ((list-of exp-val?) x)))
;; empty-nameless-env? : Nameless-env -> Boolean
(define empty-nameless-env? null?)
;; empty-nameless-env : () -> Nameless-env
(define empty-nameless-env
  (lambda () '()))
;; extend-nameless-env : Expval x Nameless-env -> Nameless-env
(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)))
;; apply-nameless-env : Nameless-env x Lexaddr -> DenVal
(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)))

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
(define extract-free-nameless-vars
  (lambda (nameless-env positions)
    (map (lambda (pos) (list-ref nameless-env pos)) positions)))
(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
           (const-exp
            (num)
            (num-val num))
           (diff-exp
            (exp1 exp2)
            (num-val (- (expval->num (value-of exp1 nameless-env))
                        (expval->num (value-of exp2 nameless-env)))))
           (zero?-exp
            (exp1)
            (bool-val (eqv? (expval->num (value-of exp1 nameless-env)) 0)))
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
           (nameless-var-exp
            (n)
            (apply-nameless-env nameless-env n))
           (nameless-let-exp
            (exp1 body)
            (let ((val (value-of exp1 nameless-env)))
              (value-of body (extend-nameless-env val nameless-env))))
           (nameless-proc-exp
            (body var-pos)
            (proc-val (procedure body (extract-free-nameless-vars nameless-env var-pos))))
           (nameless-inline-proc-exp
            (body)
            (proc-val (procedure body (empty-nameless-env))))
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
                     (proc-val
                      (val)
                      val)))))))
(define report-invalid-tranlated-expression
  (lambda (exp)
    (eopl:error
     'expression
     "Not a valid translated exp ~a" exp)))

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
(equal?
 (run "let x = 37
        in proc (y)
           let z = -(y, x)
           in -(x, y)")
 (procedure (nameless-let-exp
             (diff-exp (nameless-var-exp 0)
                       (const-exp 37))
             (diff-exp (const-exp 37)
                       (nameless-var-exp 1)))
            (list (num-val 37))))
(eqv?
 (run "let x = 200 in
        let f = proc (z) -(z,x) in
          let x = 100 in
            let g = proc (z) -(z,x) in
              -((f 1), (g 1))")
 -100)
(eqv?
 (run "let z = 42 in
        let x = 36 in
          let z = 53 in
            let y = 33 in
              let f = proc (x) -(x, z) in
                (f z)")
 0)
(eqv?
 (run "let x = 3 in
         let f = proc (y) -(y,x) in
           (f 13)")
 10)
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

;; exer 3.20
(eqv?
 (run "((proc (x) proc (y) -(y,-(0,x)) 3) 4)")
 7)

;; exer 3.23
(eqv?
 (run "let makemult = proc (maker)
                       proc (x)
                         if zero? (x)
                         then 0
                         else -(((maker maker) -(x, 1)), -4)
       in let times4 = proc (x) ((makemult makemult) x)
          in (times4 3)")
 12)
(eqv?
 (run "let makemult = proc (maker)
                       proc (x)
                         proc (y)
                           if zero? (x) then 0
                           else -((((maker maker) -(x, 1)) y), -(0, y))
       in let times4 = proc (x) (((makemult makemult) x) 4)
         in (times4 3)")
 12)
(eqv?
 (run "let makemult = proc (maker)
                       proc (x)
                         proc (y)
                           if zero? (x) then 0
                           else -((((maker maker) -(x, 1)) y), -(0, y))
      in let times = proc (z) proc (x) (((makemult makemult) x) z)
         in ((times 3) 4)")
 12)
(eqv?
 (run "let makemult = proc (maker)
                       proc (x)
                         proc (y)
                           if zero? (x) then 0
                           else -((((maker maker) -(x, 1)) y), -(0, y))
      in let times = proc (z) proc (x) (((makemult makemult) x) z)
         in let makefact = proc (maker)
                         proc (x)
                           if zero? (x) then 1
                           else ((times ((maker maker) -(x, 1))) x)
            in let fact = proc (x) ((makefact makefact) x)
               in (fact 4)")
 24)

;; exer 3.24
(eqv?
 (run "let makeodd = proc (maker)
                      proc (maker2)
                        proc (x)
                          if zero? (x) then 0
                          else (((maker maker2) maker) -(x, 1))
      in let makeeven = proc (maker)
                          proc (maker2)
                            proc (x)
                              if zero? (x) then 1
                              else (((maker maker2) maker) -(x, 1))
         in let odd = proc (x) (((makeodd makeeven) makeodd) x)
            in (odd 5)")
 1)
(eqv?
 (run "let makeodd = proc (maker)
                      proc (maker2)
                        proc (x)
                          if zero? (x) then 0
                          else (((maker maker2) maker) -(x, 1))
       in let makeeven = proc (maker)
                          proc (maker2)
                            proc (x)
                              if zero? (x) then 1
                              else (((maker maker2) maker) -(x, 1))
          in let even = proc (x) (((makeeven makeodd) makeeven) x)
             in (even 5)")
 0)

;; exer 3.25
(eqv?
 (run "let makerec = proc (f)
        let d = proc (x)
                  proc (z) ((f (x x)) z)
        in proc (n) ((f (d d)) n)
       in let maketimes4 = proc (f)
                            proc (x)
                              if zero?(x) then 0
                              else -((f -(x,1)), -4)
          in let times4 = (makerec maketimes4)
             in (times4 3)")
 12)

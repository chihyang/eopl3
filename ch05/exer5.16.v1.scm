#lang eopl
;;; ---------------------- Environment (from section 3.2) ----------------------
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

(define-datatype environment environment?
  (empty-env-inner)
  (extend-env-inner
   (vars (list-of identifier?))
   (vals vector?)
   (env environment?)))
;; empty-env : () -> Env
(define empty-env empty-env-inner)
;; extend-env : Id x ExpVal x Env -> Env
(define extend-env
  (lambda (var val env)
    (extend-env-inner (list var) (make-vector 1 val) env)))
;; extend-env* : Listof(Id) x Listof(ExpVal) x Env -> Env
(define extend-env*
  (lambda (vars vals env)
    (let ((dup (check-duplicates vars))
          (var-len (length vars))
          (val-len (length vals)))
      (cond [(not (null? dup)) (report-duplicate-id dup)]
            [(< var-len val-len)
             (report-argument-mismatch 'greater)]
            [(> var-len val-len)
             (report-argument-mismatch 'less)]
            [else
             (extend-env-inner vars (list->vector vals) env)]))))
(define extend-env-rec
  (lambda (p-names b-vars bodies saved-env)
    (let ((vec (make-vector (length p-names))))
      (let ((new-env (extend-env-inner p-names vec saved-env)))
        (make-proc-vec! vec 0 b-vars bodies new-env)
        new-env))))
(define make-proc-vec!
  (lambda (vec n b-vars bodies env)
    (cond ([null? b-vars] vec)
          (else
           (vector-set!
            vec n (newref (proc-val (procedure (car b-vars) (car bodies) env))))
           (make-proc-vec! vec (+ n 1) (cdr b-vars) (cdr bodies) env)))))
(define apply-env
  (lambda (env search-var)
    (cases environment env
           (empty-env-inner
            ()
            (report-no-binding-found search-var))
           (extend-env-inner
            (saved-vars saved-vals saved-env)
            (let ((val (search-val saved-vars saved-vals search-var)))
              (if (null? val)
                  (apply-env saved-env search-var)
                  val))))))
(define search-val
  (lambda (saved-vars saved-vals search-var)
    (search-from-vector-vals saved-vars saved-vals search-var 0)))
(define search-from-vector-vals
  (lambda (saved-vars saved-vals search-var n)
    (cond ((null? saved-vars)
           '())
          ((eqv? search-var (car saved-vars))
           (vector-ref saved-vals n))
          (else
           (search-from-vector-vals
            (cdr saved-vars) saved-vals search-var (+ n 1))))))

;; init-env : () → Env
;; usage: (init-env) = [i= ^1^, v= ^5^, x= ^10^]
(define init-env
  (lambda ()
    (extend-env
     'i (newref (num-val 1))
     (extend-env
      'v (newref (num-val 5))
      (extend-env
       'x (newref (num-val 10))
       (empty-env))))))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))
(define report-argument-mismatch
  (lambda (symp)
    (eopl:error 'extend-env* "Argument number is ~s than parameter number" symp)))
(define report-duplicate-id
  (lambda (sym)
    (eopl:error 'extend-env* "Duplicate identifier ~s" sym)))

;;; ---------------------- Continuation ----------------------
;; FinalAnswer = ExpVal
;; Cont = ExpVal -> FinalAnswer
;; end-cont : () -> Cont
(define-datatype continuation continuation?
  (zero1-cont
   (cont continuation?))
  (not-cont
   (cont continuation?))
  (if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (env environment?)
   (cont continuation?))
  (diff1-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (diff2-cont
   (val1 exp-val?)
   (cont continuation?))
  (add1-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (add2-cont
   (val1 exp-val?)
   (cont continuation?))
  (mul1-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (mul2-cont
   (val1 exp-val?)
   (cont continuation?))
  (quot1-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (quot2-cont
   (val1 exp-val?)
   (cont continuation?))
  (rator-cont
   (exps (list-of expression?))
   (env environment?)
   (cont continuation?))
  (rand-cont
   (val exp-val?)
   (saved-rands (list-of exp-val?))
   (cont-exps (list-of expression?))
   (env environment?)
   (cont continuation?))
  (let-cont
   (saved-vars (list-of identifier?))
   (saved-vals (list-of exp-val?))
   (cont-vars (list-of identifier?))
   (cont-exps (list-of expression?))
   (body expression?)
   (env environment?)
   (cont continuation?))
  (cons1-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (cons2-cont
   (val1 exp-val?)
   (cont continuation?))
  (car-cont
   (cont continuation?))
  (cdr-cont
   (cont continuation?))
  (null1-cont
   (cont continuation?))
  (list1-cont
   (exps expression?)
   (env environment?)
   (cont continuation?))
  (list2-cont
   (first-val exp-val?)
   (cont continuation?))
  (set-rhs-cont
   (ref reference?)
   (cont continuation?))
  (begin-exp-cont
   (exp expression?)
   (env environment?)
   (cont continuation?))
  (set-rhs-stmt-cont
   (ref reference?)
   (cont command-cont?))
  (print-stmt-cont
   (cont command-cont?))
  (if-stmt-cont
   (stmt1 statement?)
   (stmt2 statement?)
   (env environment?)
   (cont command-cont?))
  (while-stmt-cont
   (exp expression?)
   (stmt statement?)
   (env environment?)
   (cont command-cont?)))
(define-datatype command-cont command-cont?
  (end-command-cont)
  (multi-command-cont
   (stmts (list-of statement?))
   (env environment?)
   (cont command-cont?))
  (while-command-cont
   (exp expression?)
   (stmt statement?)
   (env environment?)
   (cont command-cont?)))
;; apply-cont : Cont x ExpVal -> Unspecified
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
           (zero1-cont
            (cont)
            (apply-cont cont (bool-val (zero? (expval->num val)))))
           (not-cont
            (cont)
            (apply-cont cont (bool-val (not (expval->bool val)))))
           (if-test-cont
            (exp2 exp3 env cont)
            (if (expval->bool val)
                (value-of/k exp2 env cont)
                (value-of/k exp3 env cont)))
           (diff1-cont
            (exp2 env cont)
            (value-of/k exp2 env (diff2-cont val cont)))
           (diff2-cont
            (val1 cont)
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val)))
              (apply-cont cont (num-val (- num1 num2)))))
           (add1-cont
            (exp2 env cont)
            (value-of/k exp2 env (add2-cont val cont)))
           (add2-cont
            (val1 cont)
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val)))
              (apply-cont cont (num-val (+ num1 num2)))))
           (mul1-cont
            (exp2 env cont)
            (value-of/k exp2 env (mul2-cont val cont)))
           (mul2-cont
            (val1 cont)
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val)))
              (apply-cont cont (num-val (* num1 num2)))))
           (quot1-cont
            (exp2 env cont)
            (value-of/k exp2 env (quot2-cont val cont)))
           (quot2-cont
            (val1 cont)
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val)))
              (apply-cont cont (num-val (/ num1 num2)))))
           (rator-cont
            (exps env cont)
            (if (null? exps)
                (apply-procedure/k (expval->proc val) '() cont)
                (value-of/k (car exps) env (rand-cont val '() (cdr exps) env cont))))
           (rand-cont
            (rator saved-vals cont-exps env cont)
            (if (null? cont-exps)
                (let ((proc1 (expval->proc rator)))
                  (apply-procedure/k proc1 (reverse (cons val saved-vals)) cont))
                (value-of/k (car cont-exps)
                            env
                            (rand-cont rator (cons val saved-vals) (cdr cont-exps) env cont))))
           (let-cont
            (saved-vars saved-vals cont-vars cont-exps body env cont)
            (if (null? cont-exps)
                (let ((l-vars (reverse (cons (car cont-vars) saved-vars)))
                      (l-vals (reverse (cons val saved-vals))))
                  (value-of/k body
                              (extend-env* l-vars
                                           (map newref l-vals)
                                           env)
                              cont))
                (value-of/k (car cont-exps)
                            env
                            (let-cont (cons (car cont-vars) saved-vars)
                                      (cons val saved-vals)
                                      (cdr cont-vars)
                                      (cdr cont-exps)
                                      body
                                      env
                                      cont))))
           (cons1-cont
            (exp2 env cont)
            (value-of/k exp2 env (cons2-cont val cont)))
           (cons2-cont
            (val1 cont)
            (apply-cont cont (pair-val val1 val)))
           (car-cont
            (cont)
            (cases exp-val val
                   (pair-val
                    (first rest)
                    (apply-cont cont first))
                   (else (report-invalid-exp-value 'pair-val))))
           (cdr-cont
            (cont)
            (cases exp-val val
                   (pair-val
                    (first rest)
                    (apply-cont cont rest))
                   (else (report-invalid-exp-value 'pair-val))))
           (null1-cont
            (cont)
            (cases exp-val val
                   (null-val
                    ()
                    (apply-cont cont (bool-val #t)))
                   (else (apply-cont cont (bool-val #f)))))
           (list1-cont
            (exps env cont)
            (value-of/k exps env (list2-cont val cont)))
           (list2-cont
            (first-val cont)
            (apply-cont cont (pair-val first-val val)))
           (set-rhs-cont
            (ref cont)
            (begin
              (setref! ref val)
              (apply-cont cont (num-val 27))))
           (begin-exp-cont
            (exp env cont)
            (value-of/k exp env cont))
           (set-rhs-stmt-cont
            (ref cont1)
            (begin
              (setref! ref val)
              (apply-command-cont cont1)))
           (print-stmt-cont
            (cont1)
            (begin
              (cases exp-val val
                     (num-val
                      (num)
                      (eopl:pretty-print num))
                     (bool-val
                      (bool)
                      (eopl:pretty-print bool))
                     (null-val
                      ()
                      (eopl:pretty-print '()))
                     (pair-val
                      (val1 val2)
                      (eopl:pretty-print (expval->pair val)))
                     (proc-val
                      (val)
                      (eopl:pretty-print val)))
              (apply-command-cont cont1)))
           (if-stmt-cont
            (stmt1 stmt2 env cont1)
            (if (expval->bool val)
                (result-of/k stmt1 env cont1)
                (result-of/k stmt2 env cont1)))
           (while-stmt-cont
            (exp stmt env cont1)
            (if (expval->bool val)
                (result-of/k stmt env (while-command-cont exp stmt env cont1))
                (apply-command-cont cont1))))))
;; apply-command-cont : CmdCont -> Unspecified
(define apply-command-cont
  (lambda (cont)
    (cases command-cont cont
           (end-command-cont
            ()
            (begin
              (eopl:printf "End of computation.~%")))
           (multi-command-cont
            (stmts env cont)
            (if (null? stmts)
                (apply-command-cont cont)
                (result-of/k (car stmts) env (multi-command-cont (cdr stmts) env cont))))
           (while-command-cont
            (exp stmt env cont1)
            (value-of/k exp env (while-stmt-cont exp stmt env cont1))))))
;; report-invalid-cont : SchemeVal x Symbol -> Unspecified
(define report-invalid-cont
  (lambda (cont type)
    (eopl:error
     'apply-cont
     "Not a valid ~s continuation: ~s" type cont)))

;;; ---------------------- Expval ----------------------
(define identifier? symbol?)
(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body expression?)
   (saved-env environment?)))
;; apply-procedure/k : Proc x Listof(ExpVal) x Cont -> ExpVal
(define apply-procedure/k
  (lambda (proc1 vals cont)
    (cases proc proc1
           (procedure
            (vars body saved-env)
            (value-of/k body (extend-env* vars (map newref vals) saved-env) cont)))))
(define-datatype exp-val exp-val?
  (num-val
   (val number?))
  (bool-val
   (val boolean?))
  (proc-val
   (val proc?))
  (null-val)
  (pair-val
   (val1 exp-val?)
   (val2 exp-val?)))
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
                    (proc-val (proc) proc)
                    (pair-val (val3 val4) (expval->pair val1)))
             (cases exp-val val2
                    (num-val (num) num)
                    (bool-val (bool) bool)
                    (null-val () '())
                    (proc-val (proc) proc)
                    (pair-val (val3 val4) (expval->pair val2)))))
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
     "Not a valid exp value of type ~s" type)))

;;; ---------------------- Store (from section 4.2) ----------------------
;; empty-store : () -> Sto
(define empty-store (lambda () '()))
;; usage : A scheme variable containing the current state of the
;; store. Initially set to a dummy value.
(define the-store 'uninitialized)
;; get-store : () -> Sto
(define get-store
  (lambda () the-store))
;; initialize-store! : () -> Unspecified
;; usage : (initialize-store!) sets the store to the empty store
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))
;; reference? : SchemeVal -> Bool
(define reference?
  (lambda (v)
    (integer? v)))
;; newref : ExpVal | Uninitialized -> Ref
(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))
;; deref : Ref -> ExpVal
(define deref
  (lambda (ref)
    (let ((val (list-ref the-store ref)))
      (if (eqv? val 'uninitialized)
          (report-uninitialized-reference ref the-store)
          val))))
;; setref! : Ref x ExpVal -> Unspecified
;; usage : sets the-store to a state like the original, but with position ref
;; containing val
(define setref!
  (lambda (ref val)
    (set! the-store
      (letrec ((setref-inner
                ;; usage : returns a list like store1, except that position ref1
                ;; contains val.
                (lambda (store1 ref1)
                  (cond [(null? store1)
                         (report-invalid-reference ref the-store)]
                        [(zero? ref1)
                         (cons val (cdr store1))]
                        [else
                         (cons (car store1)
                               (setref-inner (cdr store1) (- ref1 1)))]))))
        (setref-inner the-store ref)))))
(define report-invalid-reference
  (lambda (ref store)
    (eopl:error
     'exp-val
     "Not a valid reference ~a for store ~a" ref store)))
(define report-uninitialized-reference
  (lambda (ref store)
    (eopl:error
     'deref
     "Reference ~a is not initialized in store ~a" ref store)))

;;; ---------------------- Syntax for the STATEMENT language ----------------------
;;; Program    ::= Statement
;;;                a-program (stmt1)
;;; Statement  ::= Identifier = Expression
;;;                assign-stmt (var exp1)
;;; Statement  ::= print Expression
;;;                print-stmt (exp1)
;;; Statement  ::= { Statement*{;} }
;;;                multi-stmt (stmts)
;;; Statement  ::= if Expression Statement Statement
;;;                if-stmt (exp1 stmt1 stmt2)
;;; Statement  ::= while Expression Statement
;;;                while-stmt (exp1 body)
;;; Statement  ::= do Statement while Expression
;;;                do-while-stmt (exp1 body)
;;; Statement  ::= var Identifier*{;} Statement
;;;                block-stmt (vars stmt)
;;; Statement  ::= read Identifier
;;;                read-stmt (var)
;;; Expression ::= Number
;;;                const-exp (num)
;;; Expression ::= Identifier
;;;                var-exp (var)
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
;;;                mul-exp (exp1 exp2)
;;; Expression ::= / (Expression , Expression)
;;;                quoti-exp (exp1 exp2)
;;; Expression ::= zero? (Expression)
;;;                zero?-exp (exp1)
;;; Expression ::= not (Expression)
;;;                not-exp (exp1)
;;; Expression ::= if Expression then Expression else Expression
;;;                if-exp (exp1 exp2 exp3)
;;; Expression ::= let {Identifier = Expression}* in Expression
;;;                let-exp (var exp1 body)
;;; Expression ::= proc (Identifier*,) Expression
;;;                proc-exp (var body)
;;; Expression ::= letrec {Identifier (Identifier*,)}* = Expression in Expression
;;;                letrec-exp (p-name b-var p-exp1 letrec-body)
;;; Expression ::= (Expression Expression*)
;;;                call-exp (rator rand)
;;; Expression ::= begin Expression {; Expression}* end
;;;                begin-exp (exp1 exps)
;;; Expression ::= set Identifier = Expression
;;;                assign-exp (var exp1)
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
  '((program (statement) a-program)
    (statement (identifier "=" expression)
               assign-stmt)
    (statement ("print" expression)
               print-stmt)
    (statement ("{" (separated-list statement ";") "}")
               multi-stmt)
    (statement ("if" expression statement statement)
               if-stmt)
    (statement ("while" expression statement)
               while-stmt)
    (statement ("do" statement "while" expression)
               do-while-stmt)
    (statement ("var" (separated-list identifier ",") ";" statement)
               block-stmt)
    (statement ("read" identifier)
               read-stmt)
    (expression (number)
                const-exp)
    (expression (identifier)
                var-exp)
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
    (expression ("zero?" "(" expression ")")
                zero?-exp)
    (expression ("not" "(" expression ")")
                not-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
                letrec-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression)
                proc-exp)
    (expression ("(" expression (arbno expression) ")")
                call-exp)
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)
    (expression ("set" identifier "=" expression)
                assign-exp)))

;;; ---------------------- Evaluate expression ----------------------
;; value-of/k : Exp x Env x Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
           (const-exp
            (num)
            (apply-cont cont (num-val num)))
           (var-exp
            (var)
            (apply-cont cont (deref (apply-env env var))))
           (emptylist-exp
            ()
            (apply-cont cont (null-val)))
           (cons-exp
            (exp1 exp2)
            (value-of/k exp1 env (cons1-cont exp2 env cont)))
           (car-exp
            (exp1)
            (value-of/k exp1 env (car-cont cont)))
           (cdr-exp
            (exp1)
            (value-of/k exp1 env (cdr-cont cont)))
           (null?-exp
            (exp1)
            (value-of/k exp1 env (null1-cont cont)))
           (list-exp
            (exps)
            (if (null? exps)
                (apply-cont cont (null-val))
                (value-of/k (car exps) env (list1-cont (list-exp (cdr exps)) env cont))))
           (diff-exp
            (exp1 exp2)
            (value-of/k exp1 env (diff1-cont exp2 env cont)))
           (add-exp
            (exp1 exp2)
            (value-of/k exp1 env (add1-cont exp2 env cont)))
           (mul-exp
            (exp1 exp2)
            (value-of/k exp1 env (mul1-cont exp2 env cont)))
           (quot-exp
            (exp1 exp2)
            (value-of/k exp1 env (quot1-cont exp2 env cont)))
           (zero?-exp
            (exp1)
            (value-of/k exp1 env (zero1-cont cont)))
           (not-exp
            (exp1)
            (value-of/k exp1 env (not-cont cont)))
           (if-exp
            (exp1 exp2 exp3)
            (value-of/k exp1 env (if-test-cont exp2 exp3 env cont)))
           (let-exp
            (vars exps body)
            (value-of/k (car exps) env (let-cont '() '()  vars (cdr exps) body env cont)))
           (letrec-exp
            (p-names p-vars p-bodies letrec-body)
            (value-of/k letrec-body (extend-env-rec p-names p-vars p-bodies env) cont))
           (proc-exp
            (vars body)
            (apply-cont cont (proc-val (procedure vars body env))))
           (call-exp
            (rator rand)
            (value-of/k rator env (rator-cont rand env cont)))
           (begin-exp
            (exp1 exps)
            (if (null? exps)
                (value-of/k exp1 env cont)
                (value-of/k exp1 env (begin-exp-cont (begin-exp (car exps) (cdr exps)) env cont))))
           (assign-exp
            (var exp1)
            (let ((ref (apply-env env var)))
              (value-of/k exp1 env (set-rhs-cont ref cont)))))))
;; result-of/k : Statement x Env x Command-Cont -> Sto
(define result-of/k
  (lambda (stmt env cont)
    (cases statement stmt
           (assign-stmt
            (var exp)
            (let ((ref (apply-env env var)))
              (value-of/k exp env (set-rhs-stmt-cont ref cont))))
           (print-stmt
            (exp1)
            (value-of/k exp1 env (print-stmt-cont cont)))
           (multi-stmt
            (stmts)
            (if (null? stmts)
                (apply-command-cont cont)
                (result-of/k (car stmts) env (multi-command-cont (cdr stmts) env cont))))
           (if-stmt
            (exp1 stmt1 stmt2)
            (value-of/k exp1 env (if-stmt-cont stmt1 stmt2 env cont)))
           (while-stmt
            (exp1 body)
            (value-of/k exp1 env (while-stmt-cont exp1 body env cont)))
           (do-while-stmt
            (body exp1)
            (result-of/k body env (while-command-cont exp1 body env cont)))
           (block-stmt
            (vars body)
            (if (null? vars)
                (result-of/k body env cont)
                (result-of/k body
                           (extend-env* vars
                                        (map (lambda (var) (newref 'uninitialized))
                                             vars)
                                        env)
                           cont)))
           (read-stmt
            (var)
            (let ((ref (apply-env env var))
                  (val (get-val-from-input)))
              (begin
                (setref! ref (num-val val))
                (apply-command-cont cont)))))))

;; value-of-program : Program -> SchemeVal
(define value-of-program
  (lambda (prog)
    (initialize-store!)
    (cases program prog
           (a-program
            (stmt)
            (result-of/k stmt (empty-env) (end-command-cont))))))

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
(define run
  (lambda (exp)
    (value-of-program (scan&parse exp))))
;; get-val-from-input : () -> Int
;;
;; Usage : get a constant number from a string to make tests easier
(define get-val-from-input
  (lambda ()
    (string->number "87")))

;;; ---------------------- Test ----------------------
;; tests from exercise 4.22
(run "var x, y; {x = 3; y = 4; print +(x, y) }")
(run "var x,y,z;
      {x = 3; y = 4; z = 0;
       while not(zero?(x))
         {z = +(z,y); x = -(x,1)};
       print z}")
(run "var x;
      { x = 3;
        print x;
        var x; { x = 4; print x };
        print x }")
(run "var f, x; { f = proc(x,y) *(x, y);
                  x = 3;
                  print (f 4 x)}")

;; tests from exercise 4.23
(run "var f, x; { f = proc(x,y) *(x, y);
                  read x;
                  print (f 4 x)}")

;; tests from exercise 4.24
(run "var x,y,z;
      {x = 3; y = 4; z = 0;
       do
         {x = -(x,1); z = +(z,x)}
       while not(zero?(x));
       print z}")

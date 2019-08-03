#lang eopl
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

;;; ---------------------- Syntax for the CPS-IN language ----------------------
;;; Program     ::= Expression
;;;                 a-program (exp1)
;;; Expression  ::= Number
;;;                 const-exp (num)
;;; Expression  ::= Identifier
;;;                 var-exp (var)
;;; Expression  ::= + (Expression{,}*)
;;;                 sum-exp (exps)
;;; Expression  ::= - (Expression, Expression)
;;;                 diff-exp (exp1, exp2)
;;; Expression  ::= zero? (Expression)
;;;                 zero?-exp (exp1)
;;; Expression  ::= Expression
;;;                simple-exp->exp (simple-exp1)
;;; Expression  ::= if Expression then Expression else Expression
;;;                 if-exp (exp1 exp2 exp3)
;;; Expression  ::= let {Identifier = Expression}* in Expression
;;;                 let-exp (vars exps body)
;;; Expression  ::= letrec {Identifier (Identifier*,) = Expression}* in Expression
;;;                 letrec-exp (p-names b-vars p-bodies body)
;;; Expression  ::= (Expression Expression*)
;;;                 call-exp (rator rands)
;;; Parse Expression
(define let-scanner-spec
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    (number ((or (concat digit (arbno digit))
                 (concat "-" digit (arbno digit))
                 (concat (arbno digit) "." digit (arbno digit))
                 (concat "-" (arbno digit) "." digit (arbno digit))
                 (concat digit (arbno digit) "." (arbno digit))
                 (concat "-" digit (arbno digit) "." (arbno digit)))) number)
    (comment ("%" (arbno (not #\newline))) skip)))
(define let-grammar
  '((program (expression) a-program)
    (expression (number)
                const-exp)
    (expression (identifier)
                var-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression)
                proc-exp)
    (expression ("+" "(" (separated-list expression ",") ")")
                sum-exp)
    (expression ("-" "(" expression "," expression ")")
                diff-exp)
    (expression ("zero?" "(" expression ")")
                zero?-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
                letrec-exp)
    (expression ("(" expression (arbno expression) ")")
                call-exp)))

(sllgen:make-define-datatypes let-scanner-spec let-grammar)
(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes let-scanner-spec let-grammar)))
(define just-scan
  (sllgen:make-string-scanner let-scanner-spec let-grammar))
(define scan&parse
  (sllgen:make-string-parser let-scanner-spec let-grammar))

(define unparse-exp
  (lambda (exp)
    (cases expression exp
           (const-exp (num) num)
           (var-exp (var) var)
           (proc-exp
            (vars body)
            `(lambda ,vars ,(unparse-exp body)))
           (sum-exp
            (exps)
            (append '(+) (map unparse-exp exps)))
           (diff-exp
            (exp1 exp2)
            `(- ,(unparse-exp exp1) ,(unparse-exp exp2)))
           (zero?-exp
            (exp1)
            `(zero? ,(unparse-exp exp1)))
           (if-exp
            (exp1 exp2 exp3)
            `(if ,(unparse-exp exp1) ,(unparse-exp exp2) ,(unparse-exp exp3)))
           (let-exp
            (vars exps body)
            `(let ,(map (lambda (v1 v2) (list v1 (unparse-exp v2)))
                        vars exps)
               ,(unparse-exp body)))
           (letrec-exp
            (p-names p-vars p-bodies body)
            `(letrec ,(map (lambda (v1 v2 v3)
                             (list v1
                                   `(lambda ,v2
                                      ,(unparse-exp v3))))
                        p-names p-vars p-bodies)
               ,(unparse-exp body)))
           (call-exp
            (rator rands)
            (map unparse-exp (cons rator rands))))))

(define unparse-prgm
  (lambda (prgm)
    (cases program prgm
           (a-program
            (exp)
            (unparse-exp exp)))))

;;; ---------------------- Syntax for the CPS-OUT language ----------------------
;;; TfProgram  ::= TfExp
;;;                cps-a-program (exp1)
;;; SimpleExp  ::= Number
;;;                cps-const-exp (num)
;;; SimpleExp  ::= Identifier
;;;                cps-var-exp (var)
;;; SimpleExp  ::= + (SimpleExp{,}*)
;;;                cps-sum-exp (exps)
;;; SimpleExp  ::= - (SimpleExp, SimpleExp)
;;;                cps-diff-exp (exp1, exp2)
;;; SimpleExp  ::= zero? (SimpleExp)
;;;                cps-zero?-exp (exp1)
;;; TfExp      ::= SimpleExp
;;;                simple-exp->exp (simple-exp1)
;;; TfExp      ::= if SimpleExp then TfExp else TfExp
;;;                cps-if-exp (exp1 exp2 exp3)
;;; TfExp      ::= let {Identifier = SimpleExp}* in TfExp
;;;                cps-let-exp (vars exps body)
;;; TfExp      ::= letrec {Identifier (Identifier*,) = TfExp}* in TfExp
;;;                cps-letrec-exp (p-names b-vars p-bodies body)
;;; TfExp      ::= (SimpleExp SimpleExp*)
;;;                cps-call-exp (rator rands)
;;; Parse Expression
(define cps-scanner-spec
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    (number ((or (concat digit (arbno digit))
                 (concat "-" digit (arbno digit))
                 (concat (arbno digit) "." digit (arbno digit))
                 (concat "-" (arbno digit) "." digit (arbno digit))
                 (concat digit (arbno digit) "." (arbno digit))
                 (concat "-" digit (arbno digit) "." (arbno digit)))) number)
    (comment ("%" (arbno (not #\newline))) skip)))
(define cps-grammar
  '((cps-program (tf-exp) cps-a-program)
    (simple-exp (number)
                cps-const-exp)
    (simple-exp (identifier)
                cps-var-exp)
    (simple-exp ("proc" "(" (separated-list identifier ",") ")" tf-exp)
                cps-proc-exp)
    (simple-exp ("+" "(" (separated-list simple-exp ",") ")")
                cps-sum-exp)
    (simple-exp ("-" "(" simple-exp "," simple-exp ")")
                cps-diff-exp)
    (simple-exp ("zero?" "(" simple-exp ")")
                cps-zero?-exp)
    (tf-exp (simple-exp)
            simple-exp->exp)
    (tf-exp ("if" simple-exp "then" tf-exp "else" tf-exp)
            cps-if-exp)
    (tf-exp ("let" (arbno identifier "=" simple-exp) "in" tf-exp)
            cps-let-exp)
    (tf-exp ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" tf-exp) "in" tf-exp)
            cps-letrec-exp)
    (tf-exp ("(" simple-exp (arbno simple-exp) ")")
            cps-call-exp)))

(sllgen:make-define-datatypes cps-scanner-spec cps-grammar)
(define cps-list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes cps-scanner-spec cps-grammar)))
(define cps-just-scan
  (sllgen:make-string-scanner cps-scanner-spec cps-grammar))
(define cps-scan&parse
  (sllgen:make-string-parser cps-scanner-spec cps-grammar))

(define cps-unparse-simple-exp
  (lambda (exp)
    (cases simple-exp exp
           (cps-const-exp (num) num)
           (cps-var-exp (var) var)
           (cps-proc-exp
            (vars body)
            `(lambda ,vars ,(cps-unparse-exp body)))
           (cps-sum-exp
            (exps)
            (append '(+) (map cps-unparse-simple-exp exps)))
           (cps-diff-exp
            (exp1 exp2)
            `(- ,(cps-unparse-simple-exp exp1) ,(cps-unparse-simple-exp exp2)))
           (cps-zero?-exp
            (exp1)
            `(zero? ,(cps-unparse-simple-exp exp1))))))

(define cps-unparse-exp
  (lambda (exp)
    (cases tf-exp exp
           (simple-exp->exp (simple) (cps-unparse-simple-exp simple))
           (cps-if-exp
            (exp1 exp2 exp3)
            `(if ,(cps-unparse-simple-exp exp1)
                 ,(cps-unparse-exp exp2)
                 ,(cps-unparse-exp exp3)))
           (cps-let-exp
            (vars exps body)
            `(let ,(map (lambda (v1 v2) (list v1 (cps-unparse-simple-exp v2)))
                        vars exps)
               ,(cps-unparse-exp body)))
           (cps-letrec-exp
            (p-names p-vars p-bodies body)
            `(letrec ,(map (lambda (v1 v2 v3)
                             (list v1
                                   `(lambda ,v2
                                      ,(cps-unparse-exp v3))))
                        p-names p-vars p-bodies)
               ,(cps-unparse-exp body)))
           (cps-call-exp
            (rator rands)
            (map cps-unparse-simple-exp (cons rator rands))))))

(define cps-unparse-prgm
  (lambda (prgm)
    (cases cps-program prgm
           (cps-a-program
            (exp)
            (cps-unparse-exp exp)))))

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
               (make-send-to-cont k (cps-zero?-exp (cps-of-exp exp1 (car simples)))))))
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
            (var exp1 body)
            (cps-of-exps
             (list exp1)
             (lambda (simples)
               (cps-let-exp var (car simples)
                            (cps-of-exp body k)))))
           (letrec-exp
            (p-names p-vars p-bodies body)
            (cps-letrec-exp
             p-names
             (map (lambda (vars) (append vars 'k))
                  p-vars)
             (map (lambda (body) (cps-of-exp body (cps-var-exp 'k)))
                  p-bodies)
             (cps-of-exp body k)))
           (call-exp
            (rator rands)
            (cps-of-exps (cons rator rands)
                         (lambda (simples)
                           (cps-call-exp (car simples)
                                         (append (cdr simples) (list k)))))))))

;;; cps-of-exps: Listof(InpExp) x (Listof(InpExp) -> TfExp) -> TfExp
(define cps-of-rest
  (lambda (exps builder)
    (let ((pos (list-index
                (lambda (exp)
                  (not (inp-exp-simple? exp)))
                exps)))
      (if (not pos)
          (builder (map cps-of-simple-exp exps))
          (let ((var (fresh-identifier 'var)))
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
    (eopl:error "invalid exp to cps: ~a~%" exp)))

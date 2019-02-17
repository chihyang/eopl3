#lang eopl
(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-var symbol?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))
;; unparse-lc-exp : LcExp -> SchemeVal
;;
;; peel the top layer parentheses, everything inside (including parentheses) is
;; part of the expression
(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
           (var-exp (var) var)
           (lambda-exp
            (bound-var body)
            (list 'proc
                  bound-var
                  '=>
                  (unparse-lc-exp body)))
           (app-exp
            (rator rand)
            (append (cases lc-exp rator
                           (var-exp
                            (var)
                            (list (unparse-lc-exp rator)))
                           (else
                            (unparse-lc-exp rator)))
                    (cases lc-exp rand
                           (var-exp
                            (var)
                            (list (list (unparse-lc-exp rand))))
                           (else
                            (list (unparse-lc-exp rand)))))))))
;;; ----- test -----
(define lc-exp1 (var-exp 'a))
(define lc-exp2 (lambda-exp 'x (var-exp 'b)))
(define lc-exp3 (app-exp lc-exp2 lc-exp1))
(define lc-exp4 (app-exp lc-exp3 lc-exp3))
(define lc-exp5 (app-exp lc-exp4 lc-exp4))
(define lc-exp6 (app-exp lc-exp4 lc-exp2))
(define lc-exp7 (app-exp lc-exp1 lc-exp6))
(unparse-lc-exp lc-exp1)
'a
(unparse-lc-exp lc-exp2)
'(proc x => b)
(unparse-lc-exp lc-exp3)
'(proc x => b (a))
(unparse-lc-exp lc-exp4)
'(proc x => b (a) (proc x => b (a)))
(unparse-lc-exp lc-exp5)
'(proc x => b (a) (proc x => b (a)) (proc x => b (a) (proc x => b (a))))
(unparse-lc-exp lc-exp6)
'(proc x => b (a) (proc x => b (a)) (proc x => b))
(unparse-lc-exp lc-exp7)
'(a (proc x => b (a) (proc x => b (a)) (proc x => b)))

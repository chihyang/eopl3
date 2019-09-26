#lang eopl

(provide tests-for-check)
;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

(define tests-for-check
  '(
    ;; tests from run-tests:

    ;; simple arithmetic
    (positive-const "11" int)
    (negative-const "-33" int)
    (simple-arith-1 "-(44,33)" int)

    ;; nested arithmetic
    (nested-arith-left "-(-(44,33),22)" int)
    (nested-arith-right "-(55, -(22,11))" int)

    ;; simple variables
    (test-var-1 "x" error)
    (test-var-2 "-(x,1)" error)
    (test-var-3 "-(1,x)" error)

    (zero-test-1 "zero?(-(3,2))" bool)
    (zero-test-2 "-(2,zero?(0))" error)

    ;; simple unbound variables
    (test-unbound-var-1 "foo" error)
    (test-unbound-var-2 "-(x,foo)" error)

    ;; simple conditionals
    (if-true "if zero?(1) then 3 else 4" int)
    (if-false "if zero?(0) then 3 else 4" int)

    ;; make sure that the test and both arms get evaluated
    ;; properly.
    (if-eval-test-true "if zero?(-(11,12)) then 3 else 4" int)
    (if-eval-test-false "if zero?(-(11, 11)) then 3 else 4" int)
    (if-eval-then "if zero?(1) then -(22,1) else -(22,2)" int)
    (if-eval-else "if zero?(0) then -(22,1) else -(22,2)" int)

    ;; make sure types of arms agree (new for lang5-1)

    (if-compare-arms "if zero?(0) then 1 else zero?(1)" error)
    (if-check-test-is-boolean "if 1 then 11 else 12" error)

    ;; simple let
    (simple-let-1 "let x = 3 in x" int)

    ;; make sure the body and rhs get evaluated
    (eval-let-body "let x = 3 in -(x,1)" int)
    (eval-let-rhs "let x = -(4,1) in -(x,1)" int)

    ;; check nested let and shadowing
    (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" int)
    (check-shadowing-in-body "let x = 3 in let x = 4 in x" int)
    (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" int)

    ;; simple applications
    (apply-proc-in-rator-pos "(proc(x : ?) -(x,1)  30)" int)
    (checker-doesnt-ignore-type-info-in-proc
     "(proc(x : (int -> int)) -(x,1)  30)"
     error)
    (apply-simple-proc "let f = proc (x : ?) -(x,1) in (f 30)" int)
    (let-to-proc-1 "(proc(f : (int -> int))(f 30)  proc(x : int)-(x,1))" int)


    (nested-procs "((proc (x : ?) proc (y : ?) -(x,y)  5) 6)" int)
    (nested-procs2
     "let f = proc (x : ?) proc (y : ?) -(x,y) in ((f -(10,5)) 3)"
     int)

    ;; simple letrecs
    (simple-letrec-1 "letrec int f(x : ?) = -(x,1) in (f 33)" int)
    (simple-letrec-2
     "letrec int f(x : ?) = if zero?(x) then 0 else -((f -(x,1)), -2) in (f 4)"
     int)

    (simple-letrec-3
     "let m = -5
 in letrec int f(x : ?) = if zero?(x) then -((f -(x,1)), m) else 0 in (f 4)"
     int)

    (double-it "
letrec int double (n : ?) = if zero?(n) then 0
                                  else -( (double -(n,1)), -2)
in (double 3)"
               int)

    ;; tests of expressions that produce procedures

    (build-a-proc-typed "proc (x : ?) -(x,1)" ((int) -> int))

    (build-a-proc-typed-2 "proc (x : ?) zero?(-(x,1))" ((int) -> bool))

    (bind-a-proc-typed
     "let f = proc (x : ?) -(x,1) in (f 4)"
     int)

    (bind-a-proc-return-proc
     "let f = proc (x : ?) -(x,1) in f"
     ((int) -> int))

    (type-a-ho-proc-1
     "proc(f : (int -> bool)) (f 3)"
     ((((int)  -> bool)) -> bool))

    (type-a-ho-proc-2
     "proc(f : (bool -> bool)) (f 3)"
     error)

    (apply-a-ho-proc
     "proc (x : ?) proc (f : (int -> bool)) (f x)"
     ((int) -> ((((int) -> bool)) -> bool)))

    (apply-a-ho-proc-2
     "proc (x : ?) proc (f : (int -> (int -> bool))) (f x)"
     ((int) -> ((((int) -> ((int) -> bool))) -> ((int) -> bool))) )

    (apply-a-ho-proc-3
     "proc (x : ?) proc (f : (int -> (int -> bool))) (f zero?(x))"
     error)

    (apply-curried-proc
     "((proc(x : ?) proc (y : ?)-(x,y)  4) 3)"
     int)

    (apply-a-proc-2-typed
     "(proc (x : ?) -(x,1) 4)"
     int)

    (apply-a-letrec "
letrec int f(x : ?) = -(x,1)
in (f 40)"
                    int)

    (letrec-non-shadowing
     "(proc (x : ?)
      letrec bool loop(x : bool) =(loop x)
       in x
     1)"
     int)


    (letrec-return-fact "
let times = proc (x : ?) proc (y : ?) -(x,y)    % not really times
in letrec
     int fact(x : ?) = if zero?(x) then 1 else ((times x) (fact -(x,1)))
   in fact"
                        ((int) -> int))

    (letrec-apply-fact "
let times = proc (x : ?) proc (y : ?) -(x,y)    % not really times
in letrec
     int fact(x : ?) = if zero?(x) then 1 else ((times x) (fact -(x,1)))
   in (fact 4)"
                       int)

    ;; check multiple let
    (simple-multuple-let "let x = 3 y = 4 in -(x,y)" int)

    (nested-procs-in-tf "(proc (x : ? y : ?) -(x,y)  5 6)" int)

    (nested-procs2 "let f = proc(x : ?) proc (y : ?) -(x,y) in ((f -(10,5)) 6)"
                   int)
    (nested-procs3 "let f = proc(x : ? y : ?) -(x,y) in (f -(10,5) 6)"
                   int)

    (HO-multiple-letrecs
     "letrec int even(x : ?) = if zero?(x) then 1 else (odd -(x,1))
             int odd(x : ?)  = if zero?(x) then 0 else (even -(x,1))
      in (odd 13)" int)
    ))

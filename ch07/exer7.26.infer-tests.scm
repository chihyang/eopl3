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
    (apply-proc-in-rator-pos "(proc(x : int) -(x,1)  30)" int)
    (checker-doesnt-ignore-type-info-in-proc
     "(proc(x : (int -> int)) -(x,1)  30)"
     error)
    (apply-simple-proc "let f = proc (x : int) -(x,1) in (f 30)" int)
    (let-to-proc-1 "(proc(f : (int -> int))(f 30)  proc(x : int)-(x,1))" int)


    (nested-procs "((proc (x : int) proc (y : int) -(x,y)  5) 6)" int)
    (nested-procs2
     "let f = proc (x : int) proc (y : int) -(x,y) in ((f -(10,5)) 3)"
     int)

    ;; simple letrecs
    (simple-letrec-1 "letrec int f(x : int) = -(x,1) in (f 33)" int)
    (simple-letrec-2
     "letrec int f(x : int) = if zero?(x) then 0 else -((f -(x,1)), -2) in (f 4)"
     int)

    (simple-letrec-3
     "let m = -5
 in letrec int f(x : int) = if zero?(x) then -((f -(x,1)), m) else 0 in (f 4)"
     int)

    (double-it "
letrec int double (n : int) = if zero?(n) then 0
                                  else -( (double -(n,1)), -2)
in (double 3)"
               int)

    ;; tests of expressions that produce procedures

    (build-a-proc-typed "proc (x : int) -(x,1)" ((int) -> int))

    (build-a-proc-typed-2 "proc (x : int) zero?(-(x,1))" ((int) -> bool))

    (bind-a-proc-typed
     "let f = proc (x : int) -(x,1) in (f 4)"
     int)

    (bind-a-proc-return-proc
     "let f = proc (x : int) -(x,1) in f"
     ((int) -> int))

    (type-a-ho-proc-1
     "proc(f : (int -> bool)) (f 3)"
     ((((int)  -> bool)) -> bool))

    (type-a-ho-proc-2
     "proc(f : (bool -> bool)) (f 3)"
     error)

    (apply-a-ho-proc
     "proc (x : int) proc (f : (int -> bool)) (f x)"
     ((int) -> ((((int) -> bool)) -> bool)))

    (apply-a-ho-proc-2
     "proc (x : int) proc (f : (int -> (int -> bool))) (f x)"
     ((int) -> ((((int) -> ((int) -> bool))) -> ((int) -> bool))) )

    (apply-a-ho-proc-3
     "proc (x : int) proc (f : (int -> (int -> bool))) (f zero?(x))"
     error)

    (apply-curried-proc
     "((proc(x : int) proc (y : int)-(x,y)  4) 3)"
     int)

    (apply-a-proc-2-typed
     "(proc (x : int) -(x,1) 4)"
     int)

    (apply-a-letrec "
letrec int f(x : int) = -(x,1)
in (f 40)"
                    int)

    (letrec-non-shadowing
     "(proc (x : int)
      letrec bool loop(x : bool) =(loop x)
       in x
     1)"
     int)


    (letrec-return-fact "
let times = proc (x : int) proc (y : int) -(x,y)    % not really times
in letrec
     int fact(x : int) = if zero?(x) then 1 else ((times x) (fact -(x,1)))
   in fact"
                        ((int) -> int))

    (letrec-apply-fact "
let times = proc (x : int) proc (y : int) -(x,y)    % not really times
in letrec
     int fact(x : int) = if zero?(x) then 1 else ((times x) (fact -(x,1)))
   in (fact 4)"
                       int)

    (lists-1
     "list(2, 3, 4)"
     (listof int))

    (car-1
     "car(list(2,3,4))"
     int)

    (cdr-1
     "cdr(list(2,3,4))"
     (listof int))


    (cons-1
     "cons(2,cons(3, emptylist))"
     (listof int))

    (cons-2
     "cons(2,cons(3, emptylist))"
     (listof int))

    (null-1
     "null?(emptylist)"
     bool)

    (list-2
     "list(1, emptylist)"
     error)

    (begin-test-1
     "begin 1; 2; 3 end"
     int)

    (simple-store-test-1 "let x = newref(17) in deref(x)" int)

    (assignment-test-1 "let x = newref(17)
                          in begin setref(x,27); deref(x) end"
                       int)

    (gensym-test-2
     "let g = let counter = newref(0)
         in proc (dummy : ?) begin
                           setref(counter, -(deref(counter),-1));
                           deref(counter)
                         end
 in -((g 11),(g 22))"
     int)

    (show-allocation-1 "
let x = newref(22)
in let f = proc (z : ?) let zz = newref(-(z,deref(x))) in deref(zz)
   in -((f 66), (f 55))"
                       int)

    (chains-1 "
let x = newref(newref(0))
in begin
    setref(deref(x), 11);
    deref(deref(x))
   end"
              int)

    (set-change-type "let x = newref(3) in begin setref(x, zero?(x)); x end"
                     error)
)

  )

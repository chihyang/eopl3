(module tests mzscheme

  ;; tests for cps converter, including explicit references.

  (provide test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

  (define test-list
    '(
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)

      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" -11)
      (nested-arith-right "-(55, -(22,11))" 44)

      ;; simple variables
      (test-var-1 "x" error)
      (test-var-2 "-(x,1)" error)
      (test-var-3 "-(1,x)" error)

      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "-(x,foo)" error)

      ;; simple conditionals
      (if-true "if zero?(0) then 3 else 4" 3)
      (if-false "if zero?(1) then 3 else 4" 4)

      ;; test dynamic typechecking
      (no-bool-to-diff-1 "-(zero?(0),1)" error)
      (no-bool-to-diff-2 "-(1,zero?(0))" error)
      (no-int-to-if "if 1 then 2 else 3" error)

      ;; make sure that the test and both arms get evaluated
      ;; properly.
      (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
      (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)

      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
      (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)

      ;; simple let
      (simple-let-1 "let x = 3 in x" 3)

      ;; make sure the body and rhs get evaluated
      (eval-let-body "let x = 3 in -(x,1)" 2)
      (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)

      ;; check nested let and shadowing
      (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
      (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
      (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)

      ;; simple applications
      (apply-proc-in-rator-pos "(proc(x) -(x,1)  30)" 29)
      (apply-simple-proc "let f = proc (x) -(x,1) in (f 30)" 29)
      (let-to-proc-1 "(proc(f)(f 30)  proc(x)-(x,1))" 29)



      (nested-procs-not-in-cps "((proc (x) proc (y) -(x,y)  5) 6)" -1)

      (nested-procs-in-tf "(proc (x, y) -(x,y)  5 6)" -1)

      (nested-procs2 "let f = proc(x, y) -(x,y) in (f -(10,5) 6)"
                     -1)

      (y-combinator-1-not-in-tf "
let fix =  proc (f)
            let d = proc (x) proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
in let
    t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 3)" 12)


      ;; this one is not in cps
      (twice "
        (proc (twice)
           ((twice proc (z) -(z,1)) 11)
         proc (f) proc (x) (f (f x)))"
             9)

      (twice-cps "
      let twice = proc(f, x, k)
                    (f x  proc (z) (f z k))
      in (twice
          proc (x, k) (k -(x,1))
          11
          proc(z) z)"
                 9)

      (cps-both-simple "
       let f = proc (x) -(x,1)
       in (f 27)"
                       26)

      (cps-simple-rator "
       let f = proc (x) -(x,1)
       in (f (f 27))"
                        25)

      (cps-simple-rand "
       let f = proc (x) proc (y) -(x,y)
       in ((f 27) 4)"
                       23)

      (cps-neither-simple "
      let f = proc (x) proc (y) -(x, y)
      in let g = proc (z) -(z, 1)
      in ((f 27) (g 11))"
                          17)

      (cps-serious-zero-test "
      let f = proc (x) -(x, 1)
      in if zero?((f 1)) then 11 else 22"
                             11)

      (sum-test-1 "+()" 0)
      (sum-test-2 "+(2,3,4)" 9)

      (letrec-test-1 "letrec f(x) = 17 in 34" 34)

      (letrec-test-2 "letrec f(x, y) = -(x,y) in -(34, 2)" 32)

      (letrec-test-3 "
       letrec even(x) = if zero?(x) then zero?(0) else (odd -(x,1))
              odd (x) = if zero?(x) then zero?(1) else (even -(x,1))
       in (even 5)"
                     #f)

      (letrec-test-4 "
      letrec fib(n) = if zero?(n) then 1
                      else if zero?(-(n,1)) then 1
                      else -((fib -(n,1)), -(0, (fib -(n,2))))
      in (fib 5)"
                     8)

      (letrec-sum-test-5 "
      letrec fib(n) = if zero?(n) then 1
                      else if zero?(-(n,1)) then 1
                      else -((fib -(n,1)), -(0, (fib -(n,2))))
      in +((fib 1), 12, (fib 5))"
                         21)

      ;; tests from explicit-refs
      (gensym-test-1
       "let g = let counter = newref(0)
         in proc (dummy) let d = setref(counter, -(deref(counter),-1))
                    in deref(counter)
in -((g 11),(g 22))"
       -1)

      (simple-store-test-1 "let x = newref(17) in deref(x)" 17)

      (assignment-test-1 "let x = newref(17)
                          in let y = setref(x,27) in deref(x)"
                         27)

      (gensym-test-2
       "let g = let counter = newref(0)
         in proc (dummy)
             let dummy1 = setref(counter, -(deref(counter),-1))
             in deref(counter)
 in -((g 11),(g 22))"
       -1)

      (even-odd-via-set-1 "
let x = newref(0)
in letrec even(d) = if zero?(deref(x))
                   then 1
                   else let d = setref(x, -(deref(x),1))
                        in (odd d)
          odd(d)  = if zero?(deref(x))
                   then 0
                   else let d = setref(x, -(deref(x),1))
                        in (even d)
   in let d = setref(x,13) in (odd -100)" 1)

      (even-odd-via-set-2 "
let x = newref(0)
in letrec even(d) = if zero?(deref(x))
                   then 1
                   else let d = setref(x, -(deref(x),1))
                        in (odd d)
          odd(d)  = if zero?(deref(x))
                   then 0
                   else let d = setref(x, -(deref(x),1))
                        in (even d)
   in let d = setref(x,13) in (odd -100)" 1)

      (show-allocation-1 "
let x = newref(22)
in let f = proc (z) let zz = newref(-(z,deref(x))) in deref(zz)
   in -((f 66), (f 55))"
                         11)

      ;; thanks to EFanZh for finding this
      (let-scope-1 "
      let x = 6 in -(let x = 11 in x, x)"
                   5)
      )))

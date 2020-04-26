#lang eopl

(provide tests-for-run tests-for-parse)

(define the-test-suite
    '(

      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)

      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" -11)
      (nested-arith-right "-(55, -(22,11))" 44)

      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "-(x,1)" 9)
      (test-var-3 "-(1,x)" -9)

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
      (apply-proc-in-rator-pos "(proc(x : int) -(x,1)  30)" 29)
      (interp-ignores-type-info-in-proc "(proc(x : (int -> int)) -(x,1)  30)" 29)
      (apply-simple-proc "let f = proc (x : int) -(x,1) in (f 30)" 29)
      (let-to-proc-1 "(proc(f : (int -> int))(f 30)  proc(x : int)-(x,1))" 29)


      (nested-procs "((proc (x : int) proc (y : int) -(x,y)  5) 6)" -1)
      (nested-procs2 "let f = proc(x : int) proc (y : int) -(x,y) in ((f -(10,5)) 6)"
        -1)

       (y-combinator-1 "
let fix =  proc (f : bool)
            let d = proc (x : bool) proc (z : bool) ((f (x x)) z)
            in proc (n : bool) ((f (d d)) n)
in let
    t4m = proc (f : bool) proc(x : bool) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 3)" 12)

       ;; simple letrecs
      (simple-letrec-1 "letrec int f(x : int) = -(x,1) in (f 33)" 32)
      (simple-letrec-2
        "letrec int f(x : int) = if zero?(x)  then 0 else -((f -(x,1)), -2) in (f 4)"
        8)

      (simple-letrec-3
        "let m = -5
 in letrec int f(x : int) = if zero?(x) then 0 else -((f -(x,1)), m) in (f 4)"
        20)

      (HO-nested-letrecs
"letrec int even(odd : (int -> int))  = proc(x : int) if zero?(x) then 1 else (odd -(x,1))
   in letrec  int odd(x : int)  = if zero?(x) then 0 else ((even odd) -(x,1))
   in (odd 13)" 1)


      ;;;;;;;;;;;;;;;; typed oop ;;;;;;;;;;;;;;;;

    (test-self-1 "
class c extends object
         field int s
         method void initialize(v : int)set s = v
         method void sets(v : int)set s = v
         method int gets()s
         method void testit()send self sets(13)

let o = new c (11)
       t1 = 0
       t2 = 0
   in begin
       set t1 = send o gets();
       send o testit();
       set t2 = send o gets();
       list(t1,t2)
      end" (11 13))

    (counter-1 "
class counter extends object
  field int count
   method void initialize()set count = 0
   method void countup()set count = +(count,1)
   method int getcount()count

let o1 = new counter ()
    t1 = 0
    t2 = 0
in begin
    set t1 = send o1 getcount();
    send o1 countup();
    set t2 = send o1 getcount();
    list(t1,t2)
end
" (0 1))

    (shared-counter-1 "
class counter extends object
  field int count
   method void initialize()set count = 0
   method void countup()set count = +(count,1)
   method int getcount()count

class c1 extends object
   field int n
   field counter counter1
   method void initialize(a_counter : counter)
    begin
     set n = 0;
     set counter1 = a_counter
    end
   method void countup()
     begin
      send counter1 countup();
      set n = +(n,1)
     end
   method listof int getstate()list(n, send counter1 getcount())

let counter1 = new counter()
in let o1 = new c1(counter1)
       o2 = new c1(counter1)
in begin
     send o1 countup();
     send o2 countup();
     send o2 countup();
     list( send o1 getstate(),
           send o2 getstate())
   end
" ((1 3) (2 3)))


    (inherit-1 "
class c1 extends object
  field int ivar1
  method void initialize()set ivar1 = 1

class c2 extends c1
  field int ivar2
  method void initialize()
   begin
    super initialize();
    set ivar2 = 1
   end
  method void setiv1(n : int)set ivar1 = n
  method int getiv1()ivar1

let o = new c2 ()
    t1 = 0
in begin
       send o setiv1(33);
       send o getiv1()
   end
" 33)

    (inherit-3 "
class c1 extends object
  method int initialize()1
  method int m1()1

class c2 extends c1
  method int initialize()1
  method int m1()super m1()
  method int m2()2

class c3 extends c2
  method int initialize()1
  method int m1()3
  method int m2()super m2()
  method int m3()super m1()

let o = new c3 ()
in list( send o m1(),
         send o m2(),
         send o m3()
        )
" (3 2 1))

    (chris-1 "
class aclass extends object
  field int i
  method void initialize(x : int) set i = x
  method int m(y : int) +(i,y)

let o1 = new aclass(3)
in send o1 m(2)" 5)

    (chris-2 "
class c1 extends object
  method int initialize() 1
  method int ma()1
  method int mb()send self ma()

class c2 extends c1   % just use c1's initialize
  method int ma() 2

let x = new c2 ()
in list(send x ma(),send x mb())
" (2 2))

    (for-book-1 "
class c1 extends object
  field int i
  field int j
  method void initialize(x : int) begin set i = x; set j = -(0,x) end
  method void countup(d : int) begin set i = +(i,d); set j = -(j,d) end
  method listof int getstate()list(i,j)

let o1 = new c1(3)
    t1 = list(1)
    t2 = list(1)
in begin
    set t1 = send o1 getstate();
    send o1 countup(2);
    set t2 = send o1 getstate();
    list(t1,t2)
   end" ((3 -3) (5 -5)))


    (odd-even-via-self "
class oddeven extends object
  method int initialize()1
  method bool even(n : int)if zero?(n) then 1 else send self odd(-(n,1))
  method bool odd(n : int) if zero?(n) then 0 else send self even(-(n,1))

let o1 = new oddeven() in send o1 odd(13)" 1)

    (for-book-2 "
class c1 extends object
  method int initialize()1
  method int m1()1
  method int m2()100
  method int m3()send self m2()

class c2 extends c1
  method int initialize()1
  method int m2()2

let o1 = new c1()
    o2 = new c2()
in list(send o1 m1(),           % returns 1
        send o1 m2(),           % returns 100
        send o1 m3(),           % returns 100
        send o2 m1(),           % returns 1 (from c1)
        send o2 m2(),           % returns 2 (from c2)
        send o2 m3()            % returns 2 (c1's m3 calls c2's m2)
       )
" (1 100 100 1 2 2))

    (sum-leaves "
class tree extends object
  method int initialize()1

class interior_node extends tree
  field node left
  field node right
  method void initialize(l : node, r : node)
   begin
    set left = l; set right = r
   end
  method int sum()+(send left sum(), send right sum())

class leaf_node extends tree
  field int value
  method void initialize(v : int)set value = v
  method int sum()value

let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),
            new leaf_node(4)),
          new leaf_node(5))
in send o1 sum()
" 12)

    (sum-leaves-2 "
interface tree
  method int sum (l : tree, r : tree)

class interior_node extends object
  field tree left
  field tree right
  method void initialize(l : tree, r : tree)
   begin
    set left = l; set right = r
   end
  method int sum() +(send left sum(), send right sum())

class leaf_node extends object
  field int value
  method void initialize(v : int)set value = v
  method int sum()value

let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),
            new leaf_node(4)),
          new leaf_node(5))
in send o1 sum()
" 12)

    (sum-leaves-with-abstract-method "
interface tree
  method int sum()

class interior_node extends object
  field tree left
  field tree right
  method void initialize(l : tree, r : tree)
   begin
    set left = l; set right = r
   end
  method int sum()+(send left sum(), send right sum())

class leaf_node extends object
  field int value
  method void initialize(v : int)set value = v
  method int sum()value

let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),   %% need subtyping to make this ok.
            new leaf_node(4)),
          new leaf_node(5))
in send o1 sum()
" 12)


    (equal-trees-1 "
interface tree
  method int sum()
  method bool equal(t : tree)

class interior_node extends object
  field tree left
  field tree right
  method void initialize(l : tree, r : tree)
   begin
    set left = l; set right = r
   end
  method tree getleft()left
  method tree getright()right
  method int sum()+(send left sum(), send right sum())
  method bool equal(t : tree)
    if instanceof t interior_node
     then if send left equal(send cast t interior_node getleft())
          then send right equal(send cast t interior_node getright())
          else false
     else false


class leaf_node extends object
  field int value
  method void initialize(v : int)set value = v
  method int sum()value
  method int getvalue()value
  method bool equal(t : tree)
   if instanceof t leaf_node
    then zero?(-(value, send cast t leaf_node getvalue()))
    else zero?(1)


let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),
            new leaf_node(4)),
          new leaf_node(5))
in send o1 equal(o1)
" #t)

    (good-instanceof-1 "
class c1 extends object
 method int initialize () 1
class c2 extends object
 method int initialize () 2
let p = proc (o : c1) instanceof o c2 in 11
" 11)

    (up-cast-1 "
class c1 extends object
  method int initialize ()1
  method int get()2

class c2 extends c1
let f = proc (o : c2) send cast o c1 get() in (f new c2())
" 2)

    (up-instance-1 "
class c1 extends object
  method int initialize ()1
  method int get()2

class c2 extends c1
let f = proc (o : c2) instanceof o c1 in (f new c2())
" #t)

    (duplicate-methods-1 "
class c1 extends object
  method int initialize() 1
class c2 extends c1
  method int m1() 1
  method int m1() 2
33" 33)

    (incomparable-instanceof-2 "
class c1 extends object
  method int initialize ()1
  method int get()2

class c2 extends object
  method int initialize () 100

let f = proc (o : c2) if instanceof o c1 then 1 else 2 in (f new c2())
" 2)

    (equal-trees-by-double-dispatch "
interface tree
  method int sum()
  method bool equal(t : tree)
  method bool equal_int(l : tree, r : tree)
  method bool equal_leaf(val : int)

class interior_node extends object
  field tree left
  field tree right
  method void initialize(l : tree, r : tree)
   begin
    set left = l; set right = r
   end
  method int sum() +(send left sum(), send right sum())
  method bool equal(t : tree) send t equal_int(left, right)
  method bool equal_int(l : tree, r : tree)
     if send left equal(l)
     then send right equal(r)
     else zero?(1)  % false

  method bool equal_leaf(v : int) false

class leaf_node extends object
  field int value
  field bool false
  method void initialize(v : int) begin set value = v; set
                                      false=zero?(1) end
  method int sum()value
  method bool equal(t : tree) send t equal_leaf(value)
  method bool equal_int(l : tree, r : tree) false
  method bool equal_leaf(otherval : int) zero?(-(value, otherval))

let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),
            new leaf_node(4)),
          new leaf_node(5))
in send o1 equal(o1)
" #t)

    (goldberg-80 "
class c1 extends object
  method int initialize () 1
  method int test () 1
  method int result1 () send self test ()

class c2 extends c1
  method int test () 2

class c3 extends c2
  method int result2 () send self result1 ()
  method int result3 () super test ()

class c4 extends c3
  method int test () 4

let o3 = new c3 ()
    o4 = new c4 ()
in list(send o3 test(),
        send o4 result1 (),
        send o3 result2 (),
        send o4 result2 (),
        send o3 result3 (),
        send o4 result3 ())
" (2 4 2 4 2 2))

      ))

(define tests-for-run
  (let loop ((lst the-test-suite))
    (cond
     ((null? lst) '())
     ((>= (length (car lst)) 3)
      ;; (printf "creating item: ~s~%" (caar lst))
      (cons
       (list
        (list-ref (car lst) 0)
        (list-ref (car lst) 1)
        (list-ref (car lst) 2))
       (loop (cdr lst))))
     (else (loop (cdr lst))))))

(define tests-for-parse
  (let loop ((lst the-test-suite))
    (cond
     ((null? lst) '())
     ((> (length (car lst)) 3)
      (cons
       (list
        (list-ref (car lst) 0)
        (list-ref (car lst) 1)
        (list-ref (car lst) 3))
       (loop (cdr lst))))
     (else
      (cons
       (list
        (list-ref (car lst) 0)
        (list-ref (car lst) 1)
        #t)
       (loop (cdr lst)))))))

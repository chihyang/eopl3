#lang eopl

(provide tests-for-run tests-for-check tests-for-parse)

(define the-test-suite
  '(

    (modules-dans-simplest
     "
         module m1
          interface
           [a : int
            b : int]
          body
           [a = 33
            c = -(a,1)
            b = -(c,a)]

         import m1
         let a = 10
         in -(-(from m1 take a, from m1 take b),
              a)"
     int 24)


    (example-8.2
     "
         module m1
          interface
           [u : bool]
          body
           [u = 33]

         44"
     error 44)

    (example-8.3
     "
         module m1
          interface
           [u : int
            v : int]
          body
           [u = 33]

         44"
     error)

    (example-8.4
     "
         module m1
          interface
           [u : int
            v : int]
          body
           [v = 33
            u = 44]

         from m1 take u"
     error)

    (example-8.5a
     "
         module m1
          interface
           [u : int]
          body
           [u = 44]

         module m2
          interface
           [v : int]
          body
           import m1
           [v = -(from m1 take u,11)]

         import m1, m2
         -(from m1 take u, from m2 take v)"
     int)

    (example-8.5b
     "
         module m2
          interface [v : int]
          body
           import m1
           [v = -(from m1 take u,11)]

         module m1
          interface [u : int]
          body [u = 44]

         import m1, m2
         -(from m1 take u, from m2 take v)"
     error)

    (example-8.10
     "
       module m1
       interface
        [transparent t = int
         z : t
         s : (t -> t)
         is-z? : (t -> bool)]
       body
        [type t = int
         z = 0
         s = proc (x : t) -(x,-1)
         is-z? = proc (x : t) zero?(x)]

      import m1
      let foo = proc (z : from m1 take t)
                 -(0, (from m1 take s
                       z))
      in
      (foo
       from m1 take z)"
                 int -1)

    (example-8.14
     "
         module m1
       interface [transparent t = int
                  z : t]
       body [type t = int
                       z = 0]
      module m2
       interface
        [foo : (from m1 take t -> int)]
       body
        import m1
        [foo = proc (x : from m1 take t) x]

      import m2
      from m2 take foo"
     (int -> int))

    (example-8.15
     "
         module m1
       interface
        [opaque t
         z : t
         s : (t -> t)
         is-z? : (t -> bool)]
       body
        [type t = int
         z = 0
         s = proc (x : t) -(x,-1)
         is-z? = proc (x : t) zero?(x)]

      import m1
      let foo = proc (z : from m1 take t)
                 (from m1 take s
                  (from m1 take s
                   z))
      -(0, (foo
            from m1 take z))"
     error error error)

    (example-8.15a
     "
      module m1
       interface
        [opaque t
         z : t
         s : (t -> t)
         is-z? : (t -> bool)]
       body
        [type t = int
         z = 0
         s = proc (x : t) -(x,-1)
         is-z? = proc (x : t) zero?(x)]

      import m1
      let foo = proc (z : from m1 take t)
                 (from m1 take s
                  z)
      in (foo
       from m1 take z)"
     (m1 t))

    (example-8.8
     "
         module colors
         interface
          [opaque color
           red : color
           green : color
           is-red? : (color -> bool)
           switch-colors : (color -> color)]
         body
          [type color = int
           red = 0
           green = 1
           is-red? = proc (c : color) zero?(c)
           switch-colors = proc (c : color)
                            if (is-red? c) then green else red]

         44"
     int)

    (example-8.9
     "
         module ints-1
         interface [opaque t
                    zero : t
                    succ : (t -> t)
                    pred : (t -> t)
                    is-zero : (t -> bool)]
         body [type t = int
                         zero = 0
                         succ = proc(x : t) -(x,-5)
                         pred = proc(x : t) -(x,5)
                         is-zero = proc (x : t) zero?(x)]

         import ints-1
         let zero = from ints-1 take zero
         in let succ = from ints-1 take succ
         in (succ (succ zero))"
     (ints-1 t) 10)

    (example-8.10
     "
         module ints-2
         interface [opaque t
                    zero : t
                    succ : (t -> t)
                    pred : (t -> t)
                    is-zero : (t -> bool)]
         body [type t = int
                         zero = 0
                         succ = proc(x : t) -(x,3)
                         pred = proc(x : t) -(x,-3)
                         is-zero = proc (x : t) zero?(x)]

         import ints-2
         let z = from ints-2 take zero
         in let s = from ints-2 take succ
         in (s (s z))"
     (ints-2 t) -6)

    (example-8.11
     "
         module ints-1
         interface [opaque t
                    zero : t
                    succ : (t -> t)
                    pred : (t -> t)
                    is-zero : (t -> bool)]
         body [type t = int
                         zero = 0
                         succ = proc(x : t) -(x,-5)
                         pred = proc(x : t) -(x,5)
                         is-zero = proc (x : t) zero?(x)]
        import ints-1
        let z = from ints-1 take zero
        in let s = from ints-1 take succ
        in let p = from ints-1 take pred
        in let z? = from ints-1 take is-zero
        in letrec int to-int (x : from ints-1 take t) =
                      if (z? x) then 0
                         else -((to-int (p x)), -1)
        in (to-int (s (s z)))"
     int 2)

    (example-8.12
     "
         module ints-2
         interface [opaque t
                    zero : t
                    succ : (t -> t)
                    pred : (t -> t)
                    is-zero : (t -> bool)]
         body [type t = int
                         zero = 0
                         succ = proc(x : t) -(x,3)
                         pred = proc(x : t) -(x,-3)
                         is-zero = proc (x : t) zero?(x)
                         ]

         import ints-2
         let z = from ints-2 take zero
         in let s = from ints-2 take succ
         in let p = from ints-2 take pred
         in let z? = from ints-2 take is-zero
         in letrec int to-int (x : from ints-2 take t) =
                       if (z? x) then 0
                          else -((to-int (p x)), -1)
         in (to-int (s (s z)))"
     int 2)

    (example-8.13
     "
         module mybool
          interface [opaque t
                     true : t
                     false : t
                     and : (t -> (t -> t))
                     not : (t -> t)
                     to-bool : (t -> bool)]
          body [type t = int
                          true = 0
                          false = 13
                          and = proc (x : t)
                                 proc (y : t)
                                  if zero?(x)
                                   then y
                                   else false
                          not = proc (x : t)
                                 if zero?(x)
                                  then false
                                  else true
                          to-bool = proc (x : t) zero?(x)]

         import mybool
         let true = from mybool take true
         in let false = from mybool take false
         in let and = from mybool take and
         in ((and true) false)"
     (mybool t) 13)

    (exercise-8.14
     "
         module mybool
         interface [opaque t
                    true : t
                    false : t
                    and : (t -> (t -> t))
                    not : (t -> t)
                    to-bool : (t -> bool)]
         body [type t = int
                         true = 1
                         false = 0
                         and = proc (x : t)
                                proc (y : t)
                                 if zero?(x)
                                  then false
                                  else y
                         not = proc (x : t)
                                if zero?(x)
                                 then true
                                 else false
                         to-bool = proc (x : t)
                                    if zero?(x)
                                     then zero?(1)
                                     else zero?(0)]
          44"
     int 44)

    (alice-bob-and-charlie
     "
  module Alices-point-builder
   interface
    ((database : [opaque db-type
                  opaque node-type
                  insert-node : (node-type -> (db-type -> db-type))
                  ])
     => [opaque point
         initial-point : (int -> point)])

   body
    module-proc
     (database : [opaque db-type
                  opaque node-type
                  insert-node : (node-type -> (db-type -> db-type))])

     [type point = int
      initial-point = proc (x : int) x]

  module Bobs-db-module
   interface
    [opaque db-type
     opaque node-type
     insert-node : (node-type -> (db-type -> db-type))]
   body
    [type db-type = int
     type node-type = bool
     insert-node = proc (n : node-type) proc (d : db-type) d]

  module Alices-points
   interface
    [opaque point
     initial-point : (int -> point)]
   body
    import Alices-point-builder, Bobs-db-module
    (Alices-point-builder Bobs-db-module)

  module Davids-db-module
   interface
    [opaque db-type
     opaque node-type
     insert-node : (node-type -> (db-type -> db-type))]
   body
   [type db-type = bool
     type node-type = int
     insert-node = proc (n : node-type) proc (d : db-type) d]

  module Charlies-points
   interface
     [opaque point
     initial-point : (int -> point)]
   body
    import Alices-point-builder, Davids-db-module
    (Alices-point-builder Davids-db-module)

  44"
     int 44)

    (example-8.15
     "
        module to-int-maker
         interface
          ((m1 : [opaque t
                  zero : t
                  succ : (t -> t)
                  pred : (t -> t)
                  is-zero : (t -> bool)])
            => [to-int : (from m1 take t -> int)])
         body
          module-proc
           (m1 : [opaque t
                  zero : t
                  succ : (t -> t)
                  pred : (t -> t)
                  is-zero : (t -> bool)])
           [to-int
             = let z? = from m1 take is-zero
               in let p = from m1 take pred
               in letrec int to-int (x : from m1 take t)
                 = if (z? x)
                   then 0
                   else -((to-int (p x)), -1)
               in to-int]

        module ints-1
         interface [opaque t
                    zero : t
                    succ : (t -> t)
                    pred : (t -> t)
                    is-zero : (t -> bool)]
         body [type t = int
                         zero = 0
                         succ = proc(x : t) -(x,-5)
                         pred = proc(x : t) -(x,5)
                         is-zero = proc (x : t) zero?(x)]

        module ints-1-to-int
         interface [to-int : (from ints-1 take t -> int)]
         body
          import to-int-maker, ints-1
          (to-int-maker ints-1)

        import ints-1-to-int, ints-1
        let two1 = (from ints-1 take succ
                    (from ints-1 take succ
                     from ints-1 take zero))
        in (from ints-1-to-int take to-int
            two1)"
     int 2)


    (example-8.16
     "
        module to-int-maker
         interface
          ((m1 : [opaque t
                  zero : t
                  succ : (t -> t)
                  pred : (t -> t)
                  is-zero : (t -> bool)])
            => [to-int : (from m1 take t -> int)])
         body
          module-proc
           (m1 : [opaque t
                  zero : t
                  succ : (t -> t)
                  pred : (t -> t)
                  is-zero : (t -> bool)])
           [to-int
             = let z? = from m1 take is-zero
               in let p = from m1 take pred
               in letrec int to-int (x : from m1 take t)
                 = if (z? x)
                   then 0
                   else -((to-int (p x)), -1)
               in to-int]

        module ints-1
         interface [opaque t
                    zero : t
                    succ : (t -> t)
                    pred : (t -> t)
                    is-zero : (t -> bool)]
         body [type t = int
                         zero = 0
                         succ = proc(x : t) -(x,-5)
                         pred = proc(x : t) -(x,5)
                         is-zero = proc (x : t) zero?(x)]


         module ints-2
         interface [opaque t
                    zero : t
                    succ : (t -> t)
                    pred : (t -> t)
                    is-zero : (t -> bool)]
         body [type t = int
                         zero = 0
                         succ = proc(x : t) -(x,3)
                         pred = proc(x : t) -(x,-3)
                         is-zero = proc (x : t) zero?(x)
                         ]

        module ints-1-to-int
         interface [to-int : (from ints-1 take t -> int)]
         body
          import to-int-maker, ints-1
          (to-int-maker ints-1)

        module ints-2-to-int
         interface [to-int : (from ints-2 take t -> int)]
         body
          import to-int-maker, ints-2
          (to-int-maker ints-2)

        import ints-1-to-int, ints-2-to-int, ints-1, ints-2
        let s1 = from ints-1 take succ
        in let z1 = from ints-1 take zero
        in let to-ints-1 = from ints-1-to-int take to-int

        in let s2 = from ints-2 take succ
        in let z2 = from ints-2 take zero
        in let to-ints-2 = from ints-2-to-int take to-int

        in let two1 = (s1 (s1 z1))
        in let two2 = (s2 (s2 z2))
        in -((to-ints-1 two1), (to-ints-2 two2))"
     int 0)

    ;; simple applications
    (apply-proc-in-rator-pos "(proc(x : int) -(x,1)  30)" int 29)
    (checker-doesnt-ignore-type-info-in-proc-but-interp-does
     "(proc(x : (int -> int)) -(x,1)  30)"
     error 29)
    (apply-simple-proc "let f = proc (x : int) -(x,1) in (f 30)" int 29)
    (let-to-proc-1
     "(proc( f : (int -> int))(f 30)  proc(x : int)-(x,1))" int 29)

    (nested-procs "((proc (x : int) proc (y : int) -(x,y)  5) 6)" int -1)
    (nested-procs2
     "let f = proc (x : int) proc (y : int) -(x,y) in ((f -(10,5)) 3)"
     int 2)

    ;; simple letrecs
    (simple-letrec-1 "letrec int f(x : int) = -(x,1) in (f 33)" int 32)
    (simple-letrec-2
     "letrec int double(x : int) = if zero?(x) then 0 else -((double -(x,1)), -2) in (double 4)"
     int 8)

    (simple-letrec-3
     "let m = -5
 in letrec int f(x : int) = if zero?(x) then 0 else -((f -(x,1)), m) in (f 4)"
     int 20)

    (double-it
     "
letrec int double (n : int) = if zero?(n) then 0
                                  else -( (double -(n,1)), -2)
in (double 3)"
     int 6)

    ;; tests of expressions that produce procedures

    (build-a-proc-typed "proc (x : int) -(x,1)" (int -> int))

    (build-a-proc-typed-2 "proc (x : int) zero?(-(x,1))" (int -> bool))

    (bind-a-proc-typed
     "let f = proc (x : int) -(x,1) in (f 4)"
     int 3)

    (bind-a-proc-return-proc
     "let f = proc (x : int) -(x,1) in f"
     (int -> int))

    (type-a-ho-proc-1
     "proc(f : (int -> bool)) (f 3)"
     ((int  -> bool) -> bool))

    (type-a-ho-proc-2
     "proc(f : (bool -> bool)) (f 3)"
     error)

    (apply-a-ho-proc
     "proc (x : int) proc ( f : (int -> bool)) (f x)"
     (int -> ((int -> bool) -> bool)))

    (apply-a-ho-proc-2
     "proc (x : int) proc ( f : (int -> (int -> bool))) (f x)"
     (int -> ((int -> (int -> bool)) -> (int -> bool)))
     )

    (apply-a-ho-proc-3
     "proc (x : int) proc ( f : (int -> (int -> bool))) (f zero?(x))"
     error)

    (apply-curried-proc
     "((proc(x : int) proc (y : int)-(x,y)  4) 3)"
     int 1)

    (apply-a-proc-2-typed
     "(proc (x : int) -(x,1) 4)"
     int 3)

    (apply-a-letrec
     "
letrec int f(x : int) = -(x,1)
in (f 40)"
     int 39)

    (letrec-non-shadowing
     "(proc (x : int)
      letrec bool loop(x : bool) =(loop x)
       in x
     1)"
     int 1)


    (letrec-return-fact
     "
let times = proc (x : int) proc (y : int) -(x,y)    % not really times
in letrec
     int fact(x : int) = if zero?(x) then 1 else ((times x) (fact -(x,1)))
   in fact"
     (int -> int))

    (letrec-apply-the-fcn
     "
let f = proc (x : int) proc (y : int) -(x,y)
in letrec
     int loop(x : int) = if zero?(x) then 1 else ((f x) (loop -(x,1)))
   in (loop 4)"
     int 3)

    (modules-declare-and-ignore
     "
module m
 interface
  [u : int]
 body
  [u = 3]

33"
     int 33)

    (modules-take-one-value
     "
module m
 interface
  [u : int]
 body
  [u = 3]

import m
from m take u"
     int 3)

    (modules-take-one-value-no-import
     "module m
          interface
           [u : int]
          body
           [u = 3]
         import m
         from m take u"
     int 3)

    (modules-take-from-parameterized-module
     "
module m
 interface
  ((m1 : []) => [u : int])
 body
  module-proc (m1 : []) [u = 3]

import m
from m take u
"
     error error)

    (modules-check-iface-subtyping-1
     "
module m
 interface
  [u : int]
 body
  [u = 3 v = 4]

import m
from m take u"
     int 3)


    ;; if the interpreter always called the typechecker, or put
    ;; only declared variables in the module, this would raise an
    ;; error.  Exercise: make this modification.

    (modules-take-one-value-but-interface-bad
     "
         module m interface []  body [u = 3]
         import m
         from m take u"
                                        ; this version for permissive interp
     error 3
                                        ; this version for strict interp
                                        ;         error error
     )

    (modules-take-bad-value
     "module m interface []  body [u = 3]
         import m
         from m take x"
     error error)

    (modules-two-vals
     "
module m
 interface
  [u : int
   v : int]
 body
  [u = 44
   v = 33]

 import m
  -(from m take u, from m take v)"
     int 11)


    (modules-two-vals-bad-interface-1
     "module m interface [u : int v : bool]
                  body [u = 44 v = 33]
         import m
         -(from m take u, from m take v)"
     error 11)

    (modules-extra-vals-are-ok-1
     "
          module m interface [x : int] body [x = 3 y = 4]
          import m
          from m take x"
     int 3)

    (module-extra-vals-are-ok-2
     "
          module m interface [y : int] body [x = 3 y = 4]
          import m
          from m take y"
     int)

    (module-extra-types-are-ok-11
     "module m interface [y : int] body [x = 3 type t = int y = 4]
         import m
         from m take y"
     int 4)

    (module-extra-types-are-ok-12
     "module m interface [opaque t y : int]
                    body [type u = bool x = 3 type t = int y = 4]
          import m
          from m take y"
     int)

    (module-extra-types-are-ok-13
     "module m interface [transparent t = int y : int]
                    body [type u = bool x = 3 type t = int y = 4]
          import m
          from m take y"
     int 4)


    (modules-two-vals-bad-interface-14
     "module m interface
            [v : int
             u : bool]
          body
           [v = zero?(0) u = 33]
         import m
         -(from m take u, from m take v)"
     error)


    (modules-check-let*-1
     "module m interface      [u : int v : int]
                  body [u = 44  v = -(u,11)]
         import m
         -(from m take u, from m take v)"
     int 11)

    (modules-check-let*-2.0
     "module m1 interface [u : int] body [u = 44]
         module m2 interface [v : int]
          body
           import m1
           [v = -(from m1 take u,11)]
         import m1, m2
         -(from m1 take u, from m2 take v)"
     int 11)

    (modules-check-let*-2.05
     "module m1 interface [u : int] body [u = 44]
         module m2 interface [v : int] body import m1 [v = -(from m1 take u,11)]
         33"
     int 33)                       ; doesn't actually import anything

    (modules-check-let*-2.1
     "module m1 interface [u : int] body [u = 44]
         module m2
          interface [v : int]
          body import m1 [v = -(from m1 take u,11)]
         import m1, m2
         -(from m1 take u, from m2 take v)"
     int 11)

    (modules-check-let*-2.2
     "module m2
          interface [v : int]
          body
           [v = -(from m1 take u,11)]
         module m1 interface [u : int] body import m1 [u = 44]
         import m1, m2
         -(from m1 take u, from m2 take v)"
     error)

    (modules-check-parameterized-1
     "
        module m1
            interface ((m : [v : int]) => [w : int])
            body
             module-proc (m : [v : int]) [w = -(from m take v, 1)]
        module m2
         interface [v : int]
         body [v = 33]
        module m3
         interface [w : int]
         body
          import m1, m2
          (m1 m2)
        import m3
        from m3 take w"
     int 32)

    (modules-check-parameterized-bad-argument
     "
        module m1
            interface ((m : [v : int]) =>  [w : int])
            body
             module-proc (m : [v : int]) [w = from m take v]
        module m2 interface [u : int] body [u = 33]
        module m3
         interface [w : int]
         body
           import m1, m2
           (m1 m2)
        import m3
        from m3 take w"
     error)

    (modules-check-parameterized-bad-interface-1
     "
        module m1
            interface ((m : [v : int])  => [w : int])
            body module-proc (m : [v : int]) [w = from m take v]
        module m2 interface [v : int] body [x = 33]  % bad
        module m3 interface [w : int] body  (m1 m2)
        from m3 take w"
     error)

    (modules-check-parameterized-2
     "
        module m1
         interface
          ((m : [v : int]) => [u : int])
         body
          module-proc (m : [v : int]) [w = from m take v]
        module m2
         interface [v : int]
         body [v = 33]
        module m3   interface [w : int] body
         import m1, m2
         (m1 m2)
        import m3
        from m3 take w"
     error)

    (modules-export-abs-type-1
     "module m1 interface [opaque t] body [type t = int]
         33"
     int 33)

    (modules-take-from-ints-0.1
     "module m1
          interface [opaque t
             zero : t]
          body [type t = int
             zero = 0]
         33"
     int 33)

    (modules-take-from-ints-0.1a
     "module m1
          interface [opaque t
             zero : t]
          body [type t = int
             zero = 0]
         import m1
         from m1 take zero"
     (m1 t) 0)

    (modules-take-from-ints-0.1.91
     "module m1
            interface [opaque t
               zero : t]
            body [type t = int
               zero = 0
               foo = 3]
         import m1
         let check = proc (x : from m1 take t) zero?(0)
         in (check  from m1 take zero)"
     bool #t)

    (modules-take-from-ints-0.1.91a
     "module m1
            interface [opaque t
               zero : t]
            body [type t = int
               zero = 0
               foo = 3]
         import m1
         let check = proc (x : from m1 take t  ) zero?(0)
         in check"
     ((m1 t) -> bool))

    (modules-take-from-ints-0.2
     "module m1
          interface [opaque t
             zero : t
             check : (t -> bool)]
          body [type t = int
             zero = 0
             check = proc(x : t) zero?(x)]
         import m1
         (from m1 take check  from m1 take zero)"
     bool #t)

    (modules-mybool-1
     "module mybool
          interface [opaque t
             true : t
             false : t
             and : (t -> (t -> t))
             not : (t -> t)
             to-bool : (t -> bool)]
          body [type t = int
             true = 0
             false = 1
             and = proc (x : t) proc (y : t)
                        if zero?(x) then y else false
             not = proc (x : t) if zero?(x) then false else true
             to-bool = proc (x : t)
                            if zero?(x) then zero?(0) else zero?(1)
             ]
          import mybool
          (from mybool take to-bool
           from mybool take false)
          "
     bool #f)

    (modules-mybool-1a
     "module mybool
          interface [opaque t
             true : t
             false : t
             and : (t -> (t -> t))
             not : (t -> t)
             to-bool : (t -> bool)]
          body [type t = int
             true = 0
             false = 1
             and = proc (x : t) proc (y : t)
                        if zero?(x) then y else false
             not = proc (x : t) if zero?(x) then false else true
             to-bool = proc (x : t)
                            if zero?(x) then zero?(0) else zero?(1)
             ]
          import mybool
          from mybool take to-bool"
     ((mybool t) -> bool))

    (modules-mybool-1b
     "module mybool
          interface [opaque t
             true : t
             false : t
             and : (t -> (t -> t))
             not : (t -> t)
             to-bool : (t -> bool)]
          body [type t = int
             true = 0
             false = 1
             and = proc (x : t) proc (y : t)
                        if zero?(x) then y else false
             not = proc (x : t) if zero?(x) then false else true
             to-bool = proc (x : t)
                            if zero?(x) then zero?(0) else zero?(1)
             ]
          import mybool
          from mybool take false
          "
     (mybool t) )

    (modules-take-from-ints-1
     "module ints-1
          interface [opaque t
             zero : t
             succ : (t -> t)
             pred : (t -> t)
             check : (t -> bool)]
          body [type t = int
             zero = 0
             succ = proc(x : t) -(x,-1)
             pred = proc(x : t) -(x,1)
             check = proc(x : t) zero?(0)]
         import ints-1
         let z = from ints-1 take zero
         in let s = from ints-1 take succ
         in let p = from ints-1 take pred
         in let check = from ints-1 take check
         in (check (s (s (p (s z)))))"
     bool #t)

    (modules-take-from-ints-1a
     "module ints-1
          interface [opaque t
             zero : t
             succ : (t -> t)
             pred : (t -> t)
             check : (t -> bool)]
          body [type t = int
             zero = 0
             succ = proc(x : t) -(x,-1)
             pred = proc(x : t) -(x,1)
             check = proc(x : t) zero?(0)]
         import ints-1
         let z = from ints-1 take zero
         in let s = from ints-1 take succ
         in let p = from ints-1 take pred
         in let check = from ints-1 take check
         in s"
     ((ints-1 t) -> (ints-1 t)))


    (modules-take-from-ints-1b
     "module ints-1
          interface [opaque t
             zero : t
             succ : (t -> t)
             pred : (t -> t)
             check : (t -> bool)]
          body [type t = int
             zero = 0
             succ = proc(x : t) -(x,-1)
             pred = proc(x : t) -(x,1)
             check = proc(x : t) zero?(0)]
         import ints-1
         let z = from ints-1 take zero
         in let s = from ints-1 take succ
         in let p = from ints-1 take pred
         in let check = from ints-1 take check
         in check"
     ((ints-1 t) -> bool))


    (modules-take-from-ints-2
     "module ints-1
          interface [opaque t
             zero : t
             succ : (t -> t)
             pred : (t -> t)
             is-zero : (t -> bool)]
          body [type t = int
             zero = 0
             succ = proc(x : t) -(x,-1)
             pred = proc(x : t) -(x,1)
             is-zero = proc (x : t) zero?(x)]
         import ints-1
         let z = from ints-1 take zero
         in let s = from ints-1 take succ
         in let p = from ints-1 take pred
         in let z? = from ints-1 take is-zero
         in if (z? (s z)) then 22 else 33"
     int 33)


    (modules-take-from-ints-2-bad-1
     "module ints-1
          interface [opaque t
             zero : t
             succ : (t -> t)
             pred : (t -> t)
             is-zero : (t -> bool)]
          body [zero = proc (x : t) x
             succ = proc(x : t) -(x,-1)
             pred = proc(x : t) -(x,1)
             is-zero = proc (x : t) zero?(x)
             ]
         import ints-1
         let z = from ints-1 take zero
         in let s = from ints-1 take succ
         in let p = from ints-1 take pred
         in let z? = from ints-1 take is-zero
         in if (z? (s z)) then 22 else 33"
     error)

    (modules-take-from-ints-3
     "module ints-1
          interface [opaque t
             zero : t
             succ : (t -> t)
             pred : (t -> t)
             is-zero : (t -> int)]
          body [zero = 0
             succ = proc(x : t) -(x,-1)
             pred = proc(x : t) -(x,1)
             is-zero = proc (x : t) zero?(x)]
         import ints-1
         let z = from ints-1 take zero
         in let s = from ints-1 take succ
         in let p = from ints-1 take pred
         in let z? = from ints-1 take is-zero
         in if (z? (s z)) then 22 else 33"
     error)

    (modules-check-polymorphism-1
     "
     module m interface [opaque t
                 f : (t -> t)]
              body [type t = int
                 f = proc (x : t) x]
     import m
     from m take f"
     ((m t) -> (m t)))


    (modules-check-polymorphism-1a
     "
     module m interface [opaque t
                 f : (t -> t)]
              body [type t = int
                 f = proc (x : t) x]
     import m
     from m take f"
     ((m t) -> (m t)))

    (modules-check-polymorphism-1b
     "
     module m interface [opaque t
                 f : (t -> t)]
              body [type t = int
                 f = proc (x : t) -(x,1)]
     import m
     from m take f"
     ((m t) -> (m t)))

    ;; in exercise 8.27, this fails because the program body depends on ints-2,
    ;; which depends on ints-1. This makes the program body implicitly depends
    ;; on ints-1. But the checker does not resolve this implicit
    ;; dependencies. The program just fails to pass the type checking. To solve
    ;; this problem, one way is to allow implicit dependency. But this might
    ;; require a whole traverse of 'dependency graph'.
    (modules-check-shadowing-1
     "
      module ints-1
       interface
        [opaque t
          zero : t
          succ : (t -> t)
          is-zero : (t -> bool)]
       body
        [type t = int
         zero = 0
         succ = proc(x : t) -(x,-1)
         pred = proc(x : t) -(x,1)
         is-zero = proc (x : t) zero?(x)]
      module ints-2
       interface
        [zero : from ints-1 take t
         succ : (from ints-1 take t -> from ints-1 take t)
         is-zero : (from ints-1 take t -> bool)]
       body
        import ints-1
        [zero = from ints-1 take zero
         succ = from ints-1 take succ
         is-zero = from ints-1 take is-zero]
      import ints-2
      let s = from ints-2 take succ
      in let z? = from ints-2 take is-zero
      in let z = from ints-2 take zero
      in (z? (s z))"
     bool #f)


    (modules-check-shadowing-1.8
     "
      module ints-1
       interface
        [opaque t
         zero : t]
       body
        [type t = int
         zero = 0]
      module ints-2
       interface
        [foo : from ints-1 take t]
       body
        import ints-1
        [foo = from ints-1 take zero]
      import ints-2
      let v = from ints-2 take foo
      in 33
     " int 33)

    (modules-check-shadowing-1.8a
     "module ints-1
       interface [opaque t  zero : t]
       body [type t = int zero = 0]
      module ints-2
       interface [ foo : from ints-1 take t]
       body
        import ints-1
        [foo = from ints-1 take zero]
      import ints-1, ints-2
      from ints-2 take foo
     "
     (ints-1 t))

    ;; Once exercise 8.1 (reject duplicated module names) is done, the
    ;; test should be:

    (modules-check-shadowing-1.9.2
     "module ints-1 interface [opaque t  zero : t]
                      body [type t = int zero = 0]
          module ints-1 interface [foo : from ints-1 take t]
                      body import ints-1
                       [foo = from ints-1 take zero]
          import ints-1
          let v = from ints-1 take foo
          in 33
         " error)    ; <<<---- changed outcome.


    ;; This is bogus in yet another way.  In the following example, v
    ;; has the type of from ints-1 take foo, which is from ints-1 take
    ;; t.  But at the point where v is used, ints-1 has been rebound,
    ;; and doesn't even have a type component t.

    (modules-check-shadowing-1.9.2
     "module ints-1 interface [opaque t  zero : t]
                      body [type t = int zero = 0]
          module ints-1 interface [foo : from ints-1 take t]
                      body import ints-1
                       [foo = from ints-1 take zero]
          import ints-1
          let v = from ints-1 take foo
          in v
         "
     error)


    ;; We can take advantage of this confusion to generate an unsound
    ;; program that type-checks:

    (modules-check-shadowing-1.9.3
     "
    module ints-1
     interface [opaque t zero : t]
     body [type t = int zero = 0]
    module ints-1
     interface [zero : from ints-1 take t
                opaque t
                f : (t -> int)]
     body import ints-1
          [zero = from ints-1 take zero
           type t = bool
           f = proc (b : t) if b then 33 else 44]
     import ints-1, ints-1
     (from ints-1 take f
      from ints-1 take zero)"
     error)

    ;; this code allows the application of ints-1.f because its type is
    ;; (ints-1.t -> int), and zero has type ints-1.t .  But those are
    ;; two different modules both named ints-1.

    ;; In general, the solution is to rename the inner ints-1 to avoid
    ;; the conflict.  Exercise: do this. When you do this,
    ;; modules-check-shadowing-1.9.3 should give back "error".

    ;; Aren't you sorry you asked?

    (modules-apply-param-module-0.1
     "module copy-module
         interface
          ((m : [opaque t zero : t]) =>
           [opaque t
            zero : t])
         body
          module-proc (n : [opaque t zero : t])
           [type t = from n take t
            zero = from n take zero]
        33"
     int 33)

    (modules-apply-param-module-1
     "module makeints
         interface
          ((m : [opaque t
             zero : t
             succ : (t -> t)
             pred : (t -> t)
             is-zero : (t -> bool)])
            => [opaque t
                zero : t
                succ : (t -> t)
                pred : (t -> t)
                is-zero : (t -> bool)])
         body
          module-proc (m : [opaque t
                            zero : t
                            succ : (t -> t)
                            pred : (t -> t)
                            is-zero : (t -> bool)])
           [type t = from m take t
            zero = from m take zero
            succ = proc (x : t)
                        (from m take succ (from m take succ x))
            pred = proc (x : t)
                        (from m take pred (from m take pred x))
            is-zero = proc (x : t) (from m take is-zero x)]

        module ints-1
         interface
           [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)]
         body
           [type t = int
            zero = 0
            succ = proc(x : t) -(x,2)
            pred = proc(x : t) -(x,-2)
            is-zero = proc (x : t) zero?(x)]

       module ints-2
         interface
           [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)]
         body
          import makeints, ints-1
          (makeints ints-1)

        import ints-2
        let check = proc (x : from ints-2 take t) zero?(0)
        in (check
             (from ints-2 take succ
               (from ints-2 take succ from ints-2 take zero)))"
     bool #t)

    (transparent-0
     "module m1 interface [transparent t = int
                   zero : t]
                body [type t = int
                   zero = 0]
      import m1
      -(from m1 take zero,1)"
     int)

    (transparent-1
     "module m1
       interface [opaque t zero : t]
       body [type t = int zero = 0]
      module m2
       interface [transparent t = from m1 take t   % don't know
                                           % what's in m1!
          one : t]
       body [type t = int
          one = 1]
      import m1, m2
     -(from m2 take one, from m1 take zero)
     "
     error)

    (transparent-2
     "module m1
      interface
       [transparent t = int
        zero : t]
      body
       [type t = int
        zero = 0]

     module m2
      interface
       [transparent t = from m1 take t   % now known to be int.
        one : t]
      body
        [type t = int
         one = 1]
      import m1, m2
      -(from m2 take one, from m1 take zero)
     "
     int 1)

    (modules-add-double-1
     "module add-double
         interface
          ((m : [opaque t
             zero : t
             succ : (t -> t)
             pred : (t -> t)
             is-zero : (t -> bool)])
           => [double : (from m take t -> from m take t)])
         body
          module-proc (m : [opaque t
                            zero : t
                            succ : (t -> t)
                            pred : (t -> t)
                            is-zero : (t -> bool)])
          [double
            = letrec
               from m take t double (x : from m take t)
                = if (from m take is-zero x)
                  then from m take zero
                  else (from m take succ
                        (from m take succ x))
              in double]

        module ints-1
         interface
           [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)]
         body
           [type t = int
            zero = 0
            succ = proc(x : t) -(x,2)
            pred = proc(x : t) -(x,-2)
            is-zero = proc (x : t) zero?(x)
            ]

        module ints-2
         interface [double : (from ints-1 take t -> from ints-1 take t)]
         body
           import add-double, ints-1
           (add-double ints-1)

        import ints-1, ints-2
        (from ints-1 take is-zero
          (from ints-2 take double
            (from ints-1 take succ
              from ints-1 take zero)))"
     bool
     #f
     )

    ;; this example shows the need for substitution in types in a module
    ;; application.   This also means you need to have the bound
    ;; variable in the type of a parameterized module.

    (diamond-1
     "
     module maker1
      interface
       ((m : [opaque t
              succ : (t -> t)])
        => [transparent t = from m take t
            double : (t -> t)])
      body
       module-proc (m : [opaque t succ : (t -> t)])
        [type t = from m take t
         double = let s = from m take succ
                  in proc (x : t) (s (s x))]

     module m0
      interface
       [opaque t
        succ : (t -> t)
        zero : t]
      body
       [type t = int
        succ = proc(x : t)-(x,-1)
        zero = 0]

     module m2
      interface
       [transparent t = from m0 take t
        double : (t -> t)]
      body
       import maker1, m0
       (maker1 m0)

     import m0, m2
     let check = proc (x : from m0 take t) zero?(0)
     in (check
          (from m2 take double
           from m0 take zero))
     "
     bool #t)

    (pass-around-ho-module-1
     "
       module m1
        interface
         ((m : [v : int]) =>  [u : int])
        body
         module-proc (m : [v : int])
          [u = from m take v]

       module m2
        interface [v : int]
        body [v = 33]

       module m1a
        interface ((m : [v : int]) => [u : int])
        body
         m1

       module m2a
         interface [v : int]
         body
          m2

        module m3
         interface [u : int]
         body
          import m1a, m2a
          (m1a m2a)

        import m3
        from m3 take u"
     int 33)

    (modules-myints-0.1
     "
        module ints-1
         interface [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)]
         body [type t = int
            zero = 0
            succ = proc(x : t) -(x,-2)
            pred = proc(x : t) -(x,2)
            is-zero = proc (x : t) zero?(x)
            ]
         import ints-1
         let zero = from ints-1 take zero
         in let succ = from ints-1 take succ
         in let is-zero = from ints-1 take is-zero
         in (succ (succ zero))"
     (ints-1 t)
     4)

    (modules-myints-0.20
     "
        module ints-1
         interface [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)]
         body [zero = 0
            succ = proc(x : t) -(x,2)
            pred = proc(x : t) -(x,-2)
            is-zero = proc (x : t) zero?(x)
            ]
         let zero = from ints-1 take zero
         in let succ = from ints-1 take succ
         in let is-zero = from ints-1 take is-zero
         in (succ (succ zero))"
     error
     -4)


    (modules-myints-0.2a
     "
        module ints-1
         interface [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)]
         body [type t = int
            zero = 0
            succ = proc(x : t) -(x,2)
            pred = proc(x : t) -(x,-2)
            is-zero = proc (x : t) zero?(x)
            ]
         import ints-1
         let zero = from ints-1 take zero
         in let succ = from ints-1 take succ
         in let is-zero = from ints-1 take is-zero
         in (succ (succ zero))"
     (ints-1 t) -4)

    (modules-apply-param-module-1
     "
        module makeints
         interface
         ((m: [opaque t
                            zero : t
                            succ : (t -> t)
                            pred : (t -> t)
                            is-zero : (t -> bool)])
          => [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)])
         body
          module-proc (m: [opaque t
                            zero : t
                            succ : (t -> t)
                            pred : (t -> t)
                            is-zero : (t -> bool)])
           [type t = from m take t
            zero = from m take zero
            succ = proc (x : t)
                        (from m take succ (from m take succ x))
            pred = proc (x : t)
                        (from m take pred (from m take pred x))
            is-zero = proc (x : t) (from m take is-zero x)
           ]

        module ints-1
         interface
           [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)]
         body
           [type t = int
            zero = 0
            succ = proc(x : t) -(x,2)
            pred = proc(x : t) -(x,-2)
            is-zero = proc (x : t) zero?(x)]

       module ints-2
         interface
           [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)]
         body
          import ints-1, makeints
          (makeints ints-1)

        import ints-2
        (from ints-2 take succ
         (from ints-2 take succ
          from ints-2 take zero)) "
     (ints-2 t)
     -8)


    (modules-apply-param-module-3
     "module makeints
         interface
          ((n : [opaque t
                 zero : t
                 succ : (t -> t)
                 pred : (t -> t)
                 is-zero : (t -> bool)])
           => [opaque t
               zero : t
               succ : (t -> t)
               pred : (t -> t)
              is-zero : (t -> bool)])
         body
          module-proc (m : [opaque t
                            zero : t
                            succ : (t -> t)
                            pred : (t -> t)
                            is-zero : (t -> bool)])
           [type t = from m take t
            zero = from m take zero
            succ = proc (x : t)
                    (from m take succ (from m take succ x))
            pred = proc (x : t)
                    (from m take pred (from m take pred x))
            is-zero = proc (x : t) (from m take is-zero x)
           ]

        module ints-1
         interface
          [opaque t
           zero : t
           succ : (t -> t)
           pred : (t -> t)
           is-zero : (t -> bool)]
         body
          [type t = int
           zero = 0
           succ = proc(x : t) -(x,2)
           pred = proc(x : t) -(x,-2)
           is-zero = proc (x : t) zero?(x)]
        module ints-2
         interface
          [opaque t
           zero : t
           succ : (t -> t)
           pred : (t -> t)
           is-zero : (t -> bool)]
         body
          import ints-1, makeints
          (makeints ints-1)

        import ints-2
        let zero = from ints-2 take zero
        in let succ = from ints-2 take succ
        in let pred = from ints-2 take pred
        in let is-zero = from ints-2 take is-zero
        in letrec int to-int (n : from ints-2 take t)
                           = if (is-zero n)
                              then 0
                              else -( (to-int (pred n)), -1)
        in (to-int (succ (succ zero)))
         "
     int
     2)


    (modules-apply-param-module-4
     "
        module makeints
         interface
          ((m : [opaque t
                 zero : t
                 succ : (t -> t)
                 pred : (t -> t)
                 is-zero : (t -> bool)])
           => [opaque t
               zero : t
               succ : (t -> t)
               pred : (t -> t)
               is-zero : (t -> bool)])
         body
          module-proc (m : [opaque t
                            zero : t
                            succ : (t -> t)
                            pred : (t -> t)
                            is-zero : (t -> bool)])
           [type t = from m take t
            zero = from m take zero
            succ = proc (x : t)
                    (from m take succ (from m take succ x))
            pred = proc (x : t)
                    (from m take pred (from m take pred x))
            is-zero = proc (x : t) (from m take is-zero x)
           ]

        module ints-1
         interface
          [opaque t
           zero : t
           succ : (t -> t)
           pred : (t -> t)
           is-zero : (t -> bool)]
         body
          [type t = int
           zero = 0
           succ = proc(x : t) -(x,2)
           pred = proc(x : t) -(x,-2)
           is-zero = proc (x : t) zero?(x)]

        module ints-2
         interface
          [opaque t
           zero : t
           succ : (t -> t)
           pred : (t -> t)
           is-zero : (t -> bool)]
         body
          import ints-1, makeints
          (makeints ints-1)

        module int3
         interface
          [opaque t
           zero : t
           succ : (t -> t)
           pred : (t -> t)
           is-zero : (t -> bool)]
         body
          import ints-2, makeints
          (makeints ints-2)

        import int3
        (from int3 take succ
         (from int3 take succ  from int3 take zero))
"
     (int3 t)
     -16)

    (lift-type-from-scope-0.01
     "
       module m1
        interface
         [transparent u = int
          transparent t = int]
        body
         [type u = int
          type t = u]
      module m2
       interface
        [transparent u = int
         x : from m1 take t]
       body
        [type u = int
         x = 3]

       33"
     int
     33)

    (lift-type-from-scope-0.1
     "
       module m1
        interface
         [transparent u = int
          transparent t = u]
        body
         [type u = int
          type t = u]
       module m2
        interface
         [transparent u = int
          x : from m1 take t]
        body
         [type u = int
          x = 3]

       33"
     int
     33)

    (lift-type-from-scope-1
     "
       module m1
        interface
         [opaque u
          transparent t = u]
        body
         [type u = bool
          type t = u]
       module m2
        interface
         [transparent u = int
          x : from m1 take t]
        body
         [type u = int
          x = 3]

       33"
     error
     33)

    (lift-type-from-scope-2
     "
       module m1
        interface
         [opaque t1
          f : (t1 -> t1)]
        body
         [type t1 = bool
          f = proc (x : t1) x]

       import m1
       from m1 take f"
     ((m1 t1) -> (m1 t1))
     )

    (lift-type-from-scope-3
     "
       module m1
        interface
         [opaque t2
          f : (t1 -> t1)]
        body
         [type t1 = bool
          f = proc (x : t1) x]

       import m1
       from m1 take f"
     error                             ; this should die because t1
                                        ; is unbound.
     )

    (modules-14.1
     "
      module m1 interface
                 [transparent t = int
                  z : t]
                body
                  [type t = int
                   z = 0]

      module m2
       interface
        [foo : (from m1 take t -> int)]
       body
        [foo = proc (x : from m1 take t) x]

      import m2
      (from m2 take foo  33)"
     int)

    (modules-14
     "
      module m1
       interface
        [transparent t = int
         z : t]
       body
        [type t = int
         z = 0]
      module m2
       interface
        [foo : (from m1 take t -> int)]
       body import m1
        [foo = proc (x : from m1 take t) x]

      import m2
      from m2 take foo"
     (int -> int))


    (modules-14b
     "
module m1 interface [transparent t1 = int] body [type t1 = int]
module m2 interface [foo : from m1 take t1] body [foo = 3]
import m2
from m2 take foo"
     int)

    (modules-test-curry1
     "
      module maker1
       interface
        ((m1 : [opaque t
                s : (t -> t)])
         => [transparent t = from m1 take t
             d : (t -> t)])
       body
        module-proc
         (m1 : [opaque t
                s : (t -> t)])
         [type t = from m1 take t
          d = proc (x : t) (from m1 take s (from m1 take s x))]

       module m0
        interface
         [opaque t
          s : (t -> t)]
        body
         [type t = int
          s = proc (u : t) -(u, -1)]

        module m1
         interface
          [opaque t
           d : (t -> t)]
         body
          import m0, maker1
          (maker1 m0)

        33" int 33)

    (modules-test-curry2
     "
      module maker1
       interface
        ((m1 : [opaque t
                s : (t -> t)])
         => ((m2 : [transparent t = from m1 take t])
              => [transparent t = from m1 take t
                  d : (t -> t)]))
       body
        module-proc
         (p1 : [opaque t
                s : (t -> t)])
         module-proc
          (p2 : [transparent t = from p1 take t])
          [type t = from p1 take t
           d = proc (x : t) (from p1 take s (from p1 take s x))]

       module m0
        interface
         [opaque t
          s : (t -> t)]
        body
         [type t = int
          s = proc (u : t) -(u, -1)]

       module m1
        interface
         ((m2 : % [opaque t]
            [transparent t = from m0 take t])
          => [transparent t = from m2 take t
              d : (t -> t)])
        body
         import m0, maker1
         (maker1 m0)

         module m2
          interface
           [opaque t
            d : (t -> t)]
          body
           import m1, m0
           (m1 m0)

        33"
     int 33)

    ;; Here are some possible tests for named interfaces (Ex. 8.27)

    (modules-named-interfaces-1
     "
            interface i1 = [u : int v: bool]
            module m1
             interface i1
             body [u = 3 v = zero?(0)]
            import m1
            from m1 take u"
     int)

    (modules-named-interfaces-2
     "
            interface i1 = [u : int v: bool]
            module m1
             interface i1
             body [u = 3 v = zero?(0)]
            module m2
             interface ((m3 : i1) => [u : int])
             body
              module-proc (m4 : i1) [u = from m4 take u]
            module builder
             interface [u:int]
             body
             import m1, m2
             (m2 m1)

            import builder
            from builder take u"
     int)

    (modules-named-interfaces-3
     "
            interface i1 = [u : int v: bool]
            interface i2 = ((m3 : i1) => [u : int])
            module m1
             interface i1
             body [u = 3 v = zero?(0)]
            module m2
             interface i2
             body
              module-proc (m4 : i1) [u = from m4 take u]
            module builder
             interface [u:int]
             body
             import m1, m2
             (m2 m1)

            import builder
            from builder take u"
     int)

    )
  )

(define tests-for-run
  (let loop ((lst the-test-suite))
    (cond
     ((null? lst) '())
     ((>= (length (car lst)) 4)
      ;; (printf "creating item: ~s~%" (caar lst))
      (cons
       (list
        (list-ref (car lst) 0)
        (list-ref (car lst) 1)
        (list-ref (car lst) 3))
       (loop (cdr lst))))
     (else (loop (cdr lst))))))

;; ok to have extra members in a test-item.
(define tests-for-check the-test-suite)

(define tests-for-parse
  (let loop ((lst the-test-suite))
    (cond
     ((null? lst) '())
     ((> (length (car lst)) 4)
      (cons
       (list
        (list-ref (car lst) 0)
        (list-ref (car lst) 1)
        (list-ref (car lst) 4))
       (loop (cdr lst))))
     (else
      (cons
       (list
        (list-ref (car lst) 0)
        (list-ref (car lst) 1)
        #t)
       (loop (cdr lst)))))))

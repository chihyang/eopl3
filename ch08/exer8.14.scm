#lang eopl

(provide test-for-exer8.14)

(define test-for-exer8.14
  '(
    (mybool-definition-1
     "module mybool
 interface
  [opaque t
   true : t
   false : t
   and : (t -> (t -> t))
   not : (t -> t)
   to-bool : (t -> bool)]
 body
  [type t = int
   true = 0
   false = 13
   and = proc (x : t)
          proc (y : t)
             if zero?(x) then y else false
   not = proc (x : t)
               if zero?(x)
               then false else true
   to-bool = proc (x : t) zero? (x)]
 in (from mybool take and
     from my bool take true
     from my bool take true)
"
     int 0)

    (mybool-definition-2
     "module mybool
 interface
  [opaque t
   true : t
   false : t
   and : (t -> (t -> t))
   not : (t -> t)
   to-bool : (t -> bool)]
 body
  [type t = int
   true = 1
   false = 0
   and = proc (x : t)
          proc (y : t)
             if zero?(x) then false else y
   not = proc (x : t)
               if zero?(x)
               then true else false
   to-bool = proc (x : t) zero? (x)]
 in (from mybool take and
     from my bool take true
     from my bool take true)
"
     int 1)
    ))

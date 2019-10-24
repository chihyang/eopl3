#lang eopl

(provide test-for-exer8.12)

(define test-for-exer8.12
  '(
    ;; move 'and' and 'or' outside of the module is okay, but to-bool
    ;; cannot be moved outside
    (move-and-or-outside-of-module
     "module mybool
 interface
  [opaque t
   true : t
   false : t
   to-bool : (t -> bool)]
 body
  [type t = int
   true = 0
   false = 13
   to-bool = proc (x : t) zero? (x)]
 let and = proc (x : from mybool take t)
            proc (y : from mybool take t)
             if (from mybool take to-bool x)
             then (from mybool take to-bool y)
             else (from mybool take to-bool from mybool take false)
 in let not = proc (x : from mybool take t)
               if (from mybool take bo-bool x)
               then (from mybool take bo-bool from mybool take false)
               else (from mybool take bo-bool from mybool take true)
 in (not (and (from mybool take true)
              (from mybool take false)))
"
     bool #t)
    ))

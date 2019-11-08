#lang eopl

(define test-for-exer8.19-23
  '(
    ;; exercise 8.19
    (exer-8.12
     "
module to-int-maker
 interface
 ((ints : [opaque t
           zero : t
           succ : (t -> t)
           pred : (t -> t)
           is-zero : (t -> bool)])
  => [to-int : (from ints take t -> int)])
 body
  module-proc (ints : [opaque t
                       zero : t
                       succ : (t -> t)
                       pred : (t -> t)
                       is-zero : (t -> bool)])
   [to-int
    = let z? = from ints take is-zero
      in let p = from ints take pred
      in letrec int to-int (x : from ints take t)
                     = if (z? x)
                       then 0
                       else - ((to-int (p x)), -1)
      in to-int]
module ints1
 interface
  [opaque t
   zero : t
   succ : (t -> t)
   pred : (t -> t)
   is-zero : (t -> bool)]
 body
  [type t = int
   zero = 0
   succ = proc (x : t) -(x, -5)
   pred = proc (x : t) -(x, 5)
   is-zero = proc (x : t) zero?(x)]
module ints2
 interface
  [opaque t
   zero : t
   succ : (t -> t)
   pred : (t -> t)
   is-zero : (t -> bool)]
 body
  [type t = int
   zero = 0
   succ = proc (x : t) -(x, 3)
   pred = proc (x : t) -(x, -3)
   is-zero = proc (x : t) zero?(x)]
module from-int-maker
 interface
  ((ints : [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)])
  => [from-int : (int -> from ints take t)])
 body
  module-proc (ints : [opaque t
                       zero : t
                       succ : (t -> t)
                       pred : (t -> t)
                       is-zero : (t -> bool)])
   [from-int =
             letrec t from-int (n : int)
              if zero?(t)
              then zero
              else (succ (from-int -(n, 1)))
             in from-int]
module ints1-to-int
 interface [to-int : (from ints1 take t -> int)]
 body (to-int-maker ints1)
module ints2-to-int
 interface [to-int : (from ints2 take t -> int)]
 body (to-int-maker ints2)
module int-to-ints1
 interface [to-int1 : (int -> from ints1 take t)]
 body (from-int-maker ints1)
module int-to-ints2
 interface [to-int2 : (int -> from ints2 take t)]
 body (from-int-maker ints2)
% convert between ints1, ints2 and int
let n1 = (from int-to-ints1 take to-int1 39)
in let n2 = (from int-to-ints2 take to-int2 39)
   in zero?(-((from to-int-ints1 n1),
              (from to-int-ints2 n2)))
"
     bool #t
     )))

#lang eopl

(provide tests-for-parse tests-for-check tests-for-run)

(define test-for-exer8.19-23
  '(
    ;; exercise 8.19
    (exer-8.19
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
             letrec from ints take t from-int (n : int) =
              if zero?(n)
              then from ints take zero
              else (from ints take succ (from-int -(n, 1)))
             in from-int]
module ints1-to-int
 interface [to-int : (from ints1 take t -> int)]
 body (to-int-maker ints1)

module int-to-ints1
 interface [from-int : (int -> from ints1 take t)]
 body (from-int-maker ints1)

module ints2-to-int
 interface [to-int : (from ints2 take t -> int)]
 body (to-int-maker ints2)

module int-to-ints2
 interface [from-int : (int -> from ints2 take t)]
 body (from-int-maker ints2)

% convert between ints1, ints2 and int
let n1 = (from int-to-ints1 take from-int 3)
in let n2 = (from int-to-ints2 take from-int 3)
   in zero?(-((from ints1-to-int take to-int n1),
              (from ints2-to-int take to-int n2)))
"
     bool #t)

    ;; exercise 8.20
    (
     exer-8.20
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
             letrec from ints take t from-int (n : int) =
              if zero?(n)
              then from ints take zero
              else (from ints take succ (from-int -(n, 1)))
             in from-int]

module sum-prod-marker
 interface
  ((ints : [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)])
   => [plus : (from ints take t
               -> (from ints take t
                   -> from ints take t))
       times : (from ints take t
                -> (from ints take t
                    -> from ints take t))])
 body
  module-proc (ints : [opaque t
                       zero : t
                       succ : (t -> t)
                       pred : (t -> t)
                       is-zero : (t -> bool)])
   [plus = letrec (from ints take t -> from ints take t) plus(x : from ints take t) =
            proc (y : from ints take t)
             if (from ints take is-zero x)
             then y
             else (from ints take succ ((plus (from ints take pred x)) y))
            in plus
    times = letrec (from ints take t -> from ints take t) times(x : from ints take t) =
             proc (y : from ints take t)
              if (from ints take is-zero x)
              then from ints take zero
              else ((plus y) ((times (from ints take pred x)) y))
             in times]
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

module ints1-to-int
 interface [to-int : (from ints1 take t -> int)]
 body (to-int-maker ints1)

module int-to-ints1
 interface [from-int : (int -> from ints1 take t)]
 body (from-int-maker ints1)

module ints1-arithmetic
 interface [plus : (from ints1 take t -> (from ints1 take t -> from ints1 take t))
            times : (from ints1 take t -> (from ints1 take t -> from ints1 take t))]
 body (sum-prod-marker ints1)

module ints2-to-int
 interface [to-int : (from ints2 take t -> int)]
 body (to-int-maker ints2)

module int-to-ints2
 interface [from-int : (int -> from ints2 take t)]
 body (from-int-maker ints2)

module ints2-arithmetic
 interface [plus : (from ints2 take t -> (from ints2 take t -> from ints2 take t))
            times : (from ints2 take t -> (from ints2 take t -> from ints2 take t))]
 body (sum-prod-marker ints2)

let n1-8 = (from int-to-ints1 take from-int 8)
in let n1-2 = (from int-to-ints1 take from-int 2)
in let n1-plus = ((from ints1-arithmetic take plus n1-8) n1-2)
in let n1-times =  ((from ints1-arithmetic take times n1-8) n1-2)
in let n2-8 = (from int-to-ints2 take from-int 8)
in let n2-2 = (from int-to-ints2 take from-int 2)
in let n2-plus = ((from ints2-arithmetic take plus n2-8) n2-2)
in let n2-times =  ((from ints2-arithmetic take times n2-8) n2-2)
in zero?(-((from ints1-to-int take to-int n1-plus),
           (from ints2-to-int take to-int n2-plus)))
"
     bool #t)

    (exer-8.21
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

module ints-maker
 interface
  ((ints : [opaque t
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
  module-proc (ints : [opaque t
                       zero : t
                       succ : (t -> t)
                       pred : (t -> t)
                       is-zero : (t -> bool)])
  [type t = from ints take t
   zero = from ints take zero
   succ = proc (x : t)
           (from ints take succ (from ints take succ x))
   pred = proc (x : t)
           (from ints take pred (from ints take pred x))
   is-zero = proc (x : t)
              (from ints take is-zero x)]
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

module ints1-to-int
 interface [to-int : (from ints1 take t -> int)]
 body (to-int-maker ints1)

module ints2
 interface [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)]
 body (ints-maker ints1)

module ints2-to-int
 interface [to-int : (from ints2 take t -> int)]
 body (to-int-maker ints2)

let n1 = (from ints1 take succ from ints1 take zero)
in let n2 = (from ints2 take succ from ints2 take zero)
in let n3 = (from ints1-to-int take to-int n1)
in let n4 = (from ints2-to-int take to-int n2)
in zero?(-(n3, n4))
"
     bool #t)


    (exer-8.22
     "
module equalify-maker
 interface
  ((ints : [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)])
   => [equal : (from ints take t
                -> (from ints take t
                    -> bool))])
 body
  module-proc (ints : [opaque t
                       zero : t
                       succ : (t -> t)
                       pred : (t -> t)
                       is-zero : (t -> bool)])
  [equal = letrec (from ints take t -> bool) equal(x : from ints take t) =
            proc (y : from ints take t)
             if (from ints take is-zero x)
             then (from ints take is-zero y)
             else ((equal (from ints take pred x)) (from ints take pred y))
            in equal]

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
             letrec from ints take t from-int (n : int) =
              if zero?(n)
              then from ints take zero
              else (from ints take succ (from-int -(n, 1)))
             in from-int]

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

module int-to-ints1
 interface [from-int : (int -> from ints1 take t)]
 body (from-int-maker ints1)

module ints1-equal
 interface [equal : (from ints1 take t
                          -> (from ints1 take t
                              -> bool))]
 body (equalify-maker ints1)

let n1 = (from int-to-ints1 take from-int 2)
in let n2 = (from int-to-ints1 take from-int 2)
in ((from ints1-equal take equal n1) n2)
"
     bool #t)

    (exer-8.23
     "
module table-of
 interface
  ((m : [opaque t
         invalid-val : t])
   => [opaque table
       empty : table
       add-to-table : (int -> (from m take t -> (table -> table)))
       lookup-in-table : (int -> (table -> from m take t))])
 body
  module-proc (m : [opaque t
                    invalid-val : t])
  [type table = (int -> from m take t)
   empty = proc (x : int) from m take invalid-val
   add-to-table =
    proc (k : int)
     proc (v : from m take t)
      proc (t : table)
       proc (f : int)
        if zero?(-(k, f)) then v else (t f)
   lookup-in-table =
     proc (x : int)
      proc (t : table)
       (t x)]

module mybool
 interface [opaque t
            invalid-val : t
            true : t
            false : t
            and : (t -> (t -> t))
            not : (t -> t)
            to-bool : (t -> bool)]
 body [type t = int
       invalid-val = -1
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

module mybool-tables
 interface
 [opaque table
  empty : table
  add-to-table : (int ->
                  (from mybool take t ->
                   (table -> table)))
  lookup-in-table : (int ->
                     (table ->
                      from mybool take t))]
 body
  (table-of mybool)

let empty = from mybool-tables take empty
in let add-binding = from mybool-tables take add-to-table
in let lookup = from mybool-tables take lookup-in-table
in let true = from mybool take true
in let false = from mybool take false
in let table1 = (((add-binding 3) true)
                 (((add-binding 4) false)
                  (((add-binding 3) true)
                   empty)))
in ((lookup 4) table1)
"
     (mybool t) 13)
    ))

(define tests-for-run
  (let loop ((lst test-for-exer8.19-23))
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

(define tests-for-parse
  (let loop ((lst test-for-exer8.19-23))
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
      ;; (printf "creating item: ~s~%" (caar lst))
      (cons
       (list
        (list-ref (car lst) 0)
        (list-ref (car lst) 1)
        #t)
       (loop (cdr lst)))))))

;; ok to have extra members in a test-item.

(define tests-for-check test-for-exer8.19-23)

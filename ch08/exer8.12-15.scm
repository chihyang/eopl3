#lang eopl

(provide tests-for-parse tests-for-check tests-for-run)

(define test-for-exer8.12-15
  '(
    ;; exercise 8.12
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
             then y else false
 in let not = proc (x : from mybool take t)
               if (from mybool take to-bool x)
               then from mybool take false
               else from mybool take true
 in (from mybool take to-bool
     (not ((and from mybool take true)
           from mybool take false)))
"
     bool #t)

    ;; exercise 8.13
    (represent-k-as-k*5+3
     "module myint
 interface
  [opaque t
   zero : t
   succ : (t -> t)
   pred : (t -> t)
   z? : (t -> bool)]
 body
  [type t = int
   zero = 3
   succ = proc (x : t) -(x, -5)
   pred = proc (x : t) -(x, 5)
   z? = proc (x : t) zero?(-(x, 3))]
 let a = (from myint take succ from myint take zero)
 in (from myint take z? (from myint take pred a))
"
     bool #t)

    ;; exercise 8.14
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
 ((from mybool take and
   from mybool take true)
  from mybool take true)
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
 ((from mybool take and
   from mybool take true)
  from mybool take true)
"
     int 1)

    ;; exercise 8.15
    (table-1
     "module tables
 interface
  [opaque table
   empty : table
   add-to-table : (int -> (int -> (table -> table)))
   lookup-in-table : (int -> (table -> int))]
 body
  [type table = (int -> int)
   empty = proc (x : int) 0
   add-to-table =
    proc (k : int)
     proc (v : int)
      proc (t : table)
       proc (f : int)
        if zero?(-(k, f)) then v else (t f)
   lookup-in-table =
     proc (x : int)
      proc (t :table)
       (t x)]
 let empty = from tables take empty
 in let add-binding = from tables take add-to-table
 in let lookup = from tables take lookup-in-table
 in let table1 = (((add-binding 3) 300)
                  (((add-binding 4) 400)
                   (((add-binding 3) 600)
                    empty)))
 in -(((lookup 4) table1),
      ((lookup 3) table1))
"
     int 100)
    ))

(define tests-for-run
  (let loop ((lst test-for-exer8.12-15))
    (cond
     ((null? lst) '())
     ((= (length (car lst)) 4)
      ;; (printf "creating item: ~s~%" (caar lst))
      (cons
       (list
        (list-ref (car lst) 0)
        (list-ref (car lst) 1)
        (list-ref (car lst) 3))
       (loop (cdr lst))))
     (else (loop (cdr lst))))))

(define tests-for-parse
  (let loop ((lst test-for-exer8.12-15))
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

(define tests-for-check test-for-exer8.12-15)

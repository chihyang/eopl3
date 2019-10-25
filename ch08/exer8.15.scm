#lang eopl

(provide test-for-exer8.15)

(define test-for-exer8.15
  '(
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
 in
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

#lang eopl
(require "exer06.11.v1.scm")
(require "exer06.11.v2.scm")
(require rackunit)
;;; removeall
(check-equal?
 (run
  "letrec
     removeall(n,s) =
       if null?(s)
       then emptylist
       else if number?(car(s))
            then if equal?(n,car(s))
                 then (removeall n cdr(s))
                 else cons(car(s), (removeall n cdr(s)))
            else cons((removeall n car(s)), (removeall n cdr(s)))
   in (removeall 2
                 list(1,2,3,list(4,2,5)))")
 (run-cps
  "letrec
     removeall(n, s, cont) =
       if null?(s)
       then (cont emptylist)
       else if number?(car(s))
            then if equal?(n,car(s))
                 then (removeall n cdr(s) cont)
                 else (removeall n cdr(s) proc (val) (cont cons(car(s), val)))
            else (removeall n
                            car(s)
                            proc (val1)
                              (removeall n
                                         cdr(s)
                                         proc (val2) (cont cons(val1, val2))))
   in (removeall 2
                 list(1,2,3,list(4,2,5))
                 proc (val) val)"))

;;; occurs-in?
(check-equal?
 (run
  "letrec
     occurs-in?(n,s) =
       if null?(s)
       then 0
       else if number?(car(s))
            then if equal?(n,car(s))
                 then 1
                 else (occurs-in? n cdr(s))
            else if (occurs-in? n car(s))
                 then 1
                 else (occurs-in? n cdr(s))
   in (occurs-in? 3
                  list(1,3,list(4,3,1)))")
 (run-cps
  "letrec
     occurs-in?(n, s, cont) =
       if null?(s)
       then (cont 0)
       else if number?(car(s))
            then if equal?(n,car(s))
                 then 1
                 else (occurs-in? n cdr(s) cont)
            else (occurs-in? n
                             car(s)
                             proc (val)
                               if equal?(val, 1)
                               then 1
                               else (occurs-in? n cdr(s) cont))
   in (occurs-in? 3
                  list(1,3,list(4,3,1))
                  proc (val) val)"))

;;; remfirst
(check-equal?
 (run
  "letrec
     remfirst(n,s) =
       letrec
         loop(s) =
           if null?(s)
           then emptylist
           else if number?(car(s))
                then if equal?(n,car(s))
                     then cdr(s)
                     else cons(car(s),(loop cdr(s)))
                else if (occurs-in? n car(s))
                     then cons((remfirst n car(s)),
                               cdr(s))
                     else cons(car(s),
                               (remfirst n cdr(s)))
     in (loop s)
   in (remfirst 3
                list(1,3,list(4,3,1)))")
 (run-cps
  "letrec
     remfirst(n,s,cont) =
       letrec
         loop(s,cont) =
           if null?(s)
           then (cont emptylist)
           else if number?(car(s))
                then if equal?(n,car(s))
                     then (cont cdr(s))
                     else (loop cdr(s) proc (val) (cont cons(car(s), val)))
                else (occurs-in? n
                                 car(s)
                                 proc (val)
                                   if equal?(val, 1)
                                   then (remfirst n
                                                  car(s)
                                                  proc (val)
                                                    (cont cons(val, cdr(s))))
                                   else (remfirst n
                                                  cdr(s)
                                                  proc (val)
                                                    (cont cons(car(s), val))))
     in (loop s cont)
   in (remfirst 3
                list(1,3,list(4,3,1))
                proc (val) val)"))


;;; depth
(check-equal?
 (run
  "letrec
     depth(s) =
       if null?(s)
       then 1
       else if number?(car(s))
            then (depth cdr(s))
            else if less?(add1((depth car(s))),
                          (depth cdr(s)))
                 then (depth cdr(s))
                 else add1((depth car(s)))
   in (depth list(list(), 1,3,list(4,5,list())))")
 (run-cps
  "letrec
     depth(s, cont) =
       if null?(s)
       then (cont 1)
       else if number?(car(s))
            then (depth cdr(s) cont)
            else (depth car(s)
                        proc (val1)
                          (depth cdr(s)
                                 proc (val2)
                                   if less?(val1, val2)
                                   then (depth cdr(s) cont)
                                   else (depth car(s)
                                               proc (val3)
                                                 (cont add1(val3)))))
   in (depth list(list(), 1, 3, list(4, 5, list()))
             proc (val) val)"))

;;; depth-with-let
(check-equal?
 (run
  "letrec
    depth(s) =
      if null?(s)
      then 1
      else if number?(car(s))
           then (depth cdr(s))
           else let dfirst = add1((depth car(s)))
                    drest = (depth cdr(s))
                in if less?(dfirst,drest)
                   then drest
                   else dfirst
  in (depth list(list(), 1, 3, list(4, 5, list())))")
 (run-cps
  "letrec
    depth(s, cont) =
      if null?(s)
      then (cont 1)
      else if number?(car(s))
           then (depth cdr(s) cont)
           else (depth car(s)
                       proc (val1)
                         (proc (val2, cont) (cont val2)
                          add1(val1)
                          proc (dfirst)
                            (depth cdr(s)
                                   proc (drest)
                                   if less?(dfirst, drest)
                                   then (cont drest)
                                   else (cont dfirst))))
   in (depth list(list(), 1, 3, list(4, 5, list()))
             proc (val) val)"))

;;; map
(check-equal?
 (run
  "letrec
    map(f, l) = if null?(l)
                then emptylist
                else cons((f car(l)),
                          (map f cdr(l)))
    square(n) = *(n,n)
   in (map square list(1,2,3,4,5))")
 (run-cps
  "letrec
     map(f, l, cont) = if null?(l)
                       then (cont emptylist)
                       else (f car(l)
                               proc (val1)
                                 (map f
                                      cdr(l)
                                      proc (val2)
                                        (cont cons(val1, val2))))
     square(n, cont) = (cont *(n,n))
   in (map square list(1,2,3,4,5) proc (val) val)"))

;;; fnlrgtn
(check-equal?
 (run
  "letrec
     fnlrgtn(l, n) = if null?(l)
                     then emptylist
                     else if number?(car(l))
                          then if greater?(car(l), n)
                               then car(l)
                               else (fnlrgtn cdr(l) n)
                          else if null?((fnlrgtn car(l) n))
                               then (fnlrgtn cdr(l) n)
                               else (fnlrgtn car(l) n)
   in (fnlrgtn list(1, list(3, list(2), 7, list(9)))
               6)")
 (run-cps
  "letrec
     fnlrgtn(l, n, cont) = if null?(l)
                           then (cont emptylist)
                           else if number?(car(l))
                                then if greater?(car(l), n)
                                     then (cont car(l))
                                     else (fnlrgtn cdr(l) n cont)
                                else (fnlrgtn car(l)
                                              n
                                              proc (val)
                                                if null?(val)
                                                then (fnlrgtn cdr(l) n cont)
                                                else (fnlrgtn car(l) n cont))
   in (fnlrgtn list(1, list(3, list(2), 7, list(9)))
               6
               proc (val) val)"))

;;; fnlrgtn-with-let
(check-equal?
 (run
  "letrec
     fnlrgtn(l, n) = if null?(l)
                     then emptylist
                     else if number?(car(l))
                          then if greater?(car(l), n)
                               then car(l)
                               else (fnlrgtn cdr(l) n)
                          else let ffirst = (fnlrgtn car(l) n)
                               in if null?(ffirst)
                                  then (fnlrgtn cdr(l) n)
                                  else ffirst
   in (fnlrgtn list(1, list(3, list(2), 7, list(9)))
               6)")
 (run-cps
  "letrec
     fnlrgtn(l, n, cont) = if null?(l)
                           then (cont emptylist)
                           else if number?(car(l))
                                then if greater?(car(l), n)
                                     then (cont car(l))
                                     else (fnlrgtn cdr(l) n cont)
                                else (fnlrgtn car(l)
                                              n
                                              proc (val)
                                                if null?(val)
                                                then (fnlrgtn cdr(l) n cont)
                                                else (cont val))
   in (fnlrgtn list(1, list(3, list(2), 7, list(9)))
               6
               proc (val) val)"))

;;; every
(check-equal?
 (run
  "letrec
     every(pred, l) =
       if null?(l)
       then 1
       else if (pred car(l))
            then (every pred cdr(l))
            else 0
   in (every proc(n) greater?(n,5)
             list(6,7,8,9))")
 (run-cps
  "letrec
     every(pred, l, cont) =
       if null?(l)
       then (cont 1)
       else (pred car(l)
                  proc (val)
                    if val
                    then (every pred cdr(l) cont)
                    else (cont 0))
   in (every proc(n, cont) (cont greater?(n, 5))
             list(6,7,8,9)
             proc (val) val)"))

#lang eopl

(provide tests-for-run tests-for-check tests-for-parse)

(define the-test-suite
  '(
    (modules-test-multi-param
     "
      module maker1
       interface
        ((m1 : [opaque t
                z : t
                z? : (t -> bool)
                s : (t -> t)]
          m2 : [opaque t
                z : t
                s : (t -> t)])
         => [transparent t1 = from m1 take t
             transparent t2 = from m2 take t
             d : (t1 -> t2)])
       body
        module-proc
         (p1 : [opaque t
                z : t
                z? : (t -> bool)
                s : (t -> t)]
          p2 : [opaque t
                z : t
                s : (t -> t)])
         [type t1 = from p1 take t
          type t2 = from p2 take t
          d = proc (x : t1)
                if (from p1 take z? x)
                then from p2 take z
                else (from p2 take s from p2 take z)]

       module m0
        interface
         [opaque t
          z : t
          z? : (t -> bool)
          s : (t -> t)]
        body
         [type t = int
          z = 0
          z? = proc (x : t) zero?(-(x,z))
          s = proc (u : t) -(u, -1)]

       module m1
        interface
         [opaque t
          z : t
          s : (t -> t)]
        body
         [type t = int
          z = 3
          s = proc (u : t) -(u, -2)]

       module m2
        interface
         [opaque t1
          opaque t2
          d : (t1 -> t2)]
        body
         (maker1 m0 m1)

        33"
     int 33)

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

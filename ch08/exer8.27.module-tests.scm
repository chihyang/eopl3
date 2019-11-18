#lang eopl

(provide tests-for-run tests-for-check tests-for-parse)

(define the-test-suite
  '(
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

#lang eopl

(provide tests-for-run tests-for-check tests-for-parse)

(define the-test-suite
  '(
    (modules-check-extend-app-1
     "
        module m1
            interface ((m : [v : int]) => [w : int])
            body
             module-proc (m : [v : int]) [w = -(from m take v, 1)]
        module m3
         interface [w : int]
         body
          (m1 [v = 33])
        from m3 take w"
     int 32)

    (modules-check-extend-app-2
     "
        module m3
         interface [w : int]
         body
          (module-proc (m : [v : int]) [w = -(from m take v, 1)] [v = 33])
        from m3 take w"
     int 32)
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

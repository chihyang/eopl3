#lang eopl
(define plus
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (plus (predecessor x) y)))))
(define sub
  (lambda (x y)
    (if (is-zero? y)
        x
        (sub (predecessor x) (predecessor y)))))
(define mul
  (lambda (x y)
    (if (is-zero? y)
        (zero)
        (if (is-zero? (predecessor y))
            x
            (plus x (mul x (predecessor y)))))))
(define mul
  (lambda (x y)
    (mul-iter (zero) x y)))
(define mul-iter
  (lambda (acc x y)
    (if (is-zero? y)
        acc
        (mul-iter (plus acc x) x (predecessor y)))))
(define fact
  (lambda (x)
    (if (is-zero? x)
        (successor x)
        (mul x (fact (predecessor x))))))
;;; rep 3
(define N 16)
(define zero (lambda () '()))
(define is-zero? (lambda (n) (null? n)))
(define successor
  (lambda (d)
    (if (null? d)
        (cons 1 d)
        (if (= (+ (car d) 1) N)
            (cons 0
                  (successor (cdr d)))
            (cons (+ (car d) 1)
                  (cdr d))))))
(define predecessor
  (lambda (d)
    (predecessor-iter d (zero))))
(define predecessor-iter
  (lambda (d p)
    (if (is-zero? d)
        #f
        (if (equal? d (successor p))
            p
            (predecessor-iter d (successor p))))))
;;; ---- test ----
(successor
 (successor
  (successor
   (plus
    (plus (plus (plus (successor (zero)) (successor (zero))) (plus (successor (zero)) (successor (zero))))
          (plus (plus (successor (zero)) (successor (zero))) (plus (successor (zero)) (successor (zero)))))
    (plus (plus (plus (successor (zero)) (successor (zero))) (plus (successor (zero)) (successor (zero))))
          (plus (plus (successor (zero)) (successor (zero))) (plus (successor (zero)) (successor (zero)))))))))

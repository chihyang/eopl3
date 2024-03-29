#lang eopl
(require rackunit)

(define f (lambda (v1) (+ v1 1)))
(define f/k (lambda (v1 c) (c (+ v1 1))))
(define g (lambda (v1) (- v1 1)))
(define g/k (lambda (v1 c) (c (- v1 1))))
(define h (lambda (v) (* v 1)))
(define h/k (lambda (v c) (c (* v 1))))
(define j (lambda (v) (* v 2)))
(define j/k (lambda (v c) (c (* v 2))))
(define x 3)
(define y 4)
(define end-cont
  (lambda ()
    (lambda (val) val)))

(check-eq?
 ((lambda (x y) (+ (f (g x)) (h (j y))))
  x y)
 ((lambda (x y cont)
    (j/k y (lambda (v1)
             (g/k x (lambda (v2)
                      (h/k v1 (lambda (v3)
                                (f/k v2 (lambda (v4)
                                          (+ v4 v3))))))))))
  x y (end-cont)))

(check-eq?
 ((lambda (x y) (+ (f (g x)) (h (j y))))
  x y)
 ((lambda (x y cont)
    (j/k y (lambda (v1)
             (g/k x (lambda (v2)
                      (f/k v1 (lambda (v3)
                                (h/k v2 (lambda (v4)
                                          (+ v3 v4))))))))))
  x y (end-cont)))

(check-eq?
 ((lambda (x y) (+ (f (g x)) (h (j y))))
  x y)
 ((lambda (x y cont)
    (j/k y (lambda (v1)
             (h/k v1 (lambda (v2)
                       (g/k x (lambda (v3)
                                (f/k v3 (lambda (v4)
                                          (+ v4 v2))))))))))
  x y (end-cont)))

(check-eq?
 ((lambda (x y) (+ (f (g x)) (h (j y))))
  x y)
 ((lambda (x y cont)
    (g/k x (lambda (v1)
             (f/k v1 (lambda (v2)
                       (j/k y (lambda (v3)
                                (h/k v3 (lambda (v4)
                                          (+ v2 v4))))))))))
  x y (end-cont)))

(check-eq?
 ((lambda (x y) (+ (f (g x)) (h (j y))))
  x y)
 ((lambda (x y cont)
    (g/k x (lambda (v1)
             (j/k y (lambda (v2)
                      (h/k v1 (lambda (v3)
                                (f/k v2 (lambda (v4)
                                          (+ v4 v3))))))))))
  x y (end-cont)))

(check-eq?
 ((lambda (x y) (+ (f (g x)) (h (j y))))
  x y)
 ((lambda (x y cont)
    (g/k x (lambda (v1)
             (j/k y (lambda (v2)
                      (f/k v1 (lambda (v3)
                                (h/k v2 (lambda (v4)
                                          (+ v3 v4))))))))))
  x y (end-cont)))

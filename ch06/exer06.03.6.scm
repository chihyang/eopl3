#lang eopl
(require rackunit)

;;; 6
;; tips
;; (let ((x (let ((y 8)) (p y)))) x)
;; =>
;; ((lambda (x) x)
;;  (let ((y 8)) (p y)))
;; =>
;; ((lambda (x) x)
;;  ((lambda (y) (p y))
;;   8))
;; =>
;; ((lambda (y cont) (p y cont))
;;  8
;;  (lambda (x) x))

;; ((lambda (x) x)
;;  ((lambda (y) (p y))
;;   8))
;; =>
;; ((lambda (x cont) (cont x))
;;  ((lambda (y cont) (p y cont))
;;   8))
;; ((lambda (y cont) (p y cont))
;;  8
;;  (lambda (v1)
;;    ((lambda (x cont) (cont x))
;;     v1
;;     (lambda (v2) v2))))
(define p (lambda (v) (+ v 1)))
(define p/k (lambda (v c) (c (+ v 1))))
(check-eqv?
 (let ((x (let ((y 8)) (p y)))) x)
 ((lambda (y cont) (p/k y cont))
  8
  (lambda (v1)
    ((lambda (x cont) (cont x))
     v1
     (lambda (v2) v2)))))
;; another transform
(check-eqv?
 (let ((x (let ((y 8)) (p y)))) x)
 (let ((y 8)
       (cont (lambda (v1)
               (let ((x v1)
                     (cont (lambda (v2) v2)))
                 (cont x)))))
   (p/k y cont)))

#lang eopl
(define val? number?)
(define-datatype stack stack?
  (empty-stack)
  (non-empty-stack                      ; this is push
   (val val?)
   (smaller-stack stack?)))
(define pop
  (lambda (any-stack)
    (cases stack any-stack
           (empty-stack
            ()
            (eopl:error 'stack "Can not pop from empty stack"))
           (non-empty-stack
            (val smaller-stack)
            smaller-stack))))
(define top
  (lambda (any-stack)
    (cases stack any-stack
           (empty-stack
            ()
            (eopl:error 'stack "Can not top from empty stack"))
           (non-empty-stack
            (val smaller-stack)
            val))))
;;; ---------------------- Test ----------------------
(eqv?
 (equal?
  (pop
   (non-empty-stack 3 (non-empty-stack 2 (non-empty-stack 1 (empty-stack)))))
  (non-empty-stack 2 (non-empty-stack 1 (empty-stack))))
 #t)
(eqv?
 (equal?
  (top
   (non-empty-stack 3 (non-empty-stack 2 (non-empty-stack 1 (empty-stack)))))
  3)
 #t)

#lang eopl
(define-datatype stack stack?
  (empty-stack)
  (non-empty-stack                      ; this is push
   (val val?)
   (smaller-stack stack?)))
(define pop
  (lambda (val any-stack)
    (cases stack any-stack
           (empty-env
            (eopl:error 'stack "Can not pop from empty stack"))
           (non-empty-stack
            (val smaller-stack)
            smaller-stack))))
(define top
  (lambda (any-stack)
    (cases stack a-stack
           (empty-env
            (eopl:error 'stack "Can not top from empty stack"))
           (non-empty-stack
            (val smaller-stack)
            val))))

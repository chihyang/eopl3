#lang eopl
(define empty-stack
  (lambda ()
    (lambda (op)
      (if (eqv? op 'empty-stack?)
          #t
          (report-empty-stack op)))))
(define push
  (lambda (val stack)
    (lambda (op)
      (cond [(eqv? op 'empty-stack?) #f]
            [(eqv? op 'pop) stack]
            [(eqv? op 'top) val]))))
(define pop
  (lambda (s)
    (s 'pop)))
(define top
  (lambda (s)
    (s 'top)))
(define empty-stack?
  (lambda (s)
    (s 'empty-stack?)))
(define stack->list
  (lambda (s)
    (if (empty-stack? s)
        '()
        (cons (top s) (stack->list (pop s))))))
(define report-empty-stack
  (lambda (op)
    (eopl:error
     'stack
     "cannot ~s on a empty stack" op)))
;;; ---- test ----
(eqv? (top (push 2 (empty-stack))) 2)
(eqv? (top (push 4 (pop (push 2 (empty-stack))))) 4)
(equal? (stack->list (push 5 (push 4 (pop (push 2 (empty-stack)))))) '(5 4))
(equal? (stack->list (pop (push 2 (empty-stack)))) '())
(eqv? (empty-stack? (empty-stack)) #t)
(eqv? (empty-stack? (push 2 (empty-stack))) #f)
(eqv? (empty-stack? (pop (push 2 (empty-stack)))) #t)
(eqv? (empty-stack? (push 4 (pop (push 2 (empty-stack))))) #f)
(eqv? (empty-stack? (push 5 (push 4 (pop (push 2 (empty-stack)))))) #f)
;; error
(top (empty-stack))
;; error
(top (pop (push 2 (empty-stack))))
;; error
(pop (empty-stack))

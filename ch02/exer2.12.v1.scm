#lang eopl
(define-datatype stack stack?
  (empty-stack)
  (push
   (val integer->char)
   (s stack?)))
(define pop
  (lambda (s)
    (cases stack s
           (empty-stack
            ()
            (report-empty-stack 'pop))
           (push
            (val s)
            s))))
(define top
  (lambda (s)
    (cases stack s
           (empty-stack
            ()
            (report-empty-stack 'top))
           (push
            (val s)
            val))))
(define empty-stack?
  (lambda (s)
    (cases stack s
           (empty-stack
            ()
            #t)
           (else #f))))
(define stack->list
  (lambda (s)
    (cases stack s
           (empty-stack
            ()
            '())
           (push
            (val s)
            (cons val (stack->list s))))))
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

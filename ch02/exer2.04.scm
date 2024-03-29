#lang eopl
;;; constructor
(define empty-stack
  (lambda () '()))
(define push
  (lambda (val stack)
    (cons val stack)))
;;; observer
(define pop
  (lambda (stack)
    (if (null? stack)
        (report-empty-stack 'pop)
        (cdr stack))))
(define top
  (lambda (stack)
    (if (null? stack)
        (report-empty-stack 'top)
        (car stack))))
(define empty-stack?
  (lambda (stack)
    (null? stack)))
(define report-empty-stack
  (lambda (op)
    (eopl:error
     'stack
     "cannot ~s on a empty stack" op)))
;;; ---- test ----
(eqv? (top (push 2 (empty-stack))) 2)
(eqv? (top (push 4 (pop (push 2 (empty-stack))))) 4)
(equal? (push 5 (push 4 (pop (push 2 (empty-stack))))) '(5 4))
(equal? (pop (push 2 (empty-stack))) '())
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

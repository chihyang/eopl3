#lang eopl
;;; non-procedural representation version
;;; constructor
(define empty-stack
  (lambda () '()))
(define push
  (lambda (val stack)
    (cons val stack)))
(define pop
  (lambda (stack)
    (if (null? stack)
        (empty-stack)
        (cdr stack))))
;;; observer
(define top
  (lambda (stack)
    (if (null? stack)
        (empty-stack)
        (car stack))))
(define empty-stack?
  (lambda (stack)
    (null? stack)))
;;; constructor-observer for top
(define empty-stack
  (lambda ()
    (lambda ()
      '())))
(define push
  (lambda (val stack)
    (lambda ()
      val)))
(define top
  (lambda (stack)
    (stack)))
;;; constructor-observer for pop
(define empty-stack
  (lambda ()
    (lambda ()
      '())))
(define push
  (lambda (val stack)
    (lambda ()
      stack)))
(define pop
  (lambda (stack)
    (lambda ()
      (stack))))
;;; constructor-observer for empty-stack?
(define empty-stack
  (lambda ()
    (lambda ()
      #t)))
(define push
  (lambda (val stack)
    (lambda ()
      #f)))
(define empty-stack?
  (lambda (stack)
    (lambda ()
      (stack))))
;;; all
(define empty-stack
  (lambda ()
    (lambda (action)
      (cond ((eqv? action 'pop)
             (report-pop-from-empty-stack))
            ((eqv? action 'top)
             (report-top-from-empty-stack))
            ((eqv? action 'empty-stack?)
             #t)
            (else
             (report-invalid-operation))))))
(define push
  (lambda (val stack)
    (lambda (action)
      (cond ((eqv? action 'pop)
             stack)
            ((eqv? action 'top)
             val)
            ((eqv? action 'empty-stack?)
             #f)
            (else
             (report-invalid-operation))))))
(define top
  (lambda (stack)
    (stack 'top)))
(define pop
  (lambda (stack)
    (stack 'pop)))
(define empty-stack?
  (lambda (stack)
    (stack 'empty-stack?)))
(define report-pop-from-empty-stack
  (lambda ()
    (eopl:error 'pop "Empty stack cannot be popped")))
(define report-top-from-empty-stack
  (lambda ()
    (eopl:error 'top "Top of empty stack")))
(define report-invalid-operation
  (lambda ()
    (eopl:error "Unknown operation")))
;;; ---- test ----
(top (empty-stack))
(top (push 2 (empty-stack)))
(top (pop (push 2 (empty-stack))))
(top (push 4 (pop (push 2 (empty-stack)))))
(top (push 5 (push 4 (pop (push 2 (empty-stack))))))

(pop (empty-stack))
(pop (push 2 (empty-stack)))
(pop (pop (push 2 (empty-stack))))
(pop (push 4 (pop (push 2 (empty-stack)))))
(pop (push 5 (push 4 (pop (push 2 (empty-stack))))))

(empty-stack? (empty-stack))
(empty-stack? (push 2 (empty-stack)))
(empty-stack? (pop (push 2 (empty-stack))))
(empty-stack? (push 4 (pop (push 2 (empty-stack)))))
(empty-stack? (push 5 (push 4 (pop (push 2 (empty-stack))))))

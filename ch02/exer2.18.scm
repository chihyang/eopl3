#lang eopl
;;; NodeInSequence ::= (Int Listof(Int) Listof(Int))
;;; Int -> NodeInSequence
(define number->sequence
  (lambda (number)
    (list number '() '())))
;;; NodeInSequence -> Int
(define current-element
  (lambda (node)
    (car node)))
;;; NodeInSequence -> NodeInSequence
(define move-to-left
  (lambda (node)
    (if (at-left-end? node)
        (report-left-end (car node))
        (list (caadr node) (cdadr node) (cons (car node) (caddr node))))))
;;; NodeInSequence -> NodeInSequence
(define move-to-right
  (lambda (node)
    (if (at-right-end? node)
        (report-right-end (car node))
        (list (caaddr node)
              (cons (car node) (cadr node))
              (cdr (caddr node))))))
(define report-left-end
  (lambda (n)
    (eopl:error 'move-to-left "~s is at the left end of sequence." n)))
(define report-right-end
  (lambda (n)
    (eopl:error 'move-to-right "~s is at the right end of sequence." n)))
;;; Int x NodeInSequence -> NodeInSequence
(define insert-to-left
  (lambda (n node)
    (list (car node)
          (cons n (cadr node))
          (caddr node))))
;;; Int x NodeInSequence -> NodeInSequence
(define insert-to-right
  (lambda (n node)
    (list (car node)
          (cadr node)
          (cons n (caddr node)))))
;;; NodeInSequence -> Bool
(define at-left-end?
  (lambda (node)
    (null? (cadr node))))
;;; NodeInSequence -> Bool
(define at-right-end?
  (lambda (node)
    (null? (caddr node))))
;;; ---- test ----
(equal? (number->sequence 7) '(7 () ()))
(move-to-left (number->sequence 7))
(move-to-right '(6 (5 4 3 2 1) ()))
(equal? (current-element '(6 (5 4 3 2 1) (7 8 9))) 6)
(equal? (move-to-left '(6 (5 4 3 2 1) (7 8 9))) '(5 (4 3 2 1) (6 7 8 9)))
(equal? (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (13 5 4 3 2 1) (7 8 9)))
(equal? (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))) '(6 (5 4 3 2 1) (13 7 8 9)))
(equal? (at-right-end? '(6 (5 4 3 2 1) (7 8 9))) #f)
(equal? (at-right-end? '(7 () ())) #t)
(equal? (at-left-end? '(6 (5 4 3 2 1) (7 8 9))) #f)
(equal? (at-left-end? '(6 () (7 8 9))) #t)

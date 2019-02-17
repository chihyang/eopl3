#lang eopl
;;; Bintree ::= () | (Int Bintree Bintree)
;;; Int -> Bintree
(define number->bintree
  (lambda (n)
    (list n '() '())))
;;; Bintree -> Int
(define current-element
  (lambda (tree)
    (car tree)))
;;; Bintree -> Bintree
(define move-to-left-son
  (lambda (tree)
    (cadr tree)))
;;; Bintree -> Bintree
(define move-to-right-son
  (lambda (tree)
    (caddr tree)))
;;; Bintree -> Bool
(define at-leaf?
  (lambda (tree)
    (null? tree)))
;;; Int x Bintree -> Bintree
(define insert-to-left
  (lambda (n tree)
    (list (car tree)
          (list n
                (cadr tree)
                '())
          (caddr tree))))
;;; Int x Bintree -> Bintree
(define insert-to-right
  (lambda (n tree)
    (list (car tree)
          (cadr tree)
          (list n
                (caddr tree)
                '()))))
;;; ---- test ----
(number->bintree 13)
(define t1
  (insert-to-right
   14
   (insert-to-left
    12
    (number->bintree 13))))
(move-to-left-son t1)
(move-to-right-son t1)
(at-leaf? (move-to-right-son (move-to-left-son t1)))
(insert-to-left 15 t1)

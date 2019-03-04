#lang eopl
;;; ------------------------------ exer1.31 ------------------------------
;; Bintree :: = Int | (Symbol Bintree Bintree)
;; definition
(define leaf
  (lambda (num)
    num))
(define interior-node
  (lambda (symbol ltree rtree)
    (list symbol ltree rtree)))
;; predicate
(define leaf?
  (lambda (tree)
    (number? tree)))
;; operation
;; left node
(define lson
  (lambda (tree)
    (cadr tree)))
;; right node
(define rson
  (lambda (tree)
    (caddr tree)))
;; left and right node
(define contents-of
  (lambda (tree)
    (if (leaf? tree)
        tree
        (car tree))))

;;; ------------------------------ exer1.32 ------------------------------
;; double-tree : Bintree -> Bintree
;;
;; usage: return a Bintree with every integer leaf doubled in tree
(define double-tree
  (lambda (tree)
    (if (leaf? tree)
        (leaf (* 2 tree))
        (interior-node
         (contents-of tree)
         (double-tree (lson tree))
         (double-tree (rson tree))))))

(equal? (double-tree (interior-node 'foo (leaf 3) (interior-node 'bar (leaf 26) (leaf 27))))
        '(foo 6 (bar 52 54)))
(equal? (double-tree (leaf 2))
        4)

;;; ------------------------------ exer1.33 ------------------------------
;; mark-leaves-with-red-depth : Bintree -> Bintree
;;
;; usage: return a Bintree as tree except that every leaf is replaced with such
;; a integer that it represents the number of the symbol red between it the root
(define mark-leaves-with-red-depth
  (lambda (tree)
    (mark-leaves-with-red-depth-iter tree 0)))

;; mark-leaves-with-red-depth : Bintree x Int -> Bintree
;;
;; usage: return a Bintree as tree except that every leaf is replaced with such
;; an integer that it represents the number of the symbol red between it the
;; root plus number n
(define mark-leaves-with-red-depth-iter
  (lambda (tree n)
    (cond ((leaf? tree)
           (leaf n))
          ((eq? (contents-of tree) 'red)
           (interior-node
             'red
             (mark-leaves-with-red-depth-iter (lson tree) (+ n 1))
             (mark-leaves-with-red-depth-iter (rson tree) (+ n 1))))
          (else
           (interior-node
             (contents-of tree)
             (mark-leaves-with-red-depth-iter (lson tree) n)
             (mark-leaves-with-red-depth-iter (rson tree) n))))))

(equal? (mark-leaves-with-red-depth
         (interior-node
          'red
          (interior-node 'bar
                         (leaf 26)
                         (leaf 12))
          (interior-node 'red
                         (leaf 11)
                         (interior-node 'quux
                                        (leaf 117)
                                        (leaf 14)))))
        '(red (bar 1 1) (red 2 (quux 2 2))))
(equal? (mark-leaves-with-red-depth
         (leaf 2))
        0)

;;; ------------------------------ exer1.34 ------------------------------
;; Binary-search-tree ::= () | (Int Binary-search-tree Binary-search-tree)
(define bst-leaf '())
(define bst-node
  (lambda (n lbst rbst)
    (list n lbst rbst)))
(define bst-leaf? null?)
(define bst-lson cadr)
(define bst-rson caddr)
(define bst-contents
  (lambda (bst)
    (if (bst-leaf? bst)
        (bst-leaf)
        (car bst))))

;; path : n x Binary-search-tree -> s-list
;;
;; usage: return a s-list representing the search path of n in binary search
;; tree bst. If n is the root of bst returns null.
(define path
  (lambda (n bst)
    (cond ((bst-leaf? bst)
           '())
          ((< (bst-contents bst) n)
           (cons 'right
                 (path n (bst-rson bst))))
          ((> (bst-contents bst) n)
           (cons 'left
                 (path n (bst-lson bst))))
          (else
           '()))))

(equal? (path 17 '(14 (7 () (12 () ()))
                      (26 (20 (17 () ())
                              ())
                          (31 () ()))))
        '(right left left))
(equal? (path 14 '(14 (7 () (12 () ()))
                      (26 (20 (17 () ())
                              ())
                          (31 () ()))))
        '())
(equal? (path 14 '())
        '())
(equal? (path 14 '(8 (7 () ())
                     ()))
        '(right))


;;; ------------------------------ exer1.35 ------------------------------
;; number-leaves : Bintree -> Bintree
;;
;; usage: return a binary tree that has the same shape as tree, with every
;; leave replaced with ordered numbers starting from 0
(define number-leaves
  (lambda (tree)
    (number-leaves-iter tree 0)))

;; number-leaves-iter : Bintree x Int -> Bintree
;;
;; usage: return a binary tree that has the same shape as tree, with every
;; leave replaced with ordered numbers starting from n
(define number-leaves-iter
  (lambda (tree n)
    (if (leaf? tree)
        n
        (interior-node
         (contents-of tree)
         (number-leaves-iter (lson tree) n)
         (number-leaves-iter (rson tree)
                             (+ (number-of-leaves (lson tree))
                                n))))))

;; number-of-leaves : Bintree -> Int
;;
;; usage: return the number of leaves in tree
(define number-of-leaves
  (lambda (tree)
    (if (leaf? tree)
        1
        (+ (number-of-leaves (lson tree))
           (number-of-leaves (rson tree))))))

(equal? (number-leaves '(red (bar 26 12) (red 11 (quux 117 (foo 32 29)))))
        '(red (bar 0 1) (red 2 (quux 3 (foo 4 5)))))
(equal? (number-leaves (interior-node 't (leaf 2) (leaf 43)))
        '(t 0 1))
(equal? (number-leaves (leaf 43))
        0)
;;; way to think
;; '(red (bar 26 12) (red 11 (quux 117 14)))
;; '(red (foo (bar 26 12) 0)
;;       (foo (red 11 (quux 117 14)) (foo (bar 26 12) 0)))
;; '(bar (26 0)
;;       (12 (26 0)))
;; '(bar 1
;;       (12 1))
;; '(bar 1 2)
;; '((red 11 (quux 117 14)) 2)
;; '(red (11 2)
;;       ((quux 117 14) (11 2)))
;; '(red 3
;;       ((quux 117 14) 3))
;; '(quux (117 3)
;;        (14 (117 3)))
;; '(quux 4
;;        (14 (117 3)))
;; '(quux 4
;;        (14 4))
;; '(quux 4 5)
;; '(red 3 (quux 4 5))
;; '(red (bar 1 2)
;;       (red 3 (quux 4 5)))

;;; ------------------------------ exer1.36 ------------------------------
(define number-elements
  (lambda (lst)
    (if (null? lst)
        '()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))

(define g
  (lambda (2-lst 2-lists)
    (cons 2-lst
          (map (lambda (ele)
                 (list (+ 1 (car ele))
                       (cadr ele)))
               2-lists))))

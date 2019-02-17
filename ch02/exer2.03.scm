#lang eopl
(define one (lambda () (quote one)))
(define diff-tree
  (lambda (diff-left diff-right)
    (list 'diff diff-left diff-right)))
(define diff-tree-left-node
  (lambda (t)
    (cadr t)))
(define right-node
  (lambda (t)
    (caddr t)))
(define one?
  (lambda (t)
    (equal? t (one))))
(define diff-tree-val
  (lambda (t)
    (if (one? t)
        1
        (- (diff-tree-val (diff-tree-left-node t))
           (diff-tree-val (right-node t))))))
(define zero (lambda () (diff-tree (one) (one))))
(define is-zero?
  (lambda (n)
    (eq? (diff-tree-val n) 0)))
(define successor
  (lambda (n)
    (if (one? n)
        (diff-tree (one) (zero))
        (diff-tree (diff-tree-left-node n)
                   (diff-tree (right-node n) (one))))))
(define predecessor
  (lambda (n)
    (if (one? n)
        (zero)
        (diff-tree (diff-tree (diff-tree-left-node n) (one))
                   (right-node n)))))
(define diff-tree-plus
  (lambda (x y)
    (diff-tree x (diff-tree (zero) y))))

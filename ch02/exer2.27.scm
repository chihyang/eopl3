#lang eopl
(define-datatype red-blue-tree red-blue-tree?
  (subtree
   (tree red-blue-subtree?)))
(define-datatype red-blue-subtree red-blue-subtree?
  (red-node
   (first red-blue-subtree?)
   (second red-blue-subtree?))
  (blue-node
   (blue-subtree red-blue-subtree-lst?))
  (leaf-node
   (num integer?)))
(define-datatype red-blue-subtree-lst red-blue-subtree-lst?
  (an-red-blue-subtree-lst
   (trees (list-of red-blue-subtree?))))
(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))
(define mark-leaves-with-red-depth
  (lambda (tree n)
    (cases red-blue-subtree tree
           (red-node
            (first second)
            (red-node (mark-leaves-with-red-depth first (+ n 1))
                      (mark-leaves-with-red-depth second (+ n 1))))
           (blue-node
            (blue-subtree)
            (cases red-blue-subtree-lst blue-subtree
                   (an-red-blue-subtree-lst
                    (trees)
                    (blue-node
                     (an-red-blue-subtree-lst
                      (map (lambda (r-b-tree)
                             (mark-leaves-with-red-depth r-b-tree n))
                           trees))))))
           (leaf-node
            (num)
            (leaf-node n)))))
;;; ----- test -----
(define tree-1
  (blue-node (an-red-blue-subtree-lst
              (list (red-node (leaf-node 24)
                              (leaf-node 72))))))
(define tree-2
  (red-node (leaf-node 12)
            tree-1))
(define tree-3
  (blue-node (an-red-blue-subtree-lst '())))
(define tree-4
  (blue-node (an-red-blue-subtree-lst
              (list
               (red-node (leaf-node 24)
                         (leaf-node 72))
               (red-node (leaf-node 24)
                         (leaf-node 72))))))
(define tree-5
  (red-node tree-1 tree-4))
(mark-leaves-with-red-depth tree-1 0)
(mark-leaves-with-red-depth tree-2 0)
(mark-leaves-with-red-depth tree-3 0)
(mark-leaves-with-red-depth tree-4 0)
(mark-leaves-with-red-depth tree-5 0)

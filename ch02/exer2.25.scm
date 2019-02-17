#lang eopl
(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))
(define bintree-sum
  (lambda (tree)
    (cases bintree tree
           (leaf-node
            (num)
            num)
           (interior-node
            (key left right)
            (+ (bintree-sum left)
               (bintree-sum right))))))
(define max-interior
  (lambda (tree)
    (cases bintree tree
           (leaf-node
            (num)
            (eopl:error 'bintree
                        "Cannot get max interior from a leaf ~s!" tree))
           (interior-node
            (key left right)
            (let* ((left-sum (bintree-sum left))
                   (right-sum (bintree-sum right))
                   (total-sum (+ left-sum right-sum)))
              (cond ((and (interior-node? left)
                          (interior-node? right))
                     (if (> left-sum right-sum)
                         (if (> left-sum total-sum)
                             (max-interior left)
                             key)
                         (if (> right-sum total-sum)
                             (max-interior right)
                             key)))
                    ((interior-node? left)
                     (if (> left-sum total-sum)
                         (max-interior left)
                         key))
                    ((interior-node? right)
                     (if (> right-sum total-sum)
                         (max-interior right)
                         key))
                    (else key)))))))
(define interior-node?
  (lambda (tree)
    (cases bintree tree
           (interior-node
            (key left right)
            #t)
           (else #f))))
;;; ----- test -----
(define tree-1 (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2 (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3 (interior-node 'baz tree-2 (leaf-node 1)))
(eqv? (max-interior tree-1) 'foo)
(eqv? (max-interior tree-2) 'foo)
(eqv? (max-interior tree-3) 'baz)

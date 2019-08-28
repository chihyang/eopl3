(module tests mzscheme

  ;; tests for cps converter, including explicit references.

  (provide try-test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

  (define try-test-list
    '((
       try-test-1
       "try
     let x = 3 in
       -(3, -(2, raise x))
   catch (y)
     -(y, 3)"
       0)

      (
       try-test-2
       "raise 1"
       error))))

(module tests mzscheme

  ;; tests for cps converter, including explicit references.

  (provide letcc-test-list)
  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

  (define letcc-test-list
    '((
       letcc-to-mimic-try
       "letcc final-cnt in
   -(letcc cnt in
       throw let x = 3 in
             -(3, -(2, throw x to cnt))
       to final-cnt,
     3)"
       0)

      (letcc-without-throw
       "letcc final-cnt in
   -(letcc cnt in
       throw let x = 3 in
             -(3, -(2, x))
       to final-cnt,
     3)"
       4))))

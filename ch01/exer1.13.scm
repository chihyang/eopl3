#lang eopl
(define subst
  (lambda (new old slist)
    (map (lambda (s-exp)
           (subst-in-s-exp new old s-exp))
         slist)))
(define subst-in-s-exp
  (lambda (new old slist)
    (if (symbol? slist)
        (if (eq? old slist)
            new
            old)
        (subst new old slist))))

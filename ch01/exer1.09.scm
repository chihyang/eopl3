#lang eopl
(define remove
  (lambda (s los)
    (if (null? los)
        '()
        (if (eq? s (car los))
            (remove s (cdr los))
            (cons (car los)
                  (remove s (cdr los)))))))
(remove 'a '())
(remove 'a '(a b c))
(remove 'a '(c b d c x y a b))

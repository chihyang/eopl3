#lang eopl

;; subst-in-s-exp : Sym × Sym × S-exp x k → S-exp
(define subst-in-s-exp
  (lambda (new old sexp k)
    (if (symbol? sexp)
        (if (equal? sexp old)
            (k new)
            (k sexp))
        (subst new old sexp k))))

;; subst : Sym × Sym × S-list x k → S-list
(define subst
  (lambda (new old slist k)
    (if (null? slist)
        (k '())
        (subst-in-s-exp new old (car slist)
                        (lambda (v1)
                          (subst new old (cdr slist)
                                 (lambda (v2)
                                   (k (cons v1 v2)))))))))

(define end-cont
  (lambda ()
    (lambda (val)
      (eopl:printf "End of computation.~%")
      (eopl:printf "This sentence should appear only once.~%")
      val)))

(require rackunit)
(check-equal?
 (subst 'y 'x '(lambda (x) x) (end-cont))
 '(lambda (y) y))

(check-equal?
 (subst 'x 'y '(lambda (x) x) (end-cont))
 '(lambda (x) x))

(check-equal?
 (subst 'x 'y '(x y) (end-cont))
 '(x x))

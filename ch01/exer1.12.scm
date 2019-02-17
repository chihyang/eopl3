#lang eopl
;;; inlining version of decent recursive subst
(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons
         (if (symbol? (car slist))
             (if (eq? old (car slist))
                 new
                 old)
             (subst new old (car slist)))
         (subst new old (cdr slist))))))
;;; simplified version 1
(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (if (symbol? (car slist))
            (if (eq? old (car slist))
                (cons new
                      (subst new old (cdr slist)))
                (cons old
                      (subst new old (cdr slist))))
            (cons (subst new old (car slist))
                  (subst new old (cdr slist)))))))
;;; simplified version 2
(define subst
  (lambda (new old slist)
    (cond ((null? slist) '())
          ((symbol? (car slist))
           (if (eq? old (car slist))
               (cons new
                     (subst new old (cdr slist)))
               (cons old
                     (subst new old (cdr slist)))))
          (else
           (cons (subst new old (car slist))
                 (subst new old (cdr slist)))))))
;;; simplified version 3
(define subst
  (lambda (new old slist)
    (cond ((null? slist) '())
          ((eq? old (car slist))
           (cons new
                 (subst new old (cdr slist))))
          ((symbol? (car slist))
           (cons old
                 (subst new old (cdr slist))))
          (else
           (cons (subst new old (car slist))
                 (subst new old (cdr slist)))))))

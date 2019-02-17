#lang eopl
(define in-S?
  (lambda (n)
    (if (zero? n)
        #t
        (if (>= (- n 3) 0)
            (in-S? (- n 3))
            #f))))
(define list-length
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (list-length (cdr l))))))
(define nth-element
  (lambda (lst n)
    (if (null? lst)
        (report-list-too-short n)
        (if (eq? n 0)
            (car lst)
            (nth-element (cdr lst) (- n 1))))))
(define report-list-too-short
  (lambda (n)
    (eopl:error 'nth-element
           "List too short by ~s elements.~%" (+ n 1))))
(nth-element '(a b c d e) 3)
(nth-element   '(b c d e) 2)
(nth-element     '(c d e) 1)
(nth-element       '(d e) 0)
(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eq? s (car los))
            (cdr los)
            (cons (car los)
                  (remove-first s (cdr los)))))))
(define occur-free?
  (lambda (var exp)
    (cond ((symbol? exp) (eq? var exp))
          ((eq? (car exp) 'lambda)
           (and (not (eq? var (caadr exp)))
                (occur-free? var (caddr exp))))
          (else
           (or (occur-free? var (car exp))
               (occur-free? var (cdr exp)))))))
(define subst
  (lambda (new old lst)
    (cond ((null? lst) '())
          ((eq? old (car lst))
           (cons new
                 (subst new old (cdr lst))))
          ((symbol? (car lst))
           (cons old
                 (subst new old (cdr lst))))
          (else
           (cons (subst new old (car lst))
                 (subst new old (cdr lst)))))))
;;; another version
(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        (cons (subst-in-s-exp new old (car slist))
              (subst new old (cdr slist))))))
(define subst-in-s-exp
  (lambda (new old slist)
    (if (symbol? slist)
        (if (eq? old slist)
            new
            old)
        (subst new old slist))))
(define number-elements-from
  (lambda (lst n)
    (if (null? lst)
        '()
        (cons (list n (car lst))
              (number-elements-from (cdr lst) (+ n 1))))))
(define number-elements
  (lambda (lst)
    (number-elements-from lst 0)))

(define partial-vector-sum
  (lambda (v n)
    (if (zero? n)
        (vector-ref v 0)
        (+ (vector-ref v n)
           (partial-vector-sum v (- n 1))))))
(define vector-sum
  (lambda (v)
    (let ((n (vector-length v)))
      (if (zero? n)
          0
          (partial-vector-sum v (- n 1))))))

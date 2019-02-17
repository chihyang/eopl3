#lang eopl
(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))
(define parse-int
  (lambda (prefix-lst)
    (cons (const-exp (car prefix-lst))
          (cdr prefix-lst))))
(define parse-diff
  (lambda (prefix-lst)
    (let* ((lhs (parse-expression (cdr prefix-lst)))
           (rhs (parse-expression (cdr lhs))))
      (cons (diff-exp (car lhs) (car rhs))
            (cdr rhs)))))
(define parse-expression
  (lambda (prefix-lst)
    (cond ((number? (car prefix-lst))
           (parse-int prefix-lst))
          ((eqv? (car prefix-lst) '-)
           (if (< (length (cdr prefix-lst)) 2)
               (report-invalid-exp prefix-lst)
               (parse-diff prefix-lst)))
          (else
           (report-invalid-exp prefix-lst)))))
(define report-invalid-exp
  (lambda (prefix-lst)
    (eopl:error 'parse-prefix-lst
                "Not a valid prefix list: ~s" prefix-lst)))
(define parse-prefix-lst
  (lambda (prefix-lst)
    (if (null? prefix-lst)
        (report-invalid-exp prefix-lst)
        (let ((ret (parse-expression prefix-lst)))
          (if (null? (cdr ret))
              (car ret)
              (report-invalid-exp (cdr prefix-lst)))))))
;;; ----- test -----
(parse-prefix-lst '(3))
(parse-prefix-lst '(- 3 2))
(parse-prefix-lst '(- - 3 2 - 4 5))
(parse-prefix-lst '(- - 3 2 - 4 - 12 7))
(parse-prefix-lst '(- - 3 2 - 4 - 12 7 2))
(parse-prefix-lst '(- - 3 2 - 4 - 12))
(parse-prefix-lst '(- x 2 - 4 - 12))
(parse-prefix-lst '(- - (3) 2 - 4 - 12 7))
(parse-prefix-lst '(- x 2 - 4 - 12))
(parse-prefix-lst '())

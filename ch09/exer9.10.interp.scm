#lang eopl
(require "exer9.10.lang.scm")
(require "chap09.s03.store.scm")
(require "exer9.10.env.scm")
(require "exer9.10.class.scm")
(provide value-of-program value-of run checked-run)

;;; ---------------------- Object ----------------------
;;; new-object : ClassName (= Sym) -> Obj
(define new-object
  (lambda (class-name)
    (an-object
     class-name
     (map (lambda (field-name)
            ;; not surplus anymore
            (a-field field-name (newref 'uninitialized-field)))
          (class->field-names (lookup-class class-name))))))

;;; ---------------------- Evaluate expression ----------------------
(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
           (procedure
            (vars body saved-env)
            (value-of body (extend-env* vars (map newref vals) saved-env))))))

;;; apply-method : Method x Obj x Listof(ExpVal) -> ExpVal
(define apply-method
  (lambda (m self args)
    (cases method m
           (a-method
            (vars body super-name field-names)
            (value-of
             body
             (extend-env* vars (map newref args)
                          (extend-env-with-self-and-super
                           (newref (obj-val self)) super-name
                           ;; Why doesn't this cause problem even if
                           ;;
                           ;; (< (length field-names) (length (object->fields self))
                           ;;
                           ;; Because fields corresponding to field-names are
                           ;; always a subset of (object->fields self), IN
                           ;; ORDER. This is why the fields in a class is
                           ;; arranged in the order specified on page 341
                           (extend-env* field-names
                                        (map field->ref (object->fields self))
                                        (empty-env)))))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp
            (num)
            (num-val num))
           (var-exp
            (var)
            (deref (apply-env env var)))
           (emptylist-exp
            ()
            (null-val))
           (cons-exp
            (exp1 exp2)
            (let ((val1 (value-of exp1 env))
                  (val2 (value-of exp2 env)))
              (pair-val val1 val2)))
           (car-exp
            (exp1)
            (let ((val1 (value-of exp1 env)))
              (cases exp-val val1
                     (pair-val
                      (first rest)
                      first)
                     (else
                      (report-invalid-exp-value 'list)))))
           (cdr-exp
            (exp1)
            (let ((val1 (value-of exp1 env)))
              (cases exp-val val1
                     (pair-val
                      (first rest)
                      rest)
                     (else
                      (report-invalid-exp-value 'list)))))
           (null?-exp
            (exp1)
            (let ((val1 (value-of exp1 env)))
              (cases exp-val val1
                     (null-val
                      ()
                      (bool-val #t))
                     (else
                      (bool-val #f)))))
           (list-exp
            (exp1)
            (if (null? exp1)
                (null-val)
                (pair-val (value-of (car exp1) env)
                          (value-of (list-exp (cdr exp1)) env))))
           (diff-exp
            (exp1 exp2)
            (num-val (- (expval->num (value-of exp1 env))
                        (expval->num (value-of exp2 env)))))
           (sum-exp
            (exp1 exp2)
            (num-val (+ (expval->num (value-of exp1 env))
                        (expval->num (value-of exp2 env)))))
           (zero?-exp
            (exp1)
            (bool-val (eqv? (expval->num (value-of exp1 env)) 0)))
           (if-exp
            (exp1 exp2 exp3)
            (if (expval->bool (value-of exp1 env))
                (value-of exp2 env)
                (value-of exp3 env)))
           (let-exp
            (vars exps body)
            (let ((vals (map (lambda (e) (value-of e env)) exps)))
              (value-of body (extend-env* vars (map newref vals) env))))
           (letrec-exp
            (p-names p-vars p-bodies letrec-body)
            (value-of letrec-body (extend-env-rec p-names p-vars p-bodies env)))
           (proc-exp
            (vars body)
            (proc-val (procedure vars body env)))
           (call-exp
            (rator rands)
            (let ((proc (expval->proc (value-of rator env)))
                  (args (map (lambda (e) (value-of e env)) rands)))
              (apply-procedure proc args)))
           (begin-exp
            (exp1 exps)
            (if (null? exps)
                (value-of exp1 env)
                (begin
                  (value-of exp1 env)
                  (value-of (begin-exp (car exps) (cdr exps)) env))))
           (assign-exp
            (var exp1)
            (let ((ref (apply-env env var))
                  (val (value-of exp1 env)))
              (setref! ref val)
              (num-val 27)))
           (new-object-exp
            (class-name rands)
            (let ((args (value-of-exps rands env))
                  (obj (new-object class-name)))
              ;; (eopl:printf "apply method ~s ~s~%" class-name 'initialize)
              (apply-method
               (find-method class-name 'initialize)
               obj
               args)
              (obj-val obj)))
           (method-call-exp
            (obj-exp method-name rands)
            (let ((args (value-of-exps rands env))
                  (obj (expval->obj (value-of obj-exp env))))
              ;; (eopl:printf "apply method ~s ~s~%" (object->class-name obj) method-name)
              (apply-method
               (find-method (object->class-name obj) method-name)
               obj
               args)))
           (super-call-exp
            (method-name rands)
            (let ((args (value-of-exps rands env))
                  (obj (expval->obj (deref (apply-env env '%self)))))
              ;; (eopl:printf "apply method ~s ~s~%" (object->class-name obj) method-name)
              (apply-method
               (find-method (apply-env env '%super) method-name)
               obj
               args)))
           (self-exp
            ()
            (deref (apply-env env '%self)))
           (fieldref-exp
            (obj-exp field-name)
            (let ((obj (expval->obj (value-of obj-exp env))))
              (field-ref obj (object->class-name obj) field-name)))
           (fieldset-exp
            (obj-exp field-name exp1)
            (let ((obj (expval->obj (value-of obj-exp env)))
                  (val (value-of exp1 env)))
              (field-set! obj (object->class-name obj) field-name exp1 env)))
           (superfieldref-exp
            (field-name)
            (let ((obj (expval->obj (deref (apply-env env '%self)))))
              (field-ref obj (apply-env env '%super) field-name)))
           (superfieldset-exp
            (field-name exp1)
            (let ((obj (expval->obj (deref (apply-env env '%self)))))
              (field-set! obj (apply-env env '%super) field-name exp1 env)))
           (named-send-exp
            (class-name obj-exp method-name rands)
            (let ((args (value-of-exps rands env))
                  (obj (expval->obj (value-of obj-exp env))))
              (if (is-subclass (object->class-name obj) class-name)
                  (apply-method
                   (find-method class-name method-name)
                   obj
                   args)
                  (report-obj-not-instance-of-class obj class-name))))
           (named-fieldref-exp
            (class-name obj-exp field-name)
            (let ((obj (expval->obj (value-of obj-exp env))))
              (if (is-subclass (object->class-name obj) class-name)
                  (field-ref obj class-name field-name)
                  (report-obj-not-instance-of-class obj class-name))))
           (named-fieldset-exp
            (class-name obj-exp field-name exp1)
            (let ((obj (expval->obj (value-of obj-exp env))))
              (if (is-subclass (object->class-name obj) class-name)
                  (field-set! obj class-name field-name exp1 env)
                  (report-obj-not-instance-of-class obj class-name)))))))

;;; is-subclass : Sym x Sym -> Bool
(define is-subclass
  (lambda (sub-class super-class)
    (if (eqv? sub-class super-class)
        #t
        (let ((super-name (class->super-name (lookup-class sub-class))))
          (cond [(eqv? super-name #f) #f]
                [(eqv? super-name sub-class) #t]
                [else (is-subclass super-name super-class)])))))

;;; field-ref : Object x Identifier x Identifier
(define field-ref
  (lambda (obj class-name field-name)
    (let ((class (lookup-class class-name)))
      (let ((pos (index-of field-name (reverse (class->field-names class)))))
        (if pos
            (let ((ref (field->ref
                        (list-ref (object->fields obj)
                                  (- (length (class->field-names class))
                                     pos
                                     1)))))
              (deref ref))
            (report-filed-not-found obj field-name))))))

;;; field-set : Object x Identifier x Identifier x Expression x Env
(define field-set!
  (lambda (obj class-name field-name exp1 env)
    (let ((class (lookup-class class-name)))
      (let ((pos (index-of field-name (reverse (class->field-names class)))))
        (if pos
            (let ((ref (field->ref
                        (list-ref (object->fields obj)
                                  (- (length (class->field-names class))
                                     pos
                                     1)))))
              (setref! ref (value-of exp1 env)))
            (report-filed-not-found obj field-name))))))

(define index-of
  (lambda (sym lst)
    (let loop ((lst lst)
               (sym sym)
               (pos 0))
      (cond [(null? lst) #f]
            [(eqv? (car lst) sym) pos]
            [else (loop (cdr lst) sym (+ pos 1))]))))

(define value-of-exps
  (lambda (exps env)
    (map (lambda (exp) (value-of exp env)) exps)))

(define report-filed-not-found
  (lambda (obj name)
    (eopl:error
     'value-of
     "Object ~a has no field ~s" obj name)))

(define report-obj-not-instance-of-class
  (lambda (obj class-name)
    (eopl:error
     'value-of
     "~a is not an instance of class ~s" obj class-name)))

;; value-of-program : Program -> SchemeVal
(define value-of-program
  (lambda (prog)
    (initialize-store!)
    (cases program prog
           (a-program
            (class-decls exp)
            (initialize-class-env! class-decls)
            (let ((val (value-of exp (init-env))))
              (expval->schemeval val))))))

(define run
  (lambda (prgm)
    (value-of-program prgm)))

;;; checked-run : String -> Int | Bool | Proc | String (for exception)
(require (only-in racket/base with-handlers exn:fail?))
(define checked-run
  (lambda (prgm)
    (with-handlers
        [(exn:fail? (lambda (en) 'error))]
      (run prgm))))

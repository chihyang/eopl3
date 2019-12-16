#lang eopl
(require "exer9.12.lang.scm")
(require "chap09.s03.store.scm")
(require "exer9.12.env.scm")
(require "exer9.12.class.scm")
(provide value-of-program value-of run checked-run)

;;; ---------------------- Object ----------------------
;;; new-object : ClassName (= Sym) -> Obj
(define new-object
  (lambda (class-name)
    (an-object
     class-name
     (map (lambda (field-decl)
            (a-field (class-field-decl->name field-decl)
                     (newref 'uninitialized-field)
                     (class-field-decl->prop field-decl)
                     (class-field-decl->host-name field-decl)))
          (class->field-decls (lookup-class class-name))))))

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
            (vars body host-name super-name field-names prop)
            (value-of
             body
             (extend-env* vars (map newref args)
                          (extend-env-with-self-and-super
                           (newref (obj-val self)) super-name
                           (extend-env-with-field
                            field-names
                            (object->fields self)
                            (empty-env)))))))))

;;; actual-reference : Ref | Field x Env -> Ref | Unspecified
(define actual-reference
  (lambda (val env)
    (cond [(reference? val) val]
          [else
           (if (field-accessible? val (current-class-name env))
               (field->ref val)
               (report-field-not-accessible (field->host-name val)
                                            (field->name val)))])))

(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp
            (num)
            (num-val num))
           (var-exp
            (var)
            (let ((val (apply-env env var)))
              (let ((ref (actual-reference val env)))
                (deref ref))))
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
            (let ((maybe-ref (apply-env env var))
                  (val (value-of exp1 env)))
              (let ((ref (actual-reference maybe-ref env)))
                (setref! ref val)
                (num-val 27))))
           (new-object-exp
            (class-name rands)
            (let ((args (value-of-exps rands env))
                  (obj (new-object class-name))
                  (method (find-method class-name 'initialize))
                  (current-class (current-class-name env)))
              (if (method-accessible? method current-class)
                  (begin
                    (apply-method method obj args)
                    (obj-val obj))
                  (report-method-not-accessible (method->host-name method) 'initialize))))
           (method-call-exp
            (obj-exp method-name rands)
            (let ((args (value-of-exps rands env))
                  (obj (expval->obj (value-of obj-exp env))))
              (let ((method (find-method (object->class-name obj) method-name))
                    (current-class (current-class-name env)))
                (if (method-accessible? method current-class)
                    (apply-method method obj args)
                    (report-method-not-accessible (method->host-name method) method-name)))))
           (super-call-exp
            (method-name rands)
            (let ((args (value-of-exps rands env))
                  (obj (expval->obj (deref (apply-env env '%self)))))
              (let ((method (find-method (apply-env env '%super) method-name))
                    (current-class (current-class-name env)))
                (if (method-accessible? method current-class)
                    (apply-method method obj args)
                    (report-method-not-accessible (method->host-name method) method-name)))))
           (self-exp
            ()
            (deref (apply-env env '%self)))
           (fieldref-exp
            (obj-exp field-name)
            (let ((obj (expval->obj (value-of obj-exp env))))
              (let ((f (object-search-field obj field-name)))
                (if (field-accessible? f (current-class-name env))
                    (let ((ref (field->ref f)))
                      (if ref
                          (deref ref)
                          (report-field-not-found obj field-name)))
                    (report-field-not-accessible (field->host-name f) (field->name f))))))
           (fieldset-exp
            (obj-exp field-name exp1)
            (let ((obj (expval->obj (value-of obj-exp env)))
                  (val (value-of exp1 (current-class-name env))))
              (let ((f (object-search-field obj field-name)))
                (if (field-accessible? f env)
                    (let ((ref (field->ref f)))
                      (if ref
                          (setref! ref val)
                          (report-field-not-found obj field-name)))
                    (report-field-not-accessible (field->host-name f) (field->name f)))))))))

;;; method-accessible? : Method x ClassName | Bool -> Bool
(define method-accessible?
  (lambda (method class-name)
    (cases property (method->prop method)
           (public-prop () #t)
           (protected-prop
            ()
            (if class-name
                (is-subclass class-name (method->host-name method))
                #f))
           (private-prop
            ()
            (eqv? class-name (method->host-name method))))))

;;; field-accessible? : Field x ClassName | Bool -> Bool
(define field-accessible?
  (lambda (f class-name)
    (cases property (field->prop f)
           (public-prop () #t)
           (protected-prop
            ()
            (if class-name
                (is-subclass class-name (field->host-name f))
                #f))
           (private-prop
            ()
            (eqv? class-name (field->host-name f))))))

;;; is-subclass : Sym x Sym -> Bool
(define is-subclass
  (lambda (sub-class super-class)
    (if (eqv? sub-class super-class)
        #t
        (let ((super-name (class->super-name (lookup-class sub-class))))
          (cond [(eqv? super-name #f) #f]
                [(eqv? super-name sub-class) #t]
                [else (is-subclass super-name super-class)])))))

(define value-of-exps
  (lambda (exps env)
    (map (lambda (exp) (value-of exp env)) exps)))

(define report-field-not-found
  (lambda (obj name)
    (eopl:error
     'value-of
     "Object ~a has no field ~s" obj name)))

(define report-obj-not-instance-of-class
  (lambda (obj class-name)
    (eopl:error
     'value-of
     "~a is not an instance of class ~s" obj class-name)))

(define report-method-not-accessible
  (lambda (class-name method-name)
    (eopl:error
     'value-of
     "Method ~a::~a is not accessible in current environment" class-name method-name)))

(define report-field-not-accessible
  (lambda (class-name field-name)
    (eopl:error
     'value-of
     "Field ~a::~a is not accessible in current environment" class-name field-name)))

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

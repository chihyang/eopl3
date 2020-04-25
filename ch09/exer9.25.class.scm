#lang eopl
(require "exer9.25.lang.scm")
(require "chap09.s03.store.scm")
(provide (all-defined-out))
;;; ---------------------- Method ----------------------
(define-datatype method method?
  (a-method
   (vars (list-of identifier?))
   (body expression?)
   (super-name identifier?)
   (field-names (list-of identifier?))))

;;; ---------------------- Class Environment ----------------------
;;; ClassEnv = Listof(Listof(ClassName, Class))
;;; the-class-env : ClassEnv
(define the-class-env '())

;;; add-to-class-env! : ClassName x Class -> Unspecified
(define add-to-class-env!
  (lambda (class-name class)
    (set! the-class-env
      (cons (list class-name class) the-class-env))))

;;; lookup-class : ClassName -> Class
(define lookup-class
  (lambda (name)
    (let ((maybe-pair (assq name the-class-env)))
      (if maybe-pair
          (cadr maybe-pair)
          (report-unknown-class name)))))

(define report-unknown-class
  (lambda (name)
    (eopl:error 'lookup-class "Unknown class ~s" name)))

;;; ---------------------- Class ----------------------
(define-datatype class class?
  (a-class
   (super-name (maybe identifier?))
   (field-names (list-of identifier?))
   (method-env method-environment?)))

;;; class->field-names : Class -> Sym
(define class->super-name
  (lambda (c)
    (cases class c
           (a-class
            (s-name f-names m-env) s-name))))

;;; class->field-names : Class -> Listof(Sym)
(define class->field-names
  (lambda (c)
    (cases class c
           (a-class
            (s-name f-names m-env) f-names))))

;;; class->field-names : Class -> MethodEnv
(define class->method-env
  (lambda (c)
    (cases class c
           (a-class
            (s-name f-names m-env) m-env))))

;;; maybe : Pred -> Pred
(define maybe
  (lambda (foo)
    (lambda (x)
      (or (foo x) (eqv? x #f)))))

;;; initialize-class-env! : Listof(ClassDecl) -> Unspecified
(define initialize-class-env!
  (lambda (c-decls)
    (set! identifier-counter 0)
    (set! the-class-env
      (list
       (list 'object (a-class #f '() '()))))
    (for-each initialize-class-decl! c-decls)))

;;; initialize-class-decl! : ClassDecl -> Unspecified
(define initialize-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
           (a-class-decl
            (c-name s-name f-names m-decls)
            (let ((f-names
                   (append-field-names
                    (class->field-names (lookup-class s-name))
                    f-names)))
              (add-to-class-env!
               c-name
               (a-class s-name f-names
                        (merge-method-envs
                         (class->method-env (lookup-class s-name))
                         (method-decls->method-env
                          m-decls s-name f-names)))))))))

;;; append-field-names : Listof(FieldName) x Listof(FieldName) -> Listof(FieldName)
(define append-field-names
  (lambda (super-fields new-fields)
    (cond [(null? super-fields) new-fields]
          [else
           (cons
            (if (memq (car super-fields) new-fields)
                (fresh-identifier (car super-fields))
                (car super-fields))
            (append-field-names
             (cdr super-fields) new-fields))])))

;;; fresh-identifier from chapter 6
(define identifier-counter 'uninitiated)
(define fresh-identifier
  (lambda (v)
    (set! identifier-counter (+ identifier-counter 1))
    (string->symbol (string-append (symbol->string v) (number->string identifier-counter)))))

;;; ---------------------- Method Environment ----------------------
;;; MethodEnv = Listof(List(MethodName, Method))
;;; method-environment? : SchemeVal -> Bool
(define method-environment?
  (lambda (val)
    (list-of method-pair?)))

;;; method-pair? : SchemeVal -> Bool
(define method-pair?
  (lambda (val)
    (if (pair? val)
        (and (identifier? (car val))
             (method? (cadr val))
             (null? (cddr val)))
        #f)))

;;; find-method : Sym x Sym -> Method
(define find-method
  (lambda (c-name name)
    (let ((m-env (class->method-env (lookup-class c-name))))
      (let ((maybe-pair (assq name m-env)))
        (if maybe-pair
            (cadr maybe-pair)
            (report-method-not-found c-name name))))))

(define report-method-not-found
  (lambda (c-name m-name)
    (eopl:error 'find-method "Unknown method ~s in class ~s" m-name c-name)))

;;; method-decls->method-env : Listof(MethodDecl) x ClassName x
;;; Listof(FieldName) -> MethodEnv
(define method-decls->method-env
  (lambda (m-decls super-name field-names)
    (map
     (lambda (m-decl)
       (cases method-decl m-decl
              (a-method-decl
               (m-name vars body)
               (list m-name
                     (a-method vars body super-name field-names)))))
     m-decls)))

;;; merge-method-envs : MethodEnv x MethodEnv -> MethodEnv
(define merge-method-envs
  (lambda (super-m-env self-m-env)
    (append self-m-env super-m-env)))

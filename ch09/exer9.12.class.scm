#lang eopl
(require "exer9.12.lang.scm")
(require "chap09.s03.store.scm")
(provide (all-defined-out))

;;; ---------------------- Class Field ----------------------
;;; class-field-decl->host-name : FieldDecl -> Sym
(define class-field-decl->host-name
  (lambda (field)
    (cases class-field-decl field
           (a-class-field-decl
            (c name prop)
            c))))

;;; class-field-decl->prop : FieldDecl -> Property
(define class-field-decl->prop
  (lambda (field)
    (cases class-field-decl field
           (a-class-field-decl
            (c name prop)
            prop))))

;;; class-field-decl->name : FieldDecl -> Sym
(define class-field-decl->name
  (lambda (field)
    (cases class-field-decl field
           (a-class-field-decl
            (c name prop)
            name))))

;;; ---------------------- Method ----------------------
(define-datatype method method?
  (a-method
   (vars (list-of identifier?))
   (body expression?)
   (host-name identifier?)
   (super-name identifier?)
   (field-names (list-of identifier?))
   (prop property?)))

(define method->host-name
  (lambda (m)
    (cases method m
           (a-method
            (vars body h-name s-name f-names prop)
            h-name))))

(define method->prop
  (lambda (m)
    (cases method m
           (a-method
            (vars body h-name s-name f-names prop)
            prop))))

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
   (field-decls (list-of class-field-decl?))
   (method-env method-environment?)))

;;; class->super-names : Class -> Sym
(define class->super-name
  (lambda (c)
    (cases class c
           (a-class
            (s-name f-decls m-env) s-name))))

;;; class->field-decls : Class -> Listof(FieldDecl)
(define class->field-decls
  (lambda (c)
    (cases class c
           (a-class
            (s-name f-decls m-env) f-decls))))

;;; class->field-decls : Class -> MethodEnv
(define class->method-env
  (lambda (c)
    (cases class c
           (a-class
            (s-name f-decls m-env) m-env))))

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
            (c-name s-name f-decls m-decls)
            (let ((f-decls
                   (append-field-decls
                    (class->field-decls (lookup-class s-name))
                    (map (lambda (f) (field-decl->class-field-decl f c-name)) f-decls))))
              (add-to-class-env!
               c-name
               (a-class s-name f-decls
                        (merge-method-envs
                         (class->method-env (lookup-class s-name))
                         (method-decls->method-env
                          m-decls c-name s-name (map class-field-decl->name f-decls))))))))))

;;; field-decl->class-field-decl : FieldDecl -> ClassFieldDecl
(define field-decl->class-field-decl
  (lambda (f c-name)
    (cases field-decl f
           (a-field-decl
            (prop name)
            (a-class-field-decl c-name name prop)))))

;;; append-field-decls : Listof(ClassFieldDecl) x Listof(ClassFieldDecl) -> Listof(ClassFieldDecl)
(define append-field-decls
  (lambda (super-fields new-fields)
    (cond [(null? super-fields) new-fields]
          [else
           (cons
            (if (is-field-shadowed? (car super-fields) new-fields)
                (cases class-field-decl (car super-fields)
                       (a-class-field-decl
                        (c n p)
                        (a-class-field-decl c (fresh-identifier n) p)))
                (car super-fields))
            (append-field-decls
             (cdr super-fields) new-fields))])))

;;; is-field-shadowed? : FieldDecl x Listof(FieldDecl) -> Bool
(define is-field-shadowed?
  (lambda (field-decl field-decls)
    (if (null? field-decls)
        #f
        (if (eqv? (class-field-decl->name field-decl)
                  (class-field-decl->name (car field-decls)))
            #t
            (is-field-shadowed? field-decl (cdr field-decls))))))

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

;;; method-decls->method-env : Listof(MethodDecl) x ClassName x ClassName x Listof(FieldName) -> MethodEnv
(define method-decls->method-env
  (lambda (m-decls host-name super-name field-names)
    (map
     (lambda (m-decl)
       (cases method-decl m-decl
              (default-method-decl
                (m-name vars body)
                (list m-name
                      (a-method vars body host-name super-name field-names (public-prop))))
              (a-method-decl
               (prop m-name vars body)
               (list m-name
                     (a-method vars body host-name super-name field-names prop)))))
     m-decls)))

;;; merge-method-envs : MethodEnv x MethodEnv -> MethodEnv
(define merge-method-envs
  (lambda (super-m-env self-m-env)
    (append self-m-env super-m-env)))

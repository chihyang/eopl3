#lang eopl
;;; ---------------------- Environment(from section 3.2) ----------------------
(define member?
  (lambda (sym lst)
    (if (null? lst)
        #f
        (or (eqv? sym (car lst))
            (member? sym (cdr lst))))))
(define check-duplicates
  (lambda (lst)
    (cond ((null? lst) '())
          ((member? (car lst) (cdr lst)) (car lst))
          (else (check-duplicates (cdr lst))))))
(define empty-env?
  (lambda (env)
    (and (list? env)
         (not (null? env))
         (eqv? (car env) 'empty-env))))
(define extended-env?
  (lambda (env)
    (and (list? env)
         (not (null? env))
         (or (eqv? (car env) 'extend-env)
             (eqv? (car env) 'extend-env-rec)))))
(define environment?
  (lambda (env)
    (or (empty-env? env)
        (extended-env? env))))
(define empty-env
  (lambda () (list 'empty-env)))
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))
(define extend-env-rec
  (lambda (p-names p-vars p-bodies env)
    (let ((dup-name (check-duplicates p-names)))
      (if (null? dup-name)
          (list 'extend-env-rec (list p-names p-vars p-bodies) env)
          (report-duplicate-id dup-name)))))
;; extend-env* : Listof(Id) x Listof(ExpVal) x Env -> Env
(define extend-env*
  (lambda (vars vals env)
    (let ((dup (check-duplicates vars))
          (var-len (length vars))
          (val-len (length vals)))
      (cond [(not (null? dup)) (report-duplicate-id dup)]
            [(< var-len val-len)
             (report-argument-mismatch 'greater)]
            [(> var-len val-len)
             (report-argument-mismatch 'less)]
            [else
             (letrec ((extend-env*-inner
                       (lambda (vars vals env)
                         (cond [(null? vars)
                                env]
                               [else
                                (extend-env*-inner
                                 (cdr vars)
                                 (cdr vals)
                                 (list 'extend-env (car vars) (car vals) env))]))))
               (extend-env*-inner vars vals env))]))))
(define apply-env
  (lambda (env search-var)
    (cond
     ((eqv? (car env) 'empty-env)
      (report-no-binding-found search-var))
     ((eqv? (car env) 'extend-env)
      (let ((saved-var (cadr env))
            (saved-val (caddr env))
            (saved-env (cadddr env)))
        (if (eqv? search-var saved-var)
            saved-val
            (apply-env saved-env search-var))))
      ((eqv? (car env) 'extend-env-rec)
       (let ((func (apply-env-rec search-var
                                  (car (cadr env))
                                  (cadr (cadr env))
                                  (caddr (cadr env)))))
         (if (null? func)
             (apply-env (caddr env) search-var)
             (let ((saved-p-vars (car func))
                   (saved-p-body (cadr func)))
               (newref (proc-val (procedure saved-p-vars saved-p-body env)))))))
     (else
      (report-invalid-env env)))))
;;; apply-env-rec : Sym x Listof(Sym) x Listof(Listof(Sym)) x Listof(Expression) ->
;;;                 ((Listof(Sym), Expression) | '(),
;;;                  (Listof(Sym),
;;;                   Listof(Listof(Sym)),
;;;                   Listof(Expression)))
(define apply-env-rec
  (lambda (var p-names p-vars p-bodies)
    (cond [(null? p-names) '()]
          [(eqv? var (car p-names))
           (list (car p-vars) (car p-bodies))]
          [else
           (apply-env-rec var (cdr p-names) (cdr p-vars) (cdr p-bodies))])))
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))
(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))
(define report-argument-mismatch
  (lambda (symp)
    (eopl:error 'extend-env* "Argument number is ~s than parameter number" symp)))
(define report-duplicate-id
  (lambda (sym)
    (eopl:error 'extend-env* "Duplicate identifier ~s" sym)))

;;; ---------------------- Expval ----------------------
(define identifier? symbol?)
(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body expression?)
   (saved-env environment?)))
;; apply-procedure/k : Proc x Listof(ExpVal) x Cont -> Bounce
(define apply-procedure/k
  (lambda (proc1 vals cont)
    (lambda ()
      (cases proc proc1
             (procedure
              (vars body saved-env)
              (value-of/k body (extend-env* vars (map newref vals) saved-env) cont))))))
(define-datatype exp-val exp-val?
  (num-val
   (val number?))
  (bool-val
   (val boolean?))
  (proc-val
   (val proc?))
  (null-val)
  (pair-val
   (val1 exp-val?)
   (val2 exp-val?))
  (mutex-val
   (val1 mutex?)))
(define expval->num
  (lambda (value)
    (cases exp-val value
           (num-val
            (number)
            number)
           (else
            (report-invalid-exp-value 'num)))))
(define expval->bool
  (lambda (value)
    (cases exp-val value
           (bool-val
            (boolean)
            boolean)
           (else
            (report-invalid-exp-value 'bool)))))
(define expval->proc
  (lambda (value)
    (cases exp-val value
           (proc-val
            (proc1)
            proc1)
           (else
            (report-invalid-exp-value 'proc)))))
(define expval->null
  (lambda (value)
    (cases exp-val value
           (null-val
            ()
            '())
           (else
            (report-invalid-exp-value 'null)))))
(define expval->mutex
  (lambda (value)
    (cases exp-val value
           (mutex-val
            (val1)
            val1)
           (else
            (report-invalid-exp-value 'mutex)))))
(define expval->pair
  (lambda (value)
    (cases exp-val value
           (pair-val
            (val1 val2)
            (cons
             (cases exp-val val1
                    (num-val (num) num)
                    (bool-val (bool) bool)
                    (null-val () '())
                    (proc-val (proc1) proc1)
                    (mutex-val (mtx) mtx)
                    (pair-val (val3 val4) (expval->pair val1)))
             (cases exp-val val2
                    (num-val (num) num)
                    (bool-val (bool) bool)
                    (null-val () '())
                    (proc-val (proc1) proc1)
                    (mutex-val (mtx) mtx)
                    (pair-val (val3 val4) (expval->pair val2)))))
           (else
            (report-invalid-exp-value 'pair)))))
;; expval->schemeval : ExpVal -> SchemeVal
(define expval->schemeval
  (lambda (v)
    (cases exp-val v
           (num-val
            (num)
            num)
           (bool-val
            (bool)
            bool)
           (null-val
            ()
            (eopl:pretty-print '()))
           (pair-val
            (val1 val2)
            (expval->pair v))
           (proc-val
            (p)
            (cases proc p
                   (procedure
                    (var saved-env body)
                    `(λ (,var) ...))))
           (mutex-val
            (m)
            (cases mutex m
                   (a-mutex
                    (ref-to-closed? ref-to-wait-queue)
                    `(mutex ref-to-closed? (,ref-to-closed?) ...)))))))
(define print-expval
  (lambda (val)
    (eopl:printf "~a~%" (expval->schemeval val))))
(define report-invalid-exp-value
  (lambda (type)
    (eopl:error
     'exp-val
     "Not a valid exp value of type ~s" type)))

;;; ---------------------- Store (from section 4.2) ----------------------
;; empty-store : () -> Sto
(define empty-store (lambda () '()))
;; usage : A scheme variable containing the current state of the
;; store. Initially set to a dummy value.
(define the-store 'uninitialized)
;; get-store : () -> Sto
(define get-store
  (lambda () the-store))
;; initialize-store! : () -> Unspecified
;; usage : (initialize-store!) sets the store to the empty store
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))
;; reference? : SchemeVal -> Bool
(define reference?
  (lambda (v)
    (integer? v)))
;; newref : ExpVal -> Ref
(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))
;; deref : Ref -> ExpVal
(define deref
  (lambda (ref)
    (list-ref the-store ref)))
;; setref! : Ref x ExpVal -> Unspecified
;; usage : sets the-store to a state like the original, but with position ref
;; containing val
(define setref!
  (lambda (ref val)
    (set! the-store
      (letrec ((setref-inner
                ;; usage : returns a list like store1, except that position ref1
                ;; contains val.
                (lambda (store1 ref1)
                  (cond [(null? store1)
                         (report-invalid-reference ref the-store)]
                        [(zero? ref1)
                         (cons val (cdr store1))]
                        [else
                         (cons (car store1)
                               (setref-inner (cdr store1) (- ref1 1)))]))))
        (setref-inner the-store ref)))))
(define report-invalid-reference
  (lambda (ref store)
    (eopl:error
     'exp-val
     "Not a valid reference ~a for store ~a" ref store)))

;;; ---------------------- Continuation ----------------------
;; FinalAnswer = ExpVal
;; Cont = ExpVal -> FinalAnswer
;; end-cont : () -> Cont
(define-datatype continuation continuation?
  (zero1-cont
   (cont continuation?))
  (let-exp-cont
   (var identifier?)
   (body expression?)
   (env environment?)
   (cont continuation?))
  (if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (env environment?)
   (cont continuation?))
  (diff1-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (diff2-cont
   (val1 exp-val?)
   (cont continuation?))
  (rator-cont
   (exps (list-of expression?))
   (env environment?)
   (cont continuation?))
  (rand-cont
   (val exp-val?)
   (saved-rands (list-of exp-val?))
   (cont-exps (list-of expression?))
   (env environment?)
   (cont continuation?))
  (let-cont
   (saved-vars (list-of identifier?))
   (saved-vals (list-of exp-val?))
   (cont-vars (list-of identifier?))
   (cont-exps (list-of expression?))
   (body expression?)
   (env environment?)
   (cont continuation?))
  (cons1-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (cons2-cont
   (val1 exp-val?)
   (cont continuation?))
  (car-cont
   (cont continuation?))
  (cdr-cont
   (cont continuation?))
  (null1-cont
   (cont continuation?))
  (list1-cont
   (exps expression?)
   (env environment?)
   (cont continuation?))
  (list2-cont
   (first-val exp-val?)
   (cont continuation?))
  (set-rhs-cont
   (ref reference?)
   (cont continuation?))
  (begin-exp-cont
   (exp expression?)
   (env environment?)
   (cont continuation?))
  (spawn-cont
   (saved-cont continuation?))
  (end-subthread-cont)
  (end-mainthread-cont)
  (wait-cont
   (saved-cont continuation?))
  (signal-cont
   (saved-cont continuation?))
  (print-cont
   (saved-cont continuation?))
  (yield-cont
   (saved-cont continuation?)))
;; apply-cont : Cont x ExpVal -> Bounce
(define apply-cont
  (lambda (cont val)
    (when (debug-mode?)
        (eopl:printf "thread id: ~a, remaining time slice: ~a~%"
                     the-running-thread-id the-time-remaining))
    (if (time-expired?)
        (begin
          (place-on-ready-queue!
           (cont-thread the-max-time-slice the-running-thread-id cont val))
          (run-next-thread))
        (begin
          (decrease-timer!)
          (cases continuation cont
                 (zero1-cont
                  (cont)
                  (apply-cont cont (bool-val (zero? (expval->num val)))))
                 (let-exp-cont
                  (var body env cont)
                  (value-of/k body (extend-env var val env) cont))
                 (if-test-cont
                  (exp2 exp3 env cont)
                  (if (expval->bool val)
                      (value-of/k exp2 env cont)
                      (value-of/k exp3 env cont)))
                 (diff1-cont
                  (exp2 env cont)
                  (value-of/k exp2 env (diff2-cont val cont)))
                 (diff2-cont
                  (val1 cont)
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val)))
                    (apply-cont cont (num-val (- num1 num2)))))
                 (rator-cont
                  (exps env cont)
                  (if (null? exps)
                      (apply-procedure/k (expval->proc val) '() cont)
                      (value-of/k (car exps) env (rand-cont val '() (cdr exps) env cont))))
                 (rand-cont
                  (rator saved-vals cont-exps env cont)
                  (if (null? cont-exps)
                      (let ((proc1 (expval->proc rator)))
                        (apply-procedure/k proc1 (reverse (cons val saved-vals)) cont))
                      (value-of/k (car cont-exps)
                                  env
                                  (rand-cont rator (cons val saved-vals) (cdr cont-exps) env cont))))
                 (let-cont
                  (saved-vars saved-vals cont-vars cont-exps body env cont)
                  (if (null? cont-exps)
                      (let ((l-vars (reverse (cons (car cont-vars) saved-vars)))
                            (l-vals (reverse (cons val saved-vals))))
                        (value-of/k body
                                    (extend-env* l-vars
                                                 (map newref l-vals)
                                                 env)
                                    cont))
                      (value-of/k (car cont-exps)
                                  env
                                  (let-cont (cons (car cont-vars) saved-vars)
                                            (cons val saved-vals)
                                            (cdr cont-vars)
                                            (cdr cont-exps)
                                            body
                                            env
                                            cont))))
                 (cons1-cont
                  (exp2 env cont)
                  (value-of/k exp2 env (cons2-cont val cont)))
                 (cons2-cont
                  (val1 cont)
                  (apply-cont cont (pair-val val1 val)))
                 (car-cont
                  (cont)
                  (cases exp-val val
                         (pair-val
                          (first rest)
                          (apply-cont cont first))
                         (else (report-invalid-exp-value 'pair-val))))
                 (cdr-cont
                  (cont)
                  (cases exp-val val
                         (pair-val
                          (first rest)
                          (apply-cont cont rest))
                         (else (report-invalid-exp-value 'pair-val))))
                 (null1-cont
                  (cont)
                  (cases exp-val val
                         (null-val
                          ()
                          (apply-cont cont (bool-val #t)))
                         (else (apply-cont cont (bool-val #f)))))
                 (list1-cont
                  (exps env cont)
                  (value-of/k exps env (list2-cont val cont)))
                 (list2-cont
                  (first-val cont)
                  (apply-cont cont (pair-val first-val val)))
                 (set-rhs-cont
                  (ref cont)
                  (begin
                    (setref! ref val)
                    (apply-cont cont (num-val 27))))
                 (begin-exp-cont
                  (exp env cont)
                  (value-of/k exp env cont))
                 (spawn-cont
                  (saved-cont)
                  (let ((proc1 (expval->proc val))
                        (th-id the-thread-number))
                    (when (debug-mode?)
                      (eopl:printf "Create new thread ~a.~%" th-id))
                    (set! the-thread-number (+ th-id 1))
                    (place-on-ready-queue!
                     (proc-thread
                      the-max-time-slice
                      th-id
                      proc1
                      (list (num-val th-id))
                      (end-subthread-cont)))
                    (apply-cont saved-cont (num-val 73))))
                 (end-mainthread-cont
                  ()
                  (when (debug-mode?)
                    (eopl:printf "Main thread exit.~%"))
                  (set-final-answer! val)
                  (run-next-thread))
                 (end-subthread-cont
                  ()
                  (when (debug-mode?)
                    (eopl:printf "Subthread exit.~%"))
                  (run-next-thread))
                 (wait-cont
                  (saved-cont)
                  (wait-for-mutex
                   (expval->mutex val)
                   (cont-thread
                    the-max-time-slice
                    the-running-thread-id
                    saved-cont
                    (num-val 52))))
                 (signal-cont
                  (saved-cont)
                  (signal-mutex
                   (expval->mutex val)
                   (cont-thread
                    the-max-time-slice
                    the-running-thread-id
                    saved-cont
                    (num-val 53))))
                 (print-cont
                  (saved-cont)
                  (begin
                    (print-expval val)
                    (apply-cont saved-cont (num-val 33))))
                 (yield-cont
                  (saved-cont)
                  (begin
                    (when (debug-mode?)
                      (eopl:printf "Force switch thread ~a with yield.~%"
                                   the-running-thread-id))
                    (if (time-expired?)
                        (place-on-ready-queue!
                         (cont-thread the-max-time-slice the-running-thread-id saved-cont val))
                        (place-on-ready-queue!
                         (cont-thread the-time-remaining the-running-thread-id saved-cont val)))
                    (run-next-thread))))))))
;;; wait-for-mutex : Mutex x Thread -> FinalAnswer
(define wait-for-mutex
  (lambda (m th)
    (cases mutex m
           (a-mutex
            (ref-to-closed? ref-to-wait-queue)
            (if (deref ref-to-closed?)
                (begin
                  (setref! ref-to-wait-queue
                           (enqueue (deref ref-to-wait-queue) th))
                  (run-next-thread))
                (begin
                  (setref! ref-to-closed? #t)
                  (apply-thread th)))))))
;;; signal-mutex : Mutex x Thread -> FinalAnswer
(define signal-mutex
  (lambda (m th)
    (cases mutex m
           (a-mutex
            (ref-to-closed? ref-to-wait-queue)
            (let ((closed? (deref ref-to-closed?))
                  (wait-queue (deref ref-to-wait-queue)))
              (if closed?
                  (if (empty? wait-queue)
                      (begin
                        (setref! ref-to-closed? #f)
                        (apply-thread th))
                      (begin
                        (dequeue
                         wait-queue
                         (lambda (first-waiting-th other-waiting-ths)
                           (place-on-ready-queue!
                            first-waiting-th)
                           (setref! ref-to-wait-queue other-waiting-ths)))
                        (apply-thread th)))
                  (apply-thread th)))))))

;;; ---------------------- Mutex Interface ----------------------
(define-datatype mutex mutex?
  (a-mutex
   (ref-to-closed? reference?)
   (ref-to-wait-queue reference?)))
;;; new-mutex : () -> Mutex
(define new-mutex
  (lambda ()
    (a-mutex
     (newref #f)
     (newref (empty-queue)))))

;;; ---------------------- Thread ----------------------
(define-datatype thread thread?
  (cont-thread
   (times-slice integer?)
   (id integer?)
   (saved-cont continuation?)
   (saved-val exp-val?))
  (proc-thread
   (time-slice integer?)
   (id integer?)
   (proc1 proc?)
   (vals (list-of exp-val?))
   (saved-cont continuation?)))
(define apply-thread
  (lambda (th)
    (cases thread th
           (cont-thread
            (t i c v)
            (when (debug-mode?)
              (eopl:printf "Resume thread ~a.~%" the-running-thread-id))
            (apply-cont c v))
           (proc-thread
            (t i p v c)
            (when (debug-mode?)
              (eopl:printf "Start thread ~a.~%" the-running-thread-id))
            (apply-procedure/k p v c)))))
(define thread-id
  (lambda (th)
    (cases thread th
           (cont-thread
            (t i c v)
            i)
           (proc-thread
            (t i p v c)
            i))))
(define thread-time-slice
  (lambda (th)
    (cases thread th
           (cont-thread
            (t i c v)
            t)
           (proc-thread
            (t i p v c)
            t))))

;;; ---------------------- Thread Interface ----------------------
;;; the current running thread's id
(define the-running-thread-id 'unspecified)
;;; thread number
(define the-thread-number 'unspecified)
;;; the ready queue
(define the-ready-queue 'unspecified)
;;; the value of the main thread, if done
(define the-final-answer 'unspecified)
;;; the number of steps that each thread may run
(define the-max-time-slice 'unspecified)
;;; the number of steps remaining for the currently running thread
(define the-time-remaining 'unspecified)
;;; initialize-scheduler! : Int -> Unspecified
;;; usage : initializes the scheduler state
(define initialize-scheduler!
  (lambda (ticks)
    (set! the-running-thread-id 0)
    (set! the-thread-number 1)
    (set! the-ready-queue (empty-queue))
    (set! the-final-answer 'uninitialized)
    (set! the-max-time-slice ticks)
    (set! the-time-remaining the-max-time-slice)))
;;; place-on-ready-queue! : Thread -> Unspecified
;;; usage : places thread on the ready queue
(define place-on-ready-queue!
  (lambda (th)
    (set! the-ready-queue
      (enqueue the-ready-queue th))))
;;; run-next-thread : () -> FinalAnswer usage : runs next thread. If no ready
;;; threads, returns the final answer
(define run-next-thread
  (lambda ()
    (if (empty? the-ready-queue)
        (begin
          (when (debug-mode?)
            (eopl:printf "End of computation.~%"))
          the-final-answer)
        (begin
          (when (debug-mode?)
            (eopl:printf "Current ready queue: ~a.~%"
                         (map thread-id (queue->list the-ready-queue)))
            (eopl:printf "Switch to another thread.~%"))
          (dequeue the-ready-queue
                   (lambda (first-ready-thread other-ready-thread)
                     (set! the-ready-queue other-ready-thread)
                     (set! the-time-remaining (thread-time-slice first-ready-thread))
                     (set! the-running-thread-id (thread-id first-ready-thread))
                     (apply-thread first-ready-thread)))))))
;;; set-final-answer! : ExpVal -> Unspecified
;;; usage : sets the final answer
(define set-final-answer!
  (lambda (val)
    (set! the-final-answer val)))
;;; time-expired? : () -> Bool
;;; usage : tests whether timer is 0
(define time-expired?
  (lambda ()
    (zero? the-time-remaining)))
;;; decrease-timer! : () -> Unspecified
;;; usage : decrements time-remaining
(define decrease-timer!
  (lambda ()
    (set! the-time-remaining (- the-time-remaining 1))))

;;; ---------------------- Queue For THREAD ----------------------
;;; empty-queue : () -> Queue
(define empty-queue
  (lambda ()
    '(() . ())))
;;; enqueue : Queue x SchemeVal -> Queue
(define enqueue
  (lambda (queue val)
    (cons (car queue)
          (cons val (cdr queue)))))
;;; empty? : Queue -> Bool
(define empty?
  (lambda (q)
    (and (null? (car q)) (null? (cdr q)))))
;;; dequeue : Queue x Proc -> Unspecified
(define dequeue
  (lambda (q f)
    (if (empty? q)
        (report-empty-queue)
        (let ((front (car q))
              (back (cdr q)))
          (if (null? front)
              (let* ((new-front (reverse back))
                     (first (car new-front))
                     (rest (cons (cdr new-front) '())))
                (f first rest))
              (let ((first (car front))
                    (rest (cons (cdr front) back)))
                (f first rest)))))))
(define queue->list
  (lambda (q)
    (if (empty? q)
        '()
        (append (car q) (reverse (cdr q))))))
;;; report-empty-queue : () -> Unspecified
(define report-empty-queue
  (lambda ()
    'dequeue
    (eopl:error "Attemp to dequeue an empty queue!")))

;;; ---------------------- Syntax for the THREAD language ----------------------
;;; Program    ::= Expression
;;;                a-program (exp1)
;;; Expression ::= Number
;;;                const-exp (num)
;;; Expression ::= -(Expression , Expression)
;;;                diff-exp (exp1 exp2)
;;; Expression ::= zero? (Expression)
;;;                zero?-exp (exp1)
;;; Expression ::= if Expression then Expression else Expression
;;;                if-exp (exp1 exp2 exp3)
;;; Expression ::= Identifier
;;;                var-exp (var)
;;; Expression ::= emptylist
;;;                emptylist-exp
;;; Expression ::= cons (Expression, Expression)
;;;                cons-exp (exp1 exp2)
;;; Expression ::= car (Expression)
;;;                car-exp (exp1)
;;; Expression ::= cdr (Expression)
;;;                cdr-exp (exp1)
;;; Expression ::= null? (Expression)
;;;                null?-exp (exp1)
;;; Expression ::= list (Expression, Expression, ...)
;;;                list-exp (exp1)
;;; Expression ::= let {Identifier = Expression}* in Expression
;;;                let-exp (var exp1 body)
;;; Expression ::= letrec {Identifier (Identifier*,)}* = Expression in Expression
;;;                letrec-exp (p-name b-var p-exp1 letrec-body)
;;; Expression ::= proc (Identifier*,) Expression
;;;                proc-exp (var body)
;;; Expression ::= (Expression Expression*)
;;;                call-exp (rator rand)
;;; Expression ::= begin Expression {; Expression}* end
;;;                begin-exp (exp1 exps)
;;; Expression ::= set Identifier = Expression
;;;                assign-exp (var exp1)
;;; Expression ::= mutex()
;;;                mutex-exp
;;; Expression ::= wait (Expression)
;;;                wait-exp (exp1)
;;; Expression ::= signal (Expression)
;;;                signal-exp (exp1)
;;; Expression ::= print (Expression)
;;;                print (exp1)
;;; Expression ::= yield ()
;;;                yield ()
;;; Parse Expression
(define let-scanner-spec
  '((white-sp (whitespace) skip)
    (identifier (letter (arbno (or letter digit "_" "-"))) symbol)
    (number ((or (concat digit (arbno digit))
                 (concat "-" digit (arbno digit))
                 (concat (arbno digit) "." digit (arbno digit))
                 (concat "-" (arbno digit) "." digit (arbno digit))
                 (concat digit (arbno digit) "." (arbno digit))
                 (concat "-" digit (arbno digit) "." (arbno digit)))) number)
    (comment ("%" (arbno (not #\newline))) skip)))
(define let-grammar
  '((program (expression) a-program)
    (expression (number)
                const-exp)
    (expression ("emptylist")
                emptylist-exp)
    (expression ("cons" "(" expression "," expression ")")
                cons-exp)
    (expression ("car" "(" expression ")")
                car-exp)
    (expression ("cdr" "(" expression ")")
                cdr-exp)
    (expression ("null?" "(" expression ")")
                null?-exp)
    (expression ("list" "(" (separated-list expression ",") ")")
                list-exp)
    (expression ("-" "(" expression "," expression ")")
                diff-exp)
    (expression ("zero?" "(" expression ")")
                zero?-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression (identifier)
                var-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression)
                letrec-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression)
                proc-exp)
    (expression ("(" expression (arbno expression) ")")
                call-exp)
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)
    (expression ("set" identifier "=" expression)
                assign-exp)
    (expression ("spawn" "(" expression ")")
                spawn-exp)
    (expression ("mutex" "(" ")")
                mutex-exp)
    (expression ("wait" "(" expression ")")
                wait-exp)
    (expression ("signal" "(" expression ")")
                signal-exp)
    (expression ("print" "(" expression ")")
                print-exp)
    (expression ("yield" "(" ")")
                yield-exp)))

;;; ---------------------- Evaluate expression ----------------------
;; value-of/k : Exp x Env x Cont -> Bounce
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
           (const-exp
            (num)
            (apply-cont cont (num-val num)))
           (var-exp
            (var)
            (apply-cont cont (deref (apply-env env var))))
           (emptylist-exp
            ()
            (apply-cont cont (null-val)))
           (cons-exp
            (exp1 exp2)
            (value-of/k exp1 env (cons1-cont exp2 env cont)))
           (car-exp
            (exp1)
            (value-of/k exp1 env (car-cont cont)))
           (cdr-exp
            (exp1)
            (value-of/k exp1 env (cdr-cont cont)))
           (null?-exp
            (exp1)
            (value-of/k exp1 env (null1-cont cont)))
           (list-exp
            (exps)
            (if (null? exps)
                (apply-cont cont (null-val))
                (value-of/k (car exps) env (list1-cont (list-exp (cdr exps)) env cont))))
           (diff-exp
            (exp1 exp2)
            (value-of/k exp1 env (diff1-cont exp2 env cont)))
           (zero?-exp
            (exp1)
            (value-of/k exp1 env (zero1-cont cont)))
           (if-exp
            (exp1 exp2 exp3)
            (value-of/k exp1 env (if-test-cont exp2 exp3 env cont)))
           (let-exp
            (vars exps body)
            (if (null? vars)
                (value-of/k body env cont)
                (value-of/k (car exps) env (let-cont '() '()  vars (cdr exps) body env cont))))
           (letrec-exp
            (p-names p-vars p-bodies letrec-body)
            (value-of/k letrec-body (extend-env-rec p-names p-vars p-bodies env) cont))
           (proc-exp
            (vars body)
            (apply-cont cont (proc-val (procedure vars body env))))
           (call-exp
            (rator rand)
            (value-of/k rator env (rator-cont rand env cont)))
           (begin-exp
            (exp1 exps)
            (if (null? exps)
                (value-of/k exp1 env cont)
                (value-of/k exp1 env (begin-exp-cont (begin-exp (car exps) (cdr exps)) env cont))))
           (assign-exp
            (var exp1)
            (let ((ref (apply-env env var)))
              (value-of/k exp1 env (set-rhs-cont ref cont))))
           (spawn-exp
            (exp1)
            (value-of/k exp1 env (spawn-cont cont)))
           (mutex-exp
            ()
            (apply-cont cont (mutex-val (new-mutex))))
           (wait-exp
            (exp1)
            (value-of/k exp1 env (wait-cont cont)))
           (signal-exp
            (exp1)
            (value-of/k exp1 env (signal-cont cont)))
           (print-exp
            (exp1)
            (value-of/k exp1 env (print-cont cont)))
           (yield-exp
            ()
            (apply-cont (yield-cont cont) (num-val 99))))))
;; value-of-program : Int x Program -> FinalAnswer
(define value-of-program
  (lambda (timeslice prog debug?)
    (initialize-store!)
    (initialize-scheduler! timeslice)
    (toggle-debug-mode debug?)
    (when debug?
      (eopl:printf "~%Begin a new computation.~%"))
    (cases program prog
           (a-program
            (exp)
            (let ((val (trampoline (value-of/k exp (empty-env) (end-mainthread-cont)))))
              (expval->schemeval val))))))
;; trampoline : Bounce -> FinalAnswer
(define trampoline
  (lambda (bounce)
    (if (procedure? bounce)
        (trampoline (bounce))
        bounce)))
;; toggle-debug-mode : Bool -> Bool
(define toggle-debug-mode
  (lambda (dbg)
    (set! global-debug-mode? dbg)
    global-debug-mode?))
;; debug-mode? : () -> Bool
(define debug-mode?
  (lambda () global-debug-mode?))
(define global-debug-mode? #f)

;;; ---------------------- Sllgen operations ----------------------
(sllgen:make-define-datatypes let-scanner-spec let-grammar)
(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes let-scanner-spec let-grammar)))
(define just-scan
  (sllgen:make-string-scanner let-scanner-spec let-grammar))
(define scan&parse
  (sllgen:make-string-parser let-scanner-spec let-grammar))
(define run
  (lambda (exp #:debug? (dbg #f) #:time-slice (time-slice 3))
    (value-of-program time-slice (scan&parse exp) dbg)))

;;; ---------------------- Test ----------------------
(require rackunit)
(check-eqv?
 (run "letrec double (x) = if zero?(x) then 0
                       else -((double -(x,1)),-2)
       in (double 6)")
 12)
(check-eqv?
 (run "let x = 3 f = proc(x) -(x, -3) in (f 3)")
 6)
(check-eqv?
 (run "let x = 3 f = proc(x) -(x, -3) y = 4 in -(x, y)")
 -1)

;; tests from exercise 3.16
(check-equal?
 (run "let x = 4 in cons(x, cons(cons(-(x,1), emptylist), emptylist))")
 '(4 (3)))
(check-eqv?
 (run "null?(cdr(let x = 4 in
                 cons(x, cons(cons(-(x,1),
                                   emptylist),
                              emptylist))))")
 #f)
(check-equal?
 (run
  "let x = 4 in list(x, -(x,1), -(x,3))")
 '(4 3 1))
(check-eqv?
 (run "let x = 30
         in let x = -(x,1)
                y = -(x,2)
           in -(x,y)")
 1)

;; tests from exercise 3.21
(check-eqv?
 (run "((proc (x) proc (y) -(y,-(0,x)) 3) 4)")
 7)
(check-eqv?
 (run "(proc (x, y) -(y,-(0,x)) 3 4)")
 7)

;; tests from exercise 3.32
(check-eqv?
 (run "letrec
         even(x) = if zero?(x) then 1 else (odd -(x,1))
         odd(x)  = if zero?(x) then 0 else (even -(x,1))
       in (odd 13)")
 1)
(check-eqv?
 (run "let x = 3 in
       letrec
         even(x) = if zero?(x) then 1 else (odd -(x,1))
         odd(x)  = if zero?(x) then 0 else (even -(x,1))
       in let y = x in (even y)")
 0)

;; tests from exercise 4.17 and 4.18
(check-eqv?
 (run "let x = 22
        in let f = proc (z)
                    let zz = -(z,x)
                    in zz
           in -((f 66), (f 55))")
 11)
(check-eqv?
 (run "letrec times4(x) = if zero?(x) then 0
                         else -((times4 -(x,1)), -4)
      in (times4 3)")
 12)
(check-eqv?
 (run "let x = 0
      in letrec even()
                 = if zero? (x)
                   then 1
                   else begin
                         set x = -(x, 1);
                         (odd)
                        end
                odd()
                 = if zero? (x)
                   then 0
                   else begin
                         set x = -(x, 1);
                         (even)
                        end
      in begin set x = 13; (odd) end")
 1)

;;; let without binding
(check-eqv?
 (run "let in 3")
 3)

;;; two-thread print
(check-eqv?
 (run "letrec noisy (l) = if null? (l) then 0
                         else begin print (car(l)); (noisy cdr(l)) end
       in
        begin
          spawn(proc (d) (noisy list(1,2,3,4,5)));
          spawn(proc (d) (noisy list(6,7,8,9,10)));
          print(100);
          33
        end"
      #:debug? #t)
 33)

;;; producer and consumer
(check-eqv?
 (run "let buffer = 0
       in let producer = proc (n)
              letrec
                wait4(k) = if zero?(k)
                           then set buffer = n
                           else begin
                                  print(-(k,-200));
                                  (wait4 -(k,1))
                                end
                in (wait4 5)
           in let consumer = proc ()
                  letrec busywait(k) = if zero?(buffer)
                                       then begin
                                             print(-(k,-100));
                                             (busywait -(k,-1))
                                            end
                                       else buffer
                  in (busywait 0)
              in begin
                  spawn(proc (d) (producer 44));
                  print(300);
                  (consumer)
                 end"
      #:debug? #t)
 44)

;;; unsafe counter, the final value of x is uncertain
(run "let x = 0
      in let mut = mutex()
         in let incr_x = proc (id)
                           proc (dummy)
                             begin
                              set x = -(x,-1);
                              x
                             end
            in begin
                spawn((incr_x 100));
                spawn((incr_x 200));
                spawn((incr_x 300));
                x
               end"
     #:debug? #f
     #:time-slice 2)

;;; safe counter, the final value of x is certain but we cannot get it, see
;;; exercise for improvement
(run "let x = 0
      in let mut = mutex()
         in let incr_x = proc (id)
                           proc (dummy)
                             begin
                              wait(mut);
                              set x = -(x,-1);
                              signal(mut);
                              x
                             end
            in begin
                spawn((incr_x 100));
                spawn((incr_x 200));
                spawn((incr_x 300));
                x
               end"
     #:debug? #t
     #:time-slice 3)

;;; two-thread print with yield
(check-eqv?
 (run "letrec noisy (l) = if null? (l) then 0
                         else begin yield(); print (car(l)); (noisy cdr(l)) end
       in
        begin
          spawn(proc (d) (noisy list(1,2,3,4,5)));
          spawn(proc (d) (noisy list(6,7,8,9,10)));
          print(yield());
          33
        end"
      #:debug? #t
      #:time-slice 50)
 33)

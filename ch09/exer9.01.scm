#lang eopl

(provide tests-for-run tests-for-parse)

(define the-test-suite
  '(
    (queue
     "
class queue extends object
  field head
  field tail
  method initialize ()
    begin
      set head = emptylist;
      set tail = emptylist
    end
  method empty? ()
    if null? (head)
    then
      if null? (tail)
      then zero?(0)
      else zero?(1)
    else zero?(1)
  method enqueue (elem)
    set tail = cons (elem, tail)
  method dequeue ()
    if null? (head)
    then
      letrec reverse(lst, rst) =
        if null?(lst)
        then rst
        else (reverse cdr(lst) cons(car(lst), rst))
      in begin
           set head = (reverse tail head);
           set tail = emptylist;
           car(head)
         end
    else
      let fst = car(head)
      in begin
           set head = cdr(head);
           fst
         end
let x = new queue() y = 0
in begin
     send x enqueue(3);
     set y = send x dequeue();
     y
   end"

     3
     )
    )
  )

(define tests-for-run the-test-suite)

(define tests-for-parse
  (let loop ((lst the-test-suite))
    (cond
     ((null? lst) '())
     ((> (length (car lst)) 3)
      (cons
       (list
        (list-ref (car lst) 0)
        (list-ref (car lst) 1)
        (list-ref (car lst) 3))
       (loop (cdr lst))))
     (else
      (cons
       (list
        (list-ref (car lst) 0)
        (list-ref (car lst) 1)
        #t)
       (loop (cdr lst)))))))

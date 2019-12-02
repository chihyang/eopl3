#lang eopl

(provide tests-for-run tests-for-parse)

(define the-test-suite
  '(
    (queue
     "
class counter extends object
  field a_counter
  method initialize() set a_counter = 0
  method inc() set a_counter = -(a_counter, -1)
  method get() a_counter
class queue extends object
  field head
  field tail
  field my_count
  field total_count
  method initialize (cnt_ref)
    begin
      set head = emptylist;
      set tail = emptylist;
      set my_count = 0;
      set total_count = cnt_ref
    end
  method empty? ()
    if null? (head)
    then
      if null? (tail)
      then zero?(0)
      else zero?(1)
    else zero?(1)
  method enqueue (elem)
    begin
      send total_count inc();
      set my_count = -(my_count, -1);
      set tail = cons (elem, tail)
    end
  method dequeue ()
    begin
      send total_count inc();
      set my_count = -(my_count, -1);
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
    end
  method get_my_count () my_count
let global_cnt = new counter()
in let x = new queue(global_cnt)
       z = new queue(global_cnt)
       y = 0
in begin
     send x enqueue(3);
     set y = send x dequeue();
     send z enqueue(5);
     send z dequeue();
     -(send global_cnt get(), send x get_my_count())
   end"

     2
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

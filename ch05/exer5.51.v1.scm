#lang eopl
(require rackunit)
(require "exer5.46-47.scm")

;;; producer and consumer with loosewait: if scheduler is fair enough, consumer
;;; would not need to wait for too long and most of time it is in wait queue,
;;; but if time slice is too big, consumer would have to wait like in figure
;;; 5.17
(map
 (lambda (d)
   (check-eqv?
    (run "let buffer = 0
           mut = mutex()
       in let producer = proc (n)
              letrec
                wait4(k) = if zero?(k)
                           then set buffer = n
                           else begin
                                  print(-(k,-200));
                                  (wait4 -(k,1))
                                end
                in begin wait(mut); (wait4 5); signal(mut) end
           in let consumer = proc ()
                  letrec loosewait(d) =
                          begin
                           wait(mut);
                           if zero?(buffer) then
                            begin
                              signal(mut);
                              print(-(d,-100));
                              (loosewait -(d, -1))
                            end
                           else buffer
                          end
                  in (loosewait 0)
              in begin
                  spawn(proc (d) (producer 44));
                  print(300);
                  (consumer)
                 end"
         #:time-slice d)
    44)
   (eopl:printf "~%"))
 '(1 10 100))

;;; with yield, however, the consumer could give up its CPU time slice, thus
;;; making loosewait unnecessary
(map
 (lambda (d)
   (check-eqv?
    (run "let buffer = 0
           mut = mutex()
       in let producer = proc (n)
              letrec
                wait4(k) = if zero?(k)
                           then set buffer = n
                           else begin
                                  print(-(k,-200));
                                  (wait4 -(k,1))
                                end
                in begin wait(mut); (wait4 5); signal(mut) end
           in let consumer = proc ()
                              begin
                               yield();
                               wait(mut);
                               buffer
                              end
              in begin
                  spawn(proc (d) (producer 44));
                  print(300);
                  (consumer)
                 end"
         #:time-slice d)
    44)
   (eopl:printf "~%"))
 '(1 10 100))

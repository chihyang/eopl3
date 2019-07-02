#lang eopl
(require rackunit)
(require "exer5.46-47.scm")

;;; safe counter, the final value of x is certain but we cannot get it, see
;;; exercise for improvement
(map
 (lambda (d)
   (check-eqv?
    (run "let th_cnt = 0
          x = 0
      in let mut = mutex()
         in let incr_x = proc (id)
                           proc (dummy)
                             begin
                              wait(mut);
                              set x = -(x,-1);
                              set th_cnt = -(th_cnt, -1);
                              signal(mut);
                              x
                             end
            in letrec loosewait(d) =
                          begin
                           wait(mut);
                           if zero?(-(th_cnt, 3)) then
                            begin
                             signal(mut);
                             x
                            end
                           else
                            begin
                              signal(mut);
                              (loosewait -(d, -1))
                            end
                          end
               in begin
                spawn((incr_x 100));
                spawn((incr_x 200));
                spawn((incr_x 300));
                (loosewait 0);
                x
               end"
         #:debug? #f
         #:time-slice 3)
    3))
 '(1 10 100))

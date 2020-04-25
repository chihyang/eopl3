#lang eopl

(provide tests-for-run tests-for-parse)

(define the-test-suite
  '(
    (similarpoints-1
     "
class point extends object
  field x
  field y
  method initialize (initx, inity)
    begin
      set x = initx;
      set y = inity
    end
  method move (dx, dy)
    begin
      set x = -(x,-(0,dx));
      set y = -(y,-(0,dy))
    end
  method get_location () list(x,y)
  method getx() x
  method gety() y
  method similarpoints (pt)
    if equal?(send pt getx(), x)
    then equal?(send pt gety(), y)
    else zero?(1)
class colorpoint extends point
  field color
  method initialize (initx, inity, initcolor)
    begin
     set x = initx;
     set y = inity;
     set color = initcolor
    end
  method set_color (c) set color = c
  method get_color () color
let p1 = new point(3, 4)
    p2 = new point(3, 4)
    p3 = new point(10, 8)
    cp1 = new colorpoint(10, 20, 0)
    cp2 = new colorpoint(10, 20, 2)
    cp3 = new colorpoint(3, 4, 2)
in list(send p1 similarpoints(p1),
        send p1 similarpoints(p2),
        send p1 similarpoints(p3),
        send p1 similarpoints(cp1),
        send p1 similarpoints(cp2),
        send p1 similarpoints(cp3),
        send cp3 similarpoints(p1),
        send cp3 similarpoints(p3),
        send cp3 similarpoints(cp1),
        send cp3 similarpoints(cp2))"
     (#t #t #f #f #f #t #t #f #f #f)
     )

    (similarpoints-2
     "
class point extends object
  field x
  field y
  method initialize (initx, inity)
    begin
      set x = initx;
      set y = inity
    end
  method move (dx, dy)
    begin
      set x = -(x,-(0,dx));
      set y = -(y,-(0,dy))
    end
  method get_location () list(x,y)
  method getx() x
  method gety() y
  method similarpoints (pt)
    if equal?(send pt getx(), x)
    then equal?(send pt gety(), y)
    else zero?(1)
class colorpoint extends point
  field color
  method initialize (initx, inity, initcolor)
    begin
     set x = initx;
     set y = inity;
     set color = initcolor
    end
  method similarpoints (pt)
    if super similarpoints(pt)
    then equal?(send pt get_color(), color)
    else zero?(1)
  method set_color (c) set color = c
  method get_color () color
let p1 = new point(3, 4)
    p2 = new point(3, 4)
    p3 = new point(10, 8)
    cp1 = new colorpoint(10, 20, 0)
    cp2 = new colorpoint(10, 20, 2)
    cp3 = new colorpoint(3, 4, 2)
in list(send p1 similarpoints(p1),
        send p1 similarpoints(p2),
        send p1 similarpoints(p3),
        send p1 similarpoints(cp1),
        send p1 similarpoints(cp2),
        send p1 similarpoints(cp3),
        send cp3 similarpoints(p1),  % error
        send cp3 similarpoints(p3),  % error
        send cp3 similarpoints(cp1),
        send cp3 similarpoints(cp2))"
     error
     )

    (similarpoints-3
     "
class point extends object
  field x
  field y
  method initialize (initx, inity)
    begin
      set x = initx;
      set y = inity
    end
  method move (dx, dy)
    begin
      set x = -(x,-(0,dx));
      set y = -(y,-(0,dy))
    end
  method get_location () list(x,y)
  method getx() x
  method gety() y
  method similarpoints (pt)
    if equal?(send pt getx(), x)
    then equal?(send pt gety(), y)
    else zero?(1)
class colorpoint extends point
  field color
  method initialize (initx, inity, initcolor)
    begin
     set x = initx;
     set y = inity;
     set color = initcolor
    end
  method similarpoints (pt)
    if instanceof pt colorpoint
    then if super similarpoints(pt)
         then equal?(send pt get_color(), color)
         else zero?(1)
    else zero?(1)
  method set_color (c) set color = c
  method get_color () color
let p1 = new point(3, 4)
    p2 = new point(3, 4)
    p3 = new point(10, 8)
    cp1 = new colorpoint(10, 20, 0)
    cp2 = new colorpoint(10, 20, 2)
    cp3 = new colorpoint(3, 4, 2)
in list(send p1 similarpoints(p1),
        send p1 similarpoints(p2),
        send p1 similarpoints(p3),
        send p1 similarpoints(cp1),
        send p1 similarpoints(cp2),
        send p1 similarpoints(cp3),
        send cp3 similarpoints(p1),  % error
        send cp3 similarpoints(p3),  % error
        send cp3 similarpoints(cp1),
        send cp3 similarpoints(cp2))"
     (#t #t #f #f #f #t #f #f #f #f)
     )


    )
  )

(define tests-for-run
  (let loop ((lst the-test-suite))
    (cond
     ((null? lst) '())
     ((>= (length (car lst)) 3)
      ;; (printf "creating item: ~s~%" (caar lst))
      (cons
       (list
        (list-ref (car lst) 0)
        (list-ref (car lst) 1)
        (list-ref (car lst) 2))
       (loop (cdr lst))))
     (else (loop (cdr lst))))))

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

Notes on chapter 8
==================

# Exercise 8.10

> We could also use a feature like `depends-on` to control when module bodies
> are evaluated. Add this capability to SIMPLE-MODULES by requiring an `imports`
> clause to each module body and program body. `imports` is like `depends-on`,
> but has the additional property that the body of a module is evaluated only
> when it is imported by some other module (using an `imports` clause).
>
> Thus if our language had print expressions, the program
>
> ``` racket
> module m1
>   interface [] body [x = print(1)]
> module m2
>   interface [] body [x = print(2)]
> module m3
>   interface []
>   body
>     import m2
>     [x = print(3)]
> import m3, m1
> 33
> ```
>
> would print 2, 3, and 1 before returning 33. Here the modules have empty
> interfaces, because we are only concerned with the order in which the bodies
> are evaluated.

For interpreter, a lazy-evaluation like thunk is used for module body
evaluation. Modules not `import`ed will never be evaluated. But for checker, all
the modules are type checked to ensure type safety. Thus, the program below will
fail in checker but pass in interpreter.

``` racket
module m1
  interface [x : bool] body [x = print(1)]
module m2
  interface [x : int] body [x = print(2)]
module m3
  interface [x : int]
  body
    import m2
    [x = print(3)]
import m3
from m3 take x
```

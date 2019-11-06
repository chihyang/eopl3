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

# Exercise 8.17

> As you did in exercise 8.8, remove the restriction that a module must produce
> the values in the same order as the interface. Remember, however, that the
> definition must respect scoping rules, especially for types.

This one is quite subtle, consider the following case:

``` racket
module m1
 interface
  [
   opaque u
   transparent t = u
  ]
 body
  [
   type t = bool
   type u = t
  ]
% something that uses type u and t
...
```

Should it fail or pass? In the declaration, `t` is a type that depends on `u`;
in the definition, `t` is a type depended on by `u`. Even though it does not
seem to make any damage to the module user, it does cause an inconsistency
between the implementation and declaration. Worse is when the declared type is
used by some other variable definitions:

``` racket
module m1
 interface
  [
   opaque u
   transparent t = u
   foo = (u -> t)
  ]
 body
  [
   type t = bool
   foo = proc (a : u)  % some definitions
   type u = t
  ]
% something that uses type u and t
...
```

According to declarations, `foo` is a procedure that takes an argument of `u`,
but in the definition, because `u` is defined after `foo`, `foo` cannot see
it. This make the program fail to pass type checking directly. Maybe this is the
reason why *the definition must respect scoping rules, especially for
types*. Surprisingly, my modification behaves this way naturally, even before I
find it out why.

# Exercise 8.18

> Our code depends on the invariant that every type in a type environment is
> already expanded. We enforce this invariant by calling `expand-type` in many
> places in the code. On the other hand, it would be easy to break the system by
> forgetting to call `expand-type`. Refactor the code so that there are fewer
> calls to `expand-type`, and the invariant is maintained more robustly.

Since most of time `expand-type` is used to maintain the invariant in a type
environment, the action of `expand-type` can be bound together with the action
of adding a type to an environment. Some functions that call `expand-type` can
be provided to replace `extend-tenv` and `extend-tenv-with-type`. Each call to
`extend-tenv` and `extend-tenv-with-type` can be replaced with them. Thus the
invariant can be maintained more robustly.

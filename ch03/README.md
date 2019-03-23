Notes on chapter 3
==================

# Exercise 3.42

Modify the lexical address translator and interpreter to use the trimmed
representation of procedures from exercise 3.26. For this, you will need to
translate the body of the procedure not `(extend-senv var senv)`, but in a new
static environment that tells exactly where each variable will be kept in the
trimmed representation.

- Extracting free variables of an expression isn't that hard; the hard part is
  how to tell `value-of` which variables need to be reserved after extraction
  from `translation-of`, one way is to record the original position of every
  free variable in `nameless-proc-exp`.

- When extracting free variables, don't forget to consider the case like this:

``` scheme
let x = 3
in let f = proc (y) proc (y) -(y,x)
   in ((f 13) x)
```

# Exercise 3.43

The translator can do more than just keep track of the names of variables. For
example, consider the program:

``` scheme
let x = 3
in let f = proc (y) -(y,x)
   in (f 13)
```

Here we can tell statically that at the procedure call, `f` will be bound to a
procedure whose body is `-(y, x)`, where `x` has the same value that it had at
the procedure creation site. Therefore we could avoid looking up `f` in the
environment entirely. Extend the translator to keep track of "known procedures"
and generate code that avoids an environment lookup at the call of such a
procedure.

- The key is to know what is a **known procedure**: if we can eliminate all the
  free variables from the body of a procedure, then that procedure can be put
  into where it is referred, without the necessity of taking a lengthy
  environment.

- Don't forget to consider the case of recursive function as in exercise 3.23
  and 3.25.

# Exercise 3.44

In the preceding example, the only use of `f` is as a known procedure. Therefore
the procedure built by the expression `proc (y) -(y,x)` is never used. Modify
the translator so that such a procedure is never constructed.

- With the completion of last exercise, this one is much easier. (Maybe this is
  the so-called `inlining`?)

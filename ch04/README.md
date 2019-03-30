Notes on chapter 4
==================

# Exercise 4.12

Our understanding of the store, as expressed in this interpreter, depends on the
meaning of effects in Scheme. In particular, it depends on us knowing *when*
these effects take place in a Scheme program. We can avoid this dependency by
writing an interpreter that more closely mimics the specification. In this
interpreter, `value-of` would return both a value and a store, just as in the
specification. A fragment of this interpreter appears in figure 4.6. We call
this a *store-passing interpreter*. Extend this interpreter to cover all of the
language EXPLICIT-REFS.

Every procedure that might modify the store returns not just its usual value but
also a new store. These are packaged in a data type called `answer`. Complete
this definition of `value-of`.

- If we don't rely on scheme procedures like `set!`, we must explicitly pass a
  store to `newref`, `deref` and `setref!`. Thus store becomes a data structure
  similar to environment. And we can use similar way for environment to process
  store. On page 116, in the implementation of `newref-exp`:

``` scheme
(deref-exp (exp1)
           (cases answer (value-of exp1 env store)
                  (an-answer (v1 new-store)
                             (let ((ref1 (expval->ref v1)))
                               (an-answer (deref ref1) new-store)))))
```

 `deref` does not explicitly specify which store it is going to use. I doubt if
 this is a misguide: shouldn't we tell `deref` which store to look up for a
 value? This
 [notes](https://cs.brown.edu/courses/cs173/2003/Textbook/2003-10-10.pdf) seems
 to support my guess, although it uses different jargon for `newref`(`newbox`),
 `deref`(`openbox`), and `setref`(`setbox`).

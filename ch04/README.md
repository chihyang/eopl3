Notes on chapter 4
==================

# Exercise 4.12
> Our understanding of the store, as expressed in this interpreter, depends on the
> meaning of effects in Scheme. In particular, it depends on us knowing *when*
> these effects take place in a Scheme program. We can avoid this dependency by
> writing an interpreter that more closely mimics the specification. In this
> interpreter, `value-of` would return both a value and a store, just as in the
> specification. A fragment of this interpreter appears in figure 4.6. We call
> this a *store-passing interpreter*. Extend this interpreter to cover all of the
> language EXPLICIT-REFS.
>
> Every procedure that might modify the store returns not just its usual value but
> also a new store. These are packaged in a data type called `answer`. Complete
> this definition of `value-of`.

If we don't rely on scheme procedures like `set!`, we must explicitly pass a
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

# Exercise 4.25
> Extend the block statement of the language of exercise 4.22 to allow variables
> to be initialized. In your solution, does the scope of a variable include the
> initializer for variables declared later in the same block statement?

# Exercise 4.26
> Extend the solution to the preceding exercise so that procedures declared in a
> single block aremutually recursive. Consider restricting the language so that
> the variable declarations in a block are followed by the procedure declarations.
 
# Exercise 4.27
> Extend the language of the preceding exercise to include *subroutines*. In our
> usage a subroutine is like a procedure, except that it does not return a value
> and its body is a statement, rather than an expression. Also, add subroutine
> calls as a new kind of statement and extend the syntax of blocks so that they
> may be used to declare both procedures and subroutines. How does this affect the
> denoted and expressed values? What happens if a procedure is referenced in a
> subroutine call, or vice versa?

In the beginning, I solved exercise 4.25, 4.26 and 4.27 separately. The result
is that I have to write test programs like below:
  
``` scheme
var x = 13;
proc add(x, y) +(x, y);
subroutine sub1() { x = -(x, 1) };
;; another statement
...
```

Procedure declarations follow normal value declarations. Subroutine declarations
follow procedure declarations. "Normal" variable declared latter can refer to
those declared earlier but not vice versa. Procedures can refer to all the
normal variables and are mutually recursive. Subroutines can refer to all the
procedures and normal variables. The result is a lengthy and bizarre program
with three different declaration rules and keywords. In some parts of the
interpreter, I have to make a function that requires a long list of arguments! I
guess if anyone really uses such a program, they would be mad at such stupid
language designer... ;( So why not make it easier? Why not make a unified
grammar for "normal" variables, procedures and subroutines? We only need to make
subroutine another kind of expression that contains a statement, as
[this](https://github.com/EFanZh/EOPL-Exercises/blob/master/solutions/exercise-4.27.rkt#L39)
does. We cannot call subroutine inside a procedure (why?), but everything else
is much more clear.

# Exercise 4.35

> We can get some of the benefits of call-by-reference without leaving
> the call-by-value framework. Extend the language IMPLICIT-REFS by adding a
> new expression
>
> Expression ::= ref Identifier<br/>
>                ref-exp (var)
>
> This differs fromthe language EXPLICIT-REFS, since references are only of variables.
> This allows us to write familiar programs such as swap within our call-by-value language.
> What should be the value of this expression?
>
> ``` scheme
> let a = 3
> in let b = 4
>    in let swap = proc (x) proc (y)
>                   let temp = deref(x)
>                   in begin
>                       setref(x,deref(y));
>                       setref(y,temp)
>                      end
>       in begin ((swap ref a) ref b); -(a,b) end
> ```
>
> Here we have used a version of let with multiple declarations (exercise 3.16). What
> are the expressed and denoted values of this language?

ExpVal = Int + Bool + Proc + Ref(ExpVal)
DenVal = Ref(ExpVal)

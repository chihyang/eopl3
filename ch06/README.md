Notes on chapter 6
==================

# Exercise 6.9
> What property of multiplication makes this program optimization
> possible?

If we replace `*` with the function below:

``` racket
(define mul
  (lambda (v1 v2)
    (* v1 v2)))
```

And trace the two versions of `fact`, we can find the succinct version first
evaluates `(* 3 1)`, then `(* 2 (* 3 1))`, then `(* 1 (* 2 (* 3 1)))`, while the
procedural version first evaluates `(* 1 1)`, then `(* 2 (* 1 1))`, then `(*3 (*
2 (* 1 1)))`. In fact, the evaluation order is completely reversed! This works because
multiplication has nor order. It is the same for addition.

# Section 6.3

Note how nested sum expressions are processed: a new call exp is generated for
each layer!

# Exercise 6.27

> As it stands, cps-of-let-exp will generate a useless let expression. (Why?)
> Modify this procedure so that the continuation variable is the same as the let
> variable. Then if `exp1` is nonsimple,
>
> ``` racket
> (cps-of-exp <<let var1 = exp1 in exp2>> K)
> = (cps-of-exp exp1 <<proc (var1) (cps-of-exp exp2 K)>>)
> ```

First see the case below:

``` racket
let x = (g m)
in if zero?(x) then 0 else 1
```

Here `x` is the de facto continuation variable, so we don't have to make another
one.

And: does this work if let has multiple bindings?

Besides, note the condition **nonsimple**. Try the case `let-scope-1` in
[cps-tests.scm](cps-tests.scm) to see what will happen and you would know what I
mean. Keep in mind that any transform shouldn't change the semantic that every
binding in let is in the same layer of environment.

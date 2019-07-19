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

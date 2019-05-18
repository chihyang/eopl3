Notes on chapter 5
==================

# Exercise 5.18
> The trampoline systemin figure 5.7 uses a procedural representation of a
> *Bounce*. Replace this by a data structure representation.

What is a *snapshot* for our interpreter? At any time, we either evaluate an
expression, apply or continuation, or get a *FinalAnswer*. So at any time, the
snapshot could be composed of one of the following:

* an expressed value
* an expression, an environment and a continuation
* a continuation and an expressed value
* a procedure, a list of expressed values, and a continuation

Hence our new data type *Bounce* has the following variants:

``` scheme
(define-datatype bounce bounce?
  (a-expval
   (val exp-val?))
  (a-value-of
   (exp expression?)
   (env environment?)
   (cont continuation?))
  (a-apply-cont
   (cont continuation?)
   (val exp-val?))
  (a-apply-procedure
   (proc1 proc?)
   (vals (list-of exp-val?))
   (cont continuation?)))
```

However, because we only make a non-`expval` bounce in `apply-procedure/k`, the
last variant above is not necessary here. In exercise 5.19, we would see the
usage of the last variant.

This note is inspired by
[this](https://github.com/chenyukang/eopl/blob/master/ch5/18.scm#L279) solution.

# Exercise 5.21

> Implement a trampolining interpreter in an ordinary procedural language. Use a
> data structure representation of the snapshots as in exercise 5.18, and
> replace the recursive call to *trampoline* in its own body by an ordinary
> *while* or other looping construct.

# Exercise 5.22

> One could also attempt to transcribe the environment-passing interpreters of
> chapter 3 in an ordinary procedural language. Such a transcription would fail
> in all but the simplest cases, for the same reasons as suggested above. Can
> the technique of trampolining be used in this situation as well?

Finally, finally, I saw the end of these two exercises... Whoops! Compared to
the dummy implementation in C, it's easy to see how much Scheme has done for
programmers (easy data structure definition, super-convenient lexer and parser,
garbage collection...). But for exercise 5.22, the interpreter does not fail as
the text above implies. In fact, if you try some big numbers, `value_of` version
would tolerate larger inputs than `value_of_k` for the test `double`
program. Why? Because in order to hold continuation arguments for the guest
*PROC* language, we need more stack space in the host language. As a result
`value_of_k` would exhaust C stack faster than `value_of`. However, we do not
have a way to avoid the growth of stack for `value_of` before `double`
returns. The non-tail-recursive `double` would always exhaust the stack space in
`value_of` at some point. On the other hand, in `value_of_k` we put continuation
into heap, it is possible to avoid (or at least, delay?) stack overflow if more
`value_of_bounce_t` is returned in `value_of_k` and `apply_cont`.

Here we can see that even C has done a little memory management: non-static
things on the stack is created and destroyed automatically. That's why they are
called automatic variables. Because these variables' accessibility is limited to
their scope (unless you refer to them by a returned pointer), their resources
can be freed at exit time. This is some kind of 'garbage collection'.

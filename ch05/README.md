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

# Exercise 5.32

> Improve your solution to the preceding exercise by minimizing the number of
> global registers used. You can get away with fewer than 5. You may use no data
> structures other than those already used by the interpreter.

Noticing that `exp` and `val` are never used at the same time, we can use the
same register for them. The only thing we need to note is the set order between
`cont`, `env` and `val`. Because `cont` and `env` are somewhat special (they
record the process in the whole computation), it's not easy to put them into the
same register. It is the same for trampoline register `pc`. As for procedure
register `proc1`, because it is used together with `val` and `pc` as below, it's
not easy to eliminate it. If we don't use trampoline, there are just four
registers; with trampoline, we need five:

``` scheme
(define apply-procedure/k
  (lambda ()
    (cases proc proc1
           (procedure
            (vars body saved-env)
            (set! pc #f)
            (set! env (extend-env* vars (map newref val) saved-env))
            (set! val body)
            (value-of/k)))))
```

# Exercise 5.33

> Translate the interpreter of this section into an imperative language.  Do
> this twice: once using zero-argument procedure calls in the host language, and
> once replacing each zero-argument procedure call by a `goto`. How do these
> alternatives perform as the computation gets longer?

Every time I thought my reference counting (RC) worked, I could find some more
tests to cause memory leak or corruption. The problem is that I can't properly
free environment used by a closure, especially for the procedure produced by
`letrec`. Here is the original idea: for `EXTEND_REC_ENV`, if RC reaches 2, the
whole environment and the procedure saved in it are freed (`2` is thought to be
the RC of the reference to the environment per se plus the reference by the
procedure saved in it). But I can find a situation where environment is not
referred to anymore while two or more values exist at the same time. In such
case the environment in the procedure cannot be freed too early. More cases
could be found to make the simple RC fail. So I gave up the endeavor: I use
value copy for every data types except continuation and ast. This is ineffective
but quite enough for small tests. As a comparison, I put similar tests in
exercise 5.21 but leave the interpreter unchanged. Run the program with
`valgrind` to see the problem.

# Exercise 5.40

> Give the exception handlers in the defined language the ability to either
> return or resume. Do this by passing the continuation fromthe `raise`
> exception as a second argument. This may require adding continuations as a new
> kind of expressed value. Devise suitable syntax for invoking a continuation on
> a value.

To be `recoverable-try` or `try ... catch (var, cont) ...`, which is better? As
one can see, in the first version, programmers can only resume the exception
after the exception handler is evaluated, while the second version gives
programmers ability to resume their work at any time they want. Sounds great?
Consider the program below:

``` scheme
let p = 3 in
  begin
     try
       let x = 3 in
         raise -(3, -(2, x))
     catch (y, cont)
       set p = cont;
     resume p with 3
  end
```

Does it terminate? Run it in exercise 5.40.v2.scm to see the result. Even
without running it, we can see the code above violates designer's purpose for
`try ... catch (var, cont)`. Unfortunately some users would definitely try to do
so if such a feature existed: they don't use `try` for exception handling but
for saving a continuation and use it later! A feature with unexpected feature
does not seem a good one.

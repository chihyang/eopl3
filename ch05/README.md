Notes on chapter 5
==================

# Exercise 5.18
> The trampoline systemin figure 5.7 uses a procedural representation of a
> *Bounce*. Replace this by a data structure representation.

What is a *snapshot* for our interpreter? At any time, we either evaluate an
expression, apply or continuation, or get a *FinalAnswer*. So at any time, the
continuation could be composed of one of the following:

* an expressed value
* an expression, an environment and a continuation
* a continuation and an expressed value

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
   (val exp-val?)))
```

However, because we only make a non-`expval` bounce in `apply-procedure/k`, the
last variant above is not necessary here. In exercise 5.19, we would see the
usage of the last variant.

This note is inspired by
[this](https://github.com/chenyukang/eopl/blob/master/ch5/18.scm#L279) solution.

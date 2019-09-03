Notes on chapter 7
==================

# Exercise 7.1

> Below is a list of closed expressions. Consider the value of each
> expression. For each value, what type or types does it have? Some of the
> values may have no type that is describable in our type language.

``` racket
6. proc (x) (x x)
```

It is not describable in our type language. Because its type is:


(t<sub>1</sub> -> t<sub>2</sub>), where t<sub>1</sub> = (t<sub>1</sub> -> t<sub>2</sub>)

There is no way to define a type that contains itself in our type language. So
it is quite possible that we cannot decide the type of a recursive
procedure. For the same reason, the type of Y-combinator in 13 might be
undecidable. But I am not sure for now.


``` racket
12. proc (x)
      proc (p)
        proc (f)
          if (p x) then -(x, 1) else (f p)
```

``` racket
(int -> ((int -> bool) -> (((int -> bool) -> int) -> int)))
```

# Exercise 7.2

> Are there any expressed values that have exactly two types according to
> definition 7.1.1?

Assume there are some expressed values that have exactly two types, then there
are at least two different types sharing a value, i.e., two type sets
intersect. According to definition 7.1.1:

> Definition 7.1.1 The property of an expressed value v being of type t is
> defined by induction on t:
>
> 1. An expressed value is of type `int` if and only if it is a `num-val`.
>
> 2. It is of type `bool` if and only if it is a `bool-val`.
>
> 3. It is of type (t<sub>1</sub> -> t<sub>2</sub>) if and only if it is a
> `proc-val` with the property that if it is given an argument of type
> t<sub>1</sub>, then one of the following things happens:
>
>     a. it returns a value of type t<sub>2</sub>
>
>     b. it fails to terminate
>
>     c. it fails with an error other than a type error.

It is easy to see that:

* 1 and 2 have no intersections
* 1 and 3 have no intersections
* 2 and 3 have no intersections

For 3, what is the type of the following program?

``` racket
letrec f(x) = (f x) in f
```

The recursive procedure above does not terminate, so we can pass a value of any
type, like `(f 1)` or `(f zero?(1))` or `(f (proc (x) x))`. Besides, since it
doesn't terminate, thus satisfying (b) in 3, t<sub>2</sub> can be any
value. Seems it's an expressed value that has more than two types.

# Exercise 7.3

> For the language LETREC, is it decidable whether an expressed value *val* is
> of type *t*?

At least for some *val*s, it is decidable.

1. For the non recursive part in LETREC, like the examples in this section 7.1.

2. For the recursive part, at least some expressed values have decidable
   types. For example:

   * the `fact` procedure has type (int -> int):

        ```racket
        letrec fact (n) = if zero?(n) then 1 else *(n, fact(-(n, 1))) in
        fact
        ```

3. But there exist undecidable expressed values as we have seen in examples in
   section 7.1 and exercise 7.1.

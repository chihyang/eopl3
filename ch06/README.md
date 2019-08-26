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

# Exercise 6.31

> Write a translator that takes the output of `cps-of-program` and produces an
> equivalent program in which all the continuations are represented by data
> structures, as in chapter 5. Represent data structures like those constructed
> using `define-datatype` as lists. Since our language does not have symbols,
> you can use an integer tag in the car position to distinguish the variants of
> a data type.

I asked a
[question](https://stackoverflow.com/questions/57441866/hints-about-exercise-6-31-of-eopl3)
on stackoverflow. But there are other problems not solved. Summer ends. Fall is
approaching. Months later I'll revisit this exercise and the following two.

# Exercise 6.34

> When we convert a program to CPS, we do more than produce a programin which
> the control contexts become explicit. We also choose the exact order in which
> the operations are done, and choose names for each intermediate result. The
> latter is called *sequentialization*. If we don’t care about obtaining
> iterative behavior, we can sequentialize a program by converting it to
> *A-normal form* or ANF. Here’s an example of a program in ANF.
>
> ``` racket
> (define fib/anf
>   (lambda (n)
>     (if (< n 2)
>         1
>         (let ((val1 (fib/anf (- n 1))))
>           (let ((val2 (fib/anf (- n 2))))
>             (+ val1 val2))))))
> ```
>
> Whereas a programin CPS sequentializes computation by passing continuations that
> name intermediate results, a program in ANF sequentializes computation by using
> let expressions that name all of the intermediate results.
>
> Retarget `cps-of-exp` so that it generates programs in ANF instead of
> CPS. (For conditional expressions occurring in nontail position, use the ideas
> in exercise 6.23.)  Then, show that applying the revised `cps-of-exp` to,
> e.g., the definition of `fib` yields the definition of `fib/anf`. Finally,
> show that given an input program which is already in ANF, your translator
> produces the same program except for the names of bound variables.

What is an *intermediate* result? I thought it was only `cps-call-exp`, but
turned out it made the program much more complicated if other `tf-exp` were not
allowed. If the condition is loosened, like
[this](https://course.ccs.neu.edu/cs4410/hw_boa_assignment.html):

``` ocaml
(* abstract syntax tree *)
type prim1 =
  | Add1
  | Sub1

type prim2 =
  | Plus
  | Minus
  | Times

type 'a bind =
  (* the third component is any information specifically about the identifier, like its position *)
  (string * 'a expr * 'a)

and 'a expr =
  | ELet of 'a bind list * 'a expr * 'a
  | EPrim1 of prim1 * 'a expr * 'a
  | EPrim2 of prim2 * 'a expr * 'a expr * 'a
  | EIf of 'a expr * 'a expr * 'a expr * 'a
  | ENumber of int * 'a
  | EId of string * 'a

(* check if an expression is in anf form *)
let rec is_anf (e : 'a expr) : bool =
  match e with
  | EPrim1(_, e, _) -> is_imm e
  | EPrim2(_, e1, e2, _) -> is_imm e1 && is_imm e2
  | ELet(binds, body, _) ->
     List.for_all (fun (_, e, _) -> is_anf e) binds
     && is_anf body
  | EIf(cond, thn, els, _) -> is_imm cond && is_anf thn && is_anf els
  | _ -> is_imm e
and is_imm e =
  match e with
  | ENumber _ -> true
  | EId _ -> true
  | _ -> false
;;
```

`anf-of-exp` is simpler than `cps-of-exp`. I don't even need to pass a
continuation!  Since the whole `anf-if-exp` can be put into a non-tail position
in this transformation, is it necessary to replace it with a variable? If not,
why ideas in exercise 6.23 must be used? This is another unanswered question for
now.

# Exercise 6.37

> Add implicit references (section 4.3) to CPS-IN. Use the same version of
> CPS-OUT, with explicit references, and make sure your translator inserts
> allocation and dereference where necessary. As a hint, recall that in the
> presence of implicit references, a `var-exp` is no longer simple, since it
> reads from the store.

The only problem for this exercise might be `letrec-exp`. Hints for it:

``` racket
(cps-of-exp <<letrec-exp p1(v1, v2, ...) = p-body1 ... in body>> k)
= (cps-of-exp
   <<let p1 = newref(165) ... in
       begin
         set p1 = proc (v1, v2, ...) p-body1;
         set p2 = proc (v1, v2, ...) p-body2;
         ...
         body
       end>>
   k)
```

It is worth noting that this is a little different from the implementation of
chapter 4. In chapter 4 `newref` is put into `apply-env`, thus each time a var
is searched, a reference for the bound procedure is generated. That is time
consuming, of course, but performance should not get in the way of understanding
a concept. In the conversion above, `apply-env` doesn't need to produce a new
procedure each time, and variables bound by `letrec` can be assigned to a value
of any expressed type. This is the same as in scheme.

# Exercise 6.38

> If a variable never appears on the left-hand side of a `set` expression, then
> it is immutable, and could be treated as simple. Extend your solution to the
> preceding exercise so that all such variables are treated as simple.

To check whether a variable is simple, we must know whether appears on the left
side of a `set` expression. To know whether a variable appears on the left side
of an expression, we must:

- Find all of its occurrences in current scope;
- Check if any of the occurrences is the left side of a `set`;
- Record the result of checking, and extend current "knowledge base", or say,
  environment.

Do you see it? We must first dive deeper into next layer of environment and then
swim back! For now, all bindings built by a procedure are treated as a
reference, because otherwise I have to search for the corresponding procedure
except for recording immutability. I don't know how to do it elegantly, so I
bypass this part. I guess I may need to traverse the AST more than once to
simplify a procedure call. But for now, let me have a rest and go for the
remaining chapters.

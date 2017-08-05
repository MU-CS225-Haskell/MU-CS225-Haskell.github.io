---
layout: page
title: Mathematical Foundations
permalink: /foundations/
visible: true
---

In order to learn about functional languages, we're going to need to start with
a little bit of mathematics.

### Mathematical Functions

Let's start by reminding ourselves of the definition of a mathematical function:
a function $$ f \colon A \to B $$ is a relationship between the elements of $$ A
$$ and the elements of $$ B $$, where, to each element of $$ A $$, we assign a
single element of $$ B $$. So if $$ A = \{ 1, 2, 3 \} $$ and $$ B = \{1, 2, 3\}
$$, we could have lots of different functions from $$ A $$ to $$ B $$; for
example,

$$
\begin{align*}
f(1) &= 1\\
f(2) &= 3\\
f(3) &= 3\\
\end{align*}
$$

is one possible function. This, however, is not a valid function:

$$
\begin{align*}
f(1) &= 1\\
f(1) &= 3\\
f(3) &= 3\\
\end{align*}
$$

since (1) $$ f $$ sends the same value to two different outputs and (2) $$ f $$
is not defined for the number $$ 2 $$, even though $$ 2 \in A $$. It would be a
bit insane to consider functions with these properties, since they would be very
difficult to reason about. For example, at no point while reasoning about, say,
the value $$ f(1) $$ can we replace $$ f(1) $$ with the value it returns, since
it could be either $$ 1 $$ or $$ 3 $$, and we can't determine which. This can
lead to some rather disturbing behaviour: $$ f(x) + f(x) \neq 2f(x) $$!

This property of being able to replace $$ f(x) $$ with the value it returns is
completely automatic for mathematical functions, so we never see this behaviour
in mathematics. We call functions with this property **referentially
transparent**.

What about 'functions' in a typical programming language, like C/C++/Java? Not
so. Consider this function:

```c
int t = 0;

int f(int x) {
  t += 1;
  return t + x;
}
```

This function will increment `t`, then return `t + x`, where `x` is passed in as
a parameter. Now ask yourself: is it true that $$ f(x) + f(x) = 2f(x) $$? No!
For example, `f(3) + f(3) = 4 + 5 = 9`, whereas `2*f(x) = 2*4 = 8`. The result
of the function changes depending on how many times it was called before! Worse
still, `f` may not be the only function to modify `t`, resulting in behaviour
that is potentially extremely unpredictable.

This example is particularly blatant, but there are endless examples of this
kind of behaviour.

If you already know how to program using something like Python, you might wonder
what the big deal about this is. This kind of behaviour, in many places, is
standard. But most would agree that *if* you could compute anything you liked
*without* resorting to writing functions that were not referentially
transparent, that would be far better, since the ability to apply equational
reasoning (i.e. replacing 'complex' things like $$ f(x) $$ with 'simple' things
like $$ y $$) to programs as we typically do in mathematics is of huge benefit.
In fact, applying equational reasoning is *exactly* how Haskell computes.

As such, Haskell has the following properties:

* All\* functions are referentially transparent.
* Everything is immutable, i.e. you cannot change the value of something.
* It is *declarative*: you just define what things are and Haskell figures out
  the best way to evaluate them.

\* There are a few exceptions, but they're always marked as 'unsafe'.

<!-- As it turns out, we can compute anything we want in this way, as Alonzo Church
discovered. His [$$ \lambda
$$-calculus](https://en.wikipedia.org/wiki/Lambda_calculus) is capable of
computing anything we currently know to be computable. Indeed, it is from the
lambda calculus that Haskell was born. Once it became known that this model of
computation was powerful enough, all there was to do was write a practical
programming language based on these ideas. -->

<!-- Actually, we can do better: we can compute anything we would like to compute
using *just one function*. Here it is:

$$
\iota(f) := (f(\iota(\iota(\iota(\iota(\iota))))))(\iota(\iota(\iota(\iota))))
$$ -->

### Haskell Syntax

It would be completely insane of me to repeat what so many have already
explained so well before, so learning basic Haskell syntax will be completely
deferred to other tutorials. We will essentially be covering everything in the
[sample pages](http://haskellbook.com/assets/img/sample.pdf) (PDF alert)
provided for the Haskell Book.

Once you have read and understood these chapters, you should well enough
understand basic syntax, some commonplace types and typeclasses, type aliases,
and type polymorphism. Not bad for too weeks work I think!

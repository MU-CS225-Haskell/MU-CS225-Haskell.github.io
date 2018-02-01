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
is not defined for the number $$ 2 $$, even though $$ 2 \in A $$. It would be
seriously masochistic to consider functions with these properties, since they
would be very difficult to reason about. For example, at no point while
reasoning about, say, the value $$ f(1) $$ can we replace $$ f(1) $$ with its
value, since that could either be $$ 1 $$ or $$ 3 $$, and we can't determine
which. This can lead to some rather disturbing behaviour: $$ f(x) + f(x) \neq
2f(x) $$!

This property of being able to replace the expression $$ f(x) $$ with its value
is completely automatic for mathematical functions, so we never see this
behaviour in mathematics. We call functions (more generally, *expressions*) with
this property **referentially transparent**. Note that a key property of
referentially transparent expressions is that we can reason about them without
evaluating them!

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
that is potentially extremely unpredictable. Any function in a language such as
C does not have type `f :: inputs -> output` as it may appear, it actually has
type `f :: program-state -> inputs -> output`. There is no way (in general) to
tell whether a function depends on the state of the program or not.

Haskell takes the approach of ensuring that the output of a function depends only
on the inputs; as such, GHC can reason about expressions before reducing them to
their values (in particular, it usually need only reduce them to weak head normal
form).

### Haskell Syntax

It would be insane of me to repeat what so many have already explained so well
before, so learning basic Haskell syntax will be completely deferred to other
tutorials. We will essentially be covering everything in the [sample
pages](http://haskellbook.com/assets/img/sample.pdf) (PDF alert) provided for
the Haskell Book.

Once you have read and understood these chapters, you should well enough
understand basic syntax, some commonplace types and typeclasses, type aliases,
and type polymorphism.

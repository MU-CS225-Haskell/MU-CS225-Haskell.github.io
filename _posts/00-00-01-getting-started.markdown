---
layout: post
title:  01 - Getting Started
date:   2016-07-29
---

Before we go anywhere, we need to make sure that you have the best possible
environment for using Haskell. As of recently, there really is only one
recommended way to build Haskell projects, and that's using **stack**. This is
by far the easiest way build any Haskell project, and is quickly becoming the
standard build tool for Haskell. I'll get into the minutae of stack in another
post, in particular when we actually start using libraries.

> **Side note 1**: There really isn't much point in installing the Haskell
Platform, stack can do everything for you. If you ever want to just compile a
single file, just run `stack ghc -- [file]` instead of just `ghc [file]`. This is
not advisable though, it's better to run `stack new [name-of-thing] simple` and
then do what you like in there; that way, you'll have access to external
libraries.

> **Side note 2**: It's worth getting familiar with the libraries that are in
the Haskell Platform since HackerRank only allows you to use these libraries.
CS210 features plenty of HackerRank style labs, so if you want to use Haskell
(and I certainly encourage you to), keep these libraries in mind.

<h3>Contents</h3> [//]:# (This ensures that the TOC doesn't include this header)

* TOC
{:toc}

## Installing Stack

Comfortable with the command line? Well, stack is a command line utility so
you're about to get comfortable! First download stack from
[here](https://docs.haskellstack.org/en/stable/README/) and follow the install
instructions (go to the detailed install instructions and click on your OS). On
Windows, you get an installer, on Unix you just have to run a few commands
(which you should probably be comfortable with if you're running Unix, certainly
Linux).

Once that's done, we should check everything installed correctly. Open the
command prompt (Windows) or terminal (Unix) and type

```bash
$ stack --version
```

Assuming that worked, you're done. Otherwise, you'll need put the stack
executables on your `PATH` environment variable.

For those unfamiliar:

- Environment variables are just aliases for certain other
commands/directories/whatever.
- the `PATH` variable just tells the command prompt where to look for certain
executables so they can be easily referenced.

So for example, you may have a `JAVA_HOME` environment variable, which is
actually just an alias for wherever your JDK is installed. If this is properly
set, it means that any program that needs to reference this directory can do so
without knowing anything about your computer. If I write an app that needs to
reference the binaries in the JDK, I can now just write `%JAVA_HOME%\bin\*`, and
no matter where your JDK is, that name will properly resolve. You may also have
a `NUMBER_OF_PROCESSORS` variable, which (confusingly) refers to the number of
*cores* your computer has.

We won't get into starting a project yet, although it may be worth testing
whether stack works or not by running `stack new my-app simple` in the folder
where you want to keep your projects. This will create a project called `my-app`
(feel free not to call it that by the way, it's your computer). We'll be
discussing the structure of this project in due time; for the time being, we'll
be living in the `src` directory. Enter `cd my-app` to move into the directory,
then run `stack build` and hope nothing explodes :)

But if something does explode, just ask me about it and I'll see what I can do
to fix it. After that, you can run `stack exec my-app` to greet the world.

## Installing a text editor

If you've been programming for a while, you probably already have a favorite
text editor, so I'm not going to try to convince you to use anything else.
However, for Haskell development (and indeed development in general), I would
recommend using [Atom](https://atom.io/) or [Vim](http://www.vim.org/) if you're
on Linux (talk to me if you want to know how to set up Vim for Haskell
development, been a while since I've used it but I should be at least *some*
help).

At this stage, I think I can safely say that I don't think you should use an IDE
(integrated development environment) unless you're doing development that, for
all practical purposes, requires one (like Android app development). At best,
they hide the details of the build process from you, at worst they can give you
a false impression as to how the build process actually works.

Text editors have recently become extremely comprehensive, filling a gap
somewhere between a traditional text editor and an IDE, but with the benefit
that they're language agnostic. In constrast, a lot of IDEs are language
specific, which means writing in a different language often requires a change in
IDE.

## The REPL

In the stack project you created, run `stack ghci`. This will start the Glasgow
Haskell Compiler Interpreter in your project environment. You're going to spend
plenty of time in here testing out things. Have a look at the
[syntax specification]({{ site.url }}/syntax/) and
[this chapter](http://learnyouahaskell.com/starting-out) of Learn You a Haskell.
LYAH assumes you're not using stack, but it's basically the same thing. Edit
the file `src/Main.hs` if you want to add some functions, then reload the
interpreter with `:r`. If he says run `ghci`, run `stack ghci` instead.

I won't run you through everything you need to know here, I'll just randomly
point out a few things that I think are important to note, and I'll leave it to
[this](http://www.cis.upenn.edu/~cis194/spring13/lectures/01-intro.html) and
[this](http://learnyouahaskell.com/starting-out) to be the real introductions.

First thing to know about Haskell is that function application (the act of
calling a function on a value) is denoted by *whitespace*.

```haskell
GHCi> sin(10)         -- Valid but horrible, never do this
GHCi> sin 10          -- Good
GHCi> logBase(10,14)  -- Doesn't compile, (10,14) is a tuple, not two arguments
GHCi> logBase 10 14   -- Good
```

There are very good reasons for this, which we'll see later in the section on
[currying](#functions-cant-take-more-than-one-argument).

Another language agnostic thing: floating point numbers often don't make any
sense to equate, so be careful of doing things like this:

```haskell
GHCi> 0.1 + 0.2 == 0.3
False
GHCi> sin pi == 0
False
```

As a quick fix, we could, if we wanted, define an 'approximately equal to' test.

```haskell
let a ≈ b = (a - b) < 0.00001
```

Yep, not only can you define your own operators, you can use Unicode
symbols/punctuation too! Go [here](http://stackoverflow.com/a/10548541/4723912)
to see a list of all the symbols you're allowed to use to define your own
operators.

One function we'll use a lot is the function application operator, `$`. This
allows us to transform this:

```haskell
sin (cos (recip (logBase 4 (exp 7))))
```

into this:

```haskell
sin $ cos $ recip $ logBase 4 $ exp 7
```

Yay, no more brackets! So how does it work? Nope, it's not some special syntax,
it's just a regular function. The definition of `$` is this:

```haskell
f $ x = f x
```

Hmm, doesn't really look like it does anything, it just takes a function and a
value, and then applies that function to the value. The magic is in this
functions *fixity* and *precedence*. In particular, its fixity is `infixr 0`.
`infixr` means that it obeys right associativity, e.g. `f $ g $ h $ x` becomes
`f $ (g $ (h $ x))`, and the `0` means it has the lowest precedence possible,
which, in practice, means that `$` is always the last function to evaluate its
arguments, meaning that we don't end up getting complete rubbish like
`sin $ 2*3` evaluating to `(sin $ 2) * 3`.

There's a lot more you can practice but that's something you need to do on your
own.

## Anonymous Functions

Sometimes we want to define a function without actually giving it a name. This
is great for when we just want to do something specific once. For example:

```haskell
GHCi> map (\x -> x + 1) [1,2,3]
[2,3,4]
```

So use `\` to start an anonymous function, then supply the arguments (separated
by whitespace) directly afterwards, then `->`, then the definition.

All regular functions can be defined in terms of anonymous functions, for
example:

```haskell
f x y = (x - y) * (y - x)
```

could be rewritten as:

```haskell
f = \x y -> (x - y) * (y - x)
```

According to the compiler, this is identical to the first in every way, but the
first way is clearer.

## Laziness

Haskell is a *lazily-evaluated* language, meaning that (in general) results
aren't computed until you actually need them. This allows you to write code like
this:

```haskell
print $ take 17 $ processListInSomeWay $ [1..]
```

Even though `[1..]` is an infinite list (try just running `[1..]` in GHCi, press
Ctrl-C when you get bored), it won't be evaluated until/unless we need it. When
we call `print`, we force the program to evaluate the rest of the code, but
since we make a call to `take 17`, the compiler knows that whatever list it
takes as an argument, it only needs the first 17 of those values. Therefore,
the infinite list will actually only have the first 17 of its elements
evaluated, and subsequently, the process in the middle only has to apply that
process to that many elements (unless of course that process requires the entire
list, in which case you'll be waiting a while (see eternity)).

There are some weird edge cases when it comes to pattern matching though (see
[here]({{ site.url }}/syntax/#pattern-matching) for a refresher on pattern matching).

For example:

```haskell
takeAlternating = foldr (\a (xs,ys) -> (a:ys, xs)) ([],[])
```

Without thinking too much about how this works (maybe come back to it later
after you've finished reading this whole post), this will take alternating
elements of a list and return them as a 2-tuple of lists, e.g.

```haskell
takeAlternating [1,2,3,4,5,6] == ([1,3,5], [2,4,6])
```

This is fine, but try it on an infinite list:

```haskell
import Control.Arrow ((***))
(take 5 *** take 5) $ takeAlternating [1..]
```

We'd like to get `([1,3,5,7,9],[2,4,6,8,10])`, but this never finishes running.
This is because pattern matching in Haskell is actually strict (*surprise!*).

This makes sense if you think about it, the pattern needs to be evaluated to
a certain degree so that the compiler can check if it matches or not, otherwise
it would be inadvertently matching everything. Since we have to evaluate every
tuple to check if it matches, this forces the whole list to be processed.

Lucky for us there's such thing as lazy pattern matching:

```haskell
takeAlternating = foldr (\a ~(xs,ys) -> (a:ys, xs)) ([],[])
```

This will tell the compiler to basically assume it matched and then check later
whether it actually matched when the result is needed. It's fine here because
the pattern cannot possibly *not* match, but in other cases this is a really bad
idea. For example:

```haskell
-- This is fine because Haskell will make sure the first pattern actually
-- matches before attempting to evaluate it
f (x:xs) = x:xs
f    []  = []

-- This is not, since Haskell will not make sure it matches first, it will
-- just assume that it has, then check later (when it's too late).
f ~(x:xs) = x:xs
f     []  = []

-- In fact you'll get this warning:
<interactive>:5:5: Warning:
    Pattern match(es) are overlapped
    In an equation for `f`: f [] = ...
```

Essentially, `f ~(x:xs) = x:xs` translates to `f xs = head xs : tail xs`, in
which case it's easy to see why any list will match. It's also easy to see where
this can go wrong: if the list is empty, `head` will throw an error, not what we
want at all.

## Quick Note on Type Signatures

In Haskell, everything has a *type*. A type is, fundamentally, just a set of
objects. A value of type $$ t $$ is just an element of that set.

E.g. if I say 1 has type `Int` (or, in Haskell parlance, `1 :: Int`), you can
think of this as saying that this '1' belongs to the set of all `Int`s. There
are plenty of other types like `Bool`, `Double`, `[]`, etc. (I'd suggest looking
at [this](http://learnyouahaskell.com/types-and-typeclasses#believe-the-type)
section of Learn You a Haskell if you want a quick rundown of the most common
basic types).

Functions also have types. Think of these like you would think of the 'type' of
a mathematical function. The following function can be said to belong to the
set of functions from the reals to the reals.

$$
\begin{align*}
f : \mathbb{R} \to \mathbb{R}\\
x \longmapsto 3x
\end{align*}
$$

We know this to be a function with domain and range both being $$ \mathbb{R} $$,
which takes some $$ x \in \mathbb{R} $$ and returns $$ 3x \in \mathbb{R} $$.
Easy. The first line can be thought of as a ballpark description of what the
function does from a very high level: it takes a real number as input, and spits
a real number back out.

Haskell has these descriptions of functions too in the form of their *type
signatures*. For example, let's inspect the type of the most simple function
imaginable, the identity function, which takes a value and returns that same
value unchanged:

```haskell
GHCi> id True
True

GHCi> id 9
9

GHCi> :t id
id :: a -> a
```

This says that `id` takes a value of type `a` and returns a value of type `a`.
What's `a` though? This is called a *type variable*, which means that it can be
any type at all. We'll discuss these in more detail in the typeclasses section.

Consider this function:

```haskell
GHCi> :t (+)
(+) :: Num a => a -> a -> a
```

This says that `+` is a function that takes two values of type `a` and returns
a value of type `a`, as long as `a` is a `Num`bery type. Again, we'll discuss
these more in the typeclasses section.

This is all I want to say about types for now, we'll get into a more detailed
discussion about data types, typeclasses and constraints in the next post.

## Functions can't take more than one argument

Wait, what?

OK, clearly we've seen functions that take more than one operand, so what's
going on?

What actually happens is that a function that takes $$ n $$ arguments is really
a function that takes *one* argument, then returns another function that takes
$$ n - 1 $$ arguments.

So instead of this:

```haskell
map :: (a -> b) -> [a] -> [b]
```

We can think of it like this:

```haskell
map :: (a -> b) -> ([a] -> [b])
```

So `map` can now be thought of as a function that takes a function from `a` to
`b` and returns another function, this time from `[a]` to `[b]`. This new
function can then be applied to `[a]` to return `[b]`.

In this sense, `map` is a function that *promotes* another function into working
on lists. For example, puny `succ` couldn't possibly increment *every* element
in a list, but if we use `map succ`, suddenly we have a new function that can.

> Note: This idea of promoting (or <i>lift</i>ing) functions into some new
context (the context in this case being lists) is an idea that will come up a
lot later.

This is why the return type of a function and the argument types are not clearly
distinguished, because the return type of a function can 'change' depending on
how many arguments you supply to it.

This is also why we can have functions like `(+1)` and `(*10)`. By simply not
suppling 'enough arguments', we get a new function instead of a value. This
transformation of multiple argument functions into a sequence of functions that
all take one argument is called **currying**.

This is the reason we can define functions without having to explicitly state
all of the arguments they take. E.g. I can define:

```haskell
func xs = init $ takeWhile even $ drop 10 xs
```

This is the way it's typically done in Leaving Cert level mathematics, we define
the function in terms of the arguments they take. A better way to define a
function (if possible) is as a composition of other functions:

```haskell
func = init . takeWhile even . drop 10
```

This is called *pointfree style*. It's usually considered good practice to write
functions in pointfree style if possible (athough this has been hotly debated),
just be careful, as functions quickly become unreadable if a pointfree style is
forced on them unnaturally:

```haskell
-- Easy to understand
f x = x^2 + 2*x + 1

-- Not as easy, requires knowledge of the Applicative instance for functions
f = (+1) . ((+) <$> (^2) <*> (*2))

-- Easy to understand
g x = x^2

-- Not as easy, requires knowledge of the Monad instance for functions
g = join (*)
```

Use your best judgement. Code is more often read than written, so make sure your
code is readable. With experience, you'll understand why the second versions of
`f` and `g` work, but the issue is that they're needlessly complex given what
they do.

Understanding currying is critical to understanding functions such as this
reimplementation of the `filter` function:

```haskell
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x -> if p x then (x:) else id) []
```

It's a good exercise to try to figure out how this works. Remember that even
though the function `\x -> if p x then (x:) else id` *looks* like it only
accepts one argument, it actually accepts *two*.

## Common Pitfalls

There are quite a few pitfalls that can catch you out early while learning
Haskell. I'd rather clear as much of them up straight away so we can get onto
the really useful language features.

### Whitespace

Haskell is a whitespace delimited language (like Python and unlike Java), so
indentation matters! First and foremost, **use spaces only**. Do **NOT** use
tabs under any circumstances, *ever*. There's lots of good reasons for this,
you can look them up.

In most, if not all, text editors, you can set it so that all tabs that would
have been inserted are now spaces. Turn this on in every text editor you use,
including IDEs.

As a side note, I recommend that you use either 2 or 4 spaces as your tab width.
Never use 3 or 8 or any other number. I personally prefer 2 spaces to 4, but
feel free to disagree with that, as long as the code you write is consistent
with yourself or the existing code base.

### Providing Explicit Types

Be careful when deciding to manually constrain types, e.g.

```haskell
[1,2,3] :: [Double]
```

is fine, but

```haskell
map floor [1,2,3] :: [Double]
```

is not. Instead, try

```haskell
map floor ([1,2,3] :: [Double])
```

or

```haskell
map floor ([1,2,3 :: Double])
```

The first one doesn't work because you're trying to assign the type `[Double]`
to the *entire* expression '`map floor [1,2,3]`' (which actually has type
`[Int]`).

The middle one works because we're now only assigning the type `[Double]` to
the list literal `[1,2,3]`.

The last one works because a list has to contain elements of the same type, so
forcing 3 to be an `Double` forces the rest of them to be `Double`s too.

### Type Constraints

Always remember that Haskell is a statically typed language. Put bluntly, if
your types don't match up, your program will *not* compile, simple as that. The
only way to mitigate this is to always be aware of the type signatures of the
functions you're using.

For example, will this work?

```haskell
GHCi> let avg xs = sum xs / length xs
```

At first glance, it may seem like it should. But it *doesn't*. Why? Let's
inspect their types, specifically their return types (which I've surrounded with
asterisks):

```haskell
GHCi> :t sum
sum :: (Num a, Foldable t) => t a -> *a*

GHCi> :t length
length :: Foldable t => t a -> *Int*
```

> Note: Ignore the Foldable constraint. That just means that `length` works on
all Foldable types, not just lists. For now, imagine `t a` as being `[a]`, it's
the only Foldable type we'll really care about for now.

The first returns a value of type `a` (as long as `a` is a number), the second
returns specifically an `Int`. Nothing wrong so far. Now let's inspect the type
of `/`:

```haskell
GHCi> :t (/)
(/) :: Fractional a => a -> a -> a
```

Ah, `Int` is *not* a fractional type, it's an integral type, so we can't use `/`
on the return type of `length`.

Before we get onto why this is, let's try and fix our average function.

Firstly, we need to make the length function more generic. Since integers are
a subset of the real numbers, we can call `fromIntegral` on the result of
length to generalise it to any number.

```haskell
GHCi> let avg xs = sum xs / fromIntegral (length xs)
GHCi> :t avg
avg :: (Fractional a, Foldable t) => t a -> a
```

This compiles! But the type is still rather restrictive, we'd really like this
to work on any list of numbers. While this is impossible for just `Num` instances
(for reasons that are beyond our current scope), we can generalise it to all
`Real` numbers, i.e. `Int`, `Integer`, `Float`, `Double`, and `Word`.

```haskell
GHCi> let avg xs = realToFrac (sum xs) / fromIntegral (length xs)
GHCi> :t avg
avg :: (Fractional a, Real b, Foldable t) => t b -> a
```

Right, now this takes a foldable container of real numbers and returns a
fractional number. Perfect!

There are reasons why the length function does not return a generic `Num` type.
The first is efficiency: supposedly `length` is faster than using a generic
length like `genericLength`. The second argument is appealing to pedagogy: it's
easier when teaching people Haskell at first to use concrete types instead of
generic types.

The efficiency argument holds little value (I've run benchmarks which show that
there's basically no difference between `length` and `fromIntegral . length`).
The pedagogical argument is also moot, because since GHC 7.10 (or thereabouts),
a lot of functions have migrated from working on just lists to working on either
any foldable type or any traversable type, so clearly pedagogy is not on the
agenda here. They're more interested in creating more functionality, which is a
better idea in my opinion.

While generic types are great from a functionality perspective (length makes
sense on way more types than just lists, so why define it only on lists?), it's
often hard to explain to beginners what all the constraints are for. I basically
have to say 'ignore that for now, we'll come back to it later', but when we do,
you'll appreciate why it's necessary.

### 'Crazy' Compile Errors

Say we want a function that filters a list on even numbers, takes the first
10 elements of that list, and then sums them up. We can write this using
Haskell's function composition operator, `.`, which behaves exactly like
function composition in mathematics. That is to say, '$$ f \circ g $$' in
mathematics is the same as `f . g` in Haskell:

```haskell
GHCi> let f = sum . take 10 . filter even
```

Cool, that compiles. What would have happened if we left out the parameter for
`take`, so `f = sum . take . filter even`?

```
<interactive>:1:22: error: ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
    • Couldn't match type ‘[a1]’ with ‘Int’
      Expected type: [a1] -> Int
        Actual type: [a1] -> [a1]
    • In the second argument of ‘(.)’, namely ‘filter even’        ←
      In the second argument of ‘(.)’, namely ‘take . filter even’ ←
      In the expression: sum . take . filter even
    • Relevant bindings include
        f :: [a1] -> [a] (bound at <interactive>:1:5)
```

This looks scary, but actually error messages in Haskell are very helpful!

OK, let's decipher this. The first line says that it couldn't match up some
types, it expected a list but got an integer. It then points to the offending
expression. From this, you should be able to spot the issue, namely that `take`
is not applied to enough arguments.

Sometimes the compiler can be a lot more helpful in working out the perpetrator,
for example:

```haskell
GHCi> let f = head . take . filter even
```

... gives the much more useful error message:

```
<interactive>:9:16:
    Couldn't match type ‘[a0] -> [a0]’ with ‘[c]’
    Expected type: Int -> [c]
      Actual type: Int -> [a0] -> [a0]
    Relevant bindings include
      f :: [a] -> c (bound at <interactive>:9:5)
    Probable cause: ‘take’ is applied to too few arguments  <---- yay!
    In the first argument of ‘(.)’, namely ‘take’
    In the second argument of ‘(.)’, namely ‘take . filter even’

...
```

To make sure your error messages are always good, be sure to always explicitly
declare the type signature for your top level functions.

## Parse error on import ... ?

Make sure you don't leave out the `where` in:

```haskell
module SomeModule where
```

Otherwise you'll never realise what the issue is, you'll just get parse errors
on everything that comes after the module declaration (which is usually
imports). Atom makes very sure you don't do this by highlighting everything red.

## Recursion

In Haskell, recursion is used a *lot*. A value is said to be defined recursively
when that value, under at least one condition, is defined in terms of some
expression containing that same value. Put simply, a recursive value is defined
in terms of itself, at least in part.

For example:

$$
\begin{equation}
x = x^2 - 1
\end{equation}
$$

This is a recursive definition. The definition of $$ x $$ is just whatever
$$ x $$ happens to be, squared, with one subtracted from it.

Another way to think about this is that we're looking for a value such that if
we square it and take away one, we get back the value we started with.

Of course, we don't think of this as being recursive, this is just a quadratic
equation. We can solve these! Assuming you're not too rusty, you should get:

$$
x = \frac{1 \pm \sqrt{5}}{2}
$$

With the sign being positive, this is the golden ratio, or $$ \varphi $$ (phi).

You can check that both values for $$ x $$ satisfy the property that squaring
them and subtracting one 'does nothing', i.e. the function $$ f(x) = x^2 - 1 $$
'does nothing' to $$ \varphi $$. Any time this happens, where applying a
function to a value 'does nothing' to that value, we call that value a *fixed
point* of that function. Notationally, when $$ f(x) = x $$, we call $$ x $$ a
fixed point of $$ f $$. Fixed points play a critical role in defining recursion
in terms of functions (which is necessary for Haskell since it's a functional
language), but we won't get into that today.

Another recursive definition you've probably already had described to you (or
that you've described to others) is the definition of the Fibonacci sequence.
Assuming $$ T_0 = 0 $$ and $$ T_1 = 1 $$:

$$
T_n = T_{n - 1} + T_{n - 2}, \forall n \geq 2
$$

Simple, each term past the first two in the sequence is the sum of the two that
came before it. This can be described in Haskell as follows:

```haskell
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

... which is suspiciously close to the definition.

One last recursive function you definitely know is the factorial function,
denoted in mathematics by a postfix exclamation mark. Here's the recursive
definition:

$$
\begin{align*}
0! &= 1\\
n! &= n \cdot (n - 1)!
\end{align*}
$$

OK, so for something like $$ 4! $$ we can use this defintion to work out what it
should be:

$$
\begin{align*}
4! &= 4 \cdot 3!\\
&= 4 \cdot 3 \cdot 2!\\
&= 4 \cdot 3 \cdot 2 \cdot 1!\\
&= 4 \cdot 3 \cdot 2 \cdot 1 \cdot 0!\\
&= 4 \cdot 3 \cdot 2 \cdot 1 \cdot 1\\
&= 24
\end{align*}
$$

Easy! Again, the definition in Haskell is very close to the original:

```haskell
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

Unfortuately, standard Haskell doesn't allow you to define postfix operators,
like the way the factorial operator is usually written, but thankfully, Haskell
has a language extension that allows you to use unary operators in postfix
style.

```haskell
{-# LANGUAGE PostfixOperators #-}

(!) 0 = 1
(!) n = n * ((n - 1)!)

-- Now we can do: print (10!)  -->  3628800
```

The only downside to this is that, in order for any code with postfix operators
to compile, you need to surround the code containing that operator in
parentheses, e.g. `10!` won't compile, but `(10!)` will.

### Encoding Loops

In Haskell, we encode repetition with recursion. Consider the definition of the
function `map`:

```haskell
map :: (a -> b) -> [a] -> [b]
map _    []  = []
map f (x:xs) = f x : map f xs
```

`map` just applies a function to each element in the list. To do that, it must
apply the function to the first element, then call itself on the rest of the
list. If it's called on the empty list, it returns the empty list.

OK, how about summing a list?

```haskell
sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs
```

Flattening a list of strings?

```haskell
concat :: [String] -> String
concat []     = []
concat (x:xs) = x ++ concat xs
```

Reversing a list?

```haskell
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
```

Hmm, all these functions look very similar. Indeed, it's rare in Haskell to have
to make recursion as explicit as in these examples, general recursive ideas are
normally abstracted into functions (see `foldl` and `foldr`) so you don't have
to write out all that code over and over.

```haskell
sum     = foldl (+) 0
concat  = foldl (++) []
reverse = foldl (flip (:)) []
```

In general you shouldn't be thinking about 'looping' over anything in a
functional language, but if you're coming from imperative languages it's useful
(at first) to know what the 'equivalent' is.

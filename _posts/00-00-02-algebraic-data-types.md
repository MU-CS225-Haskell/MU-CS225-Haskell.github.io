---
layout: post
title: 02 - Algebraic Data Types
date: 2016-07-29
---

<h3>Contents</h3> [//]:# (This ensures that the TOC doesn't include this header)

* TOC
{:toc}

> NOTE: The [CIS 194](http://www.cis.upenn.edu/~cis194/spring13/) course
material covers algebraic data types really well, so instead of just repeating
what they've already said, I decided to take a more mathematical approach to
algebraic data types. Hopefully you find it interesting/enlightening.

## Overview

Fundamentally, any program usually consists of only two things: data types, and
functions that operate on those data types. Haskell in particular uses
*algebraic data types*, which are, as far as I'm concerned, the simplest
possible way to express data in a program. Those who have programmed in
object-oriented languages will be used to classes and objects as a way of both
creating types and implementing functionality on those types. In C and Rust, you
would use a `struct`.

It's helpful to know what the term 'algebraic' refers to when defining what an
algebraic data type is. In mathematics, any algebraic structure is just a set
with one or more operations defined on the elements of that set, with the
restriction that these operations may only take a finite number of elements as
arguments.

The algebraic structure in question here is the algebra of *types*. In
particular, we're mostly interested in how these types can be combined using
certain operations to form new types. These combined types are precisely the
definition of an algebraic data type.

It's worth noting that we will *not* be using ADT as an acronym, since that's
usually reserved for
[*abstract* data types](https://en.wikipedia.org/wiki/Abstract_data_type), an
entirely different concept.

There are two major classes of algebraic data types: *sum* types and *product*
types, with 'sum' and 'product' being the two main operations we can apply to
types. These behave (as it turns out) almost identically to the operators
'$$ + $$' and '$$ \times $$', but only in an abstract sense.

## Sum Types

'Sum types' arise when you want your data to act like a *choice* between
multiple different values. Here of some examples of sum types.

```haskell
data Boolean = False | True

data Day =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

data CardValue =
    Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King

data CardSuit =
    Spades
  | Clubs
  | Diamonds
  | Hearts
```

The number of values that can be held by each of these types is just the number
of possible values the type can have (excluding $$ \bot $$, which is a
[party pooper](https://wiki.haskell.org/Bottom)).

These types are usually called **enumerable types**, or just **enums**.

They're extremely easy to pattern match on as well:

```haskell
isRedSuit :: CardSuit -> Bool
isRedSuit Hearts   = True
isRedSuit Diamonds = True
isRedSuit _        = False
```

We'll be able to see sum and product types in their full glory later on, enums
on their own are often too restrictive for most use cases. See the CIS notes for
more examples.

Those trying to reconcile the definition of algebraic data types with enums may
have a hard time seeing how they are a sum of *types*. Surely they're a sum of
*values*? Well, from the perspective of type theory, something like `Spade` is
actually a *type constructor* (more on these later) that takes no type arguments
and constructs the single value `Spade`. This is similar to defining
`data Spade = Spade`. Thus it's called a *singleton* type. Any enum can then be
considered a *tagged union* or *sum* of singleton types.

## Product Types

Product types can be considered a Cartesian product of types. Easiest way to
show this is to use the most common Cartesian product, $$ \mathbb{R}^2 $$, or
$$ \mathbb{R} \times \mathbb{R} $$, which defines the set of all points in 2D
Euclidean space.

```haskell
data Point2D = P2D Double Double
```

Here, a `Point2D` is defined to be a new type which is a product of two
`Double`s.

With product types, the number of distinct values that can be
represented by the type is the product of the number of values each component
has (hence the name). Clearly for this type it is uncountably infinite, but say
for example we had:

```haskell
data Card = Card CardValue CardSuit
```

The number of possible values the `Card` type can have is just the product of
the number of values each component type can have (which in this case is
$$ 13 \times 4 = 52 $$).

Again, we can also pattern match on these types.

```haskell
isRedFace :: Card -> Bool
isRedFace (Card King Hearts)  = True
isRedFace (Card Queen Hearts) = True
isRedFace (Card Jack Hearts) = True
...
```

Wait a second, this is a bit ridiculous. It would be much easier to define
'nobility' to be the set of all Jacks, Queens, Kings, regardless of their color,
then combine that with our `isRedSuit` function. We could just list them out
like we were doing above, but we'd like a more elegant solution. We'll see how
we can easily implement functionality on our types in the next post.

### Record Syntax

Sometimes with product types we'd like to be able to name specific components of
the type for clarity. For example:

```haskell
-- The 'Day' type here is not the one defined above, it's in Data.Time.Calendar
data Person = Person String Int Day String String
```

We could spend 5 mins guessing what these types are meant to actually represent,
or we could slap whoever wrote this code and tell them to do this instead:

```haskell
data Person = Person
  { name    :: String
  , age     :: Int
  , dob     :: Day
  , address :: String
  , bio     :: String
  }
```

Much better! This type is sematically identical to the one above, but is a lot
easier to read. These named fields also act as accessor functions, i.e.
`name` is actually a function that takes a `Person` an returns a string: their
name.

It's worth noting that *type synonyms* can help here too (if you don't like
record syntax for some reason):

```haskell
type Name    = String
type Age     = Int
type DoB     = Day
type Address = String
type Bio     = String

data Person = Person Name Age DoB Address Bio
```

Now anywhere you reference, for example, `Address`, you will actually be
referring to a String. It just helps document your code a bit better.

Type synonyms are great, but not in this case; record types are a much better
choice here.

Going back to this data type:

```haskell
data Card = Card CardValue CardSuit
```

If we represent this instead as a record type:

```haskell
data Card = Card
  { value :: CardValue
  , suit  :: CardSuit
  }
```

We can now write:

```haskell
isRedFace :: Card -> Bool
isRedFace c = isRed (suit c) && isNoble (value c)

isRed :: CardSuit -> Bool
isRed Diamonds = True
isRed Hearts   = True
isRed _        = False

isNoble :: CardValue -> Bool
isNoble Jack   = True
isNoble Queen  = True
isNoble King   = True
isNoble _      = False
```

This is better, but not much better. In the next section we'll come back to
this and fix it with our newfound knowledge of typeclasses.

## `data` vs. `newtype`

There's one way of declaring data types I haven't discussed yet: using the
`newtype` keyword. Although they're both used to define data types, newtypes
are more limited; they can only declare data types with exactly one data
constructor and exactly one field inside that data constructor.

```haskell
-- Allowed
newtype Kilogram = Kg Double

-- Not allowed, too many constructors
newtype Point = P Double Double
```

Here, `Kg` is the data constructor, since it takes a value of type `Double` and
'constructs' a concrete value, e.g. `Kg 4.0 :: Kilogram`.

The main difference between `data` and `newtype` is that `newtype` is usually
used to enforce type restrictions on an existing type without changing the
runtime representation of that type. Here, `Double` is the existing type, but
we want the compiler to think of it as `Kilogram`. Since type checking is done
at compile time, once the compiler is happy that all the types match up, it can
safely remove all type annotations, leaving us with `Double`. Therefore, any
function with type `Kilogram -> Kilogram` is, at runtime, identical to
`Double -> Double`.

As a result of this, newtypes are considered a *zero-cost abstraction*, as they
incur no runtime cost, but have all the benefits of the type system.

## Type Constructors

Here's a fun idea. What if you could have a *type* function that takes a types
as arguments and returns a single, new type? Well with Haskell, not only is this
possible, but you'll use them a lot!

E.g., consider the following type:

```haskell
newtype Stack a = Stack [a]
```

This is a data type that represents a stack
[ADT](https://en.wikipedia.org/wiki/Abstract_data_type). Notice that `Stack`
(the type) is a type constructor in the sense that you can have `Stack Int`,
`Stack Bool`, `Stack (Stack (Stack String))`, etc., i.e. you can pass `Stack` a
type as a parameter and it will construct the type signature of a stack that
contains elements of that type.

We can then implement functions on this type which are independent of the type
parameter, i.e. they work on a stack of anything:

```haskell
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> (a, Stack a)
pop (Stack [])     = error "Empty stack!"
pop (Stack (x:xs)) = (x, Stack xs)
```

Cool! Only, there are two issues. The first issue is that popping an empty stack
causes the program to fail, the second issue is that the output of `pop` is a
little unwieldy. We'll address the first problem in the next section with the
`Maybe` type constructor. The second issue is largely due to the fact that a
stack is a *mutable* data structure, by definition, and so it doesn't really
make sense to use them in functional code. Not to mention that lists can be
treated like stacks if necessary (it's never necessary).

## Maybe and Either

### Maybe

`Maybe` is a type constructor that represents potential failure in a
computation. Here's the definition:

```haskell
data Maybe a = Nothing | Just a
```

That is to say, computation either returns `Nothing`, or `Just` some value.
Notice that we could have `Maybe Bool`, `Maybe Int`, `Maybe String`, etc. This
is why `Maybe` is called a type constructor, it takes a type as argument and
returns a concrete type.

Let's define a safe division function:

```haskell
safeDiv :: Integral a => a -> a -> Maybe a
_ `safeDiv` 0 = Nothing
x `safeDiv` y = Just (x `div` y)
```

Easy enough. Now if we divide by zero, no error will occur and we can deal with
the problem safely.

And for our stack example:

```haskell
pop :: Stack a -> Maybe (a, Stack a)
pop (Stack [])     = Nothing
pop (Stack (x:xs)) = Just (x, Stack xs)
```

`Maybe`s can be used any time a function is not total (i.e., allows inputs for
which there is no valid output). Partial functions are not actually considered
functions mathematically speaking (because we are, in a sense, lying about the
domain of the function), but by using `Maybe` we create an extra point in the
codomain which serves as the image for all those values which would have
otherwise caused the original partial function to fail.

### Either

`Either` represents failure as well, but this time you can, if you want, include
an error message.

```haskell
data Either a b = Left a | Right b
```

There's actually a lot of different things you can use `Either` for, but the
assumed use by almost all Haskell libraries is error handling, so usually you'll
see things like:

```haskell
safeDiv :: Integral a => a -> a -> Either String a
_ `safeDiv` 0 = Left "Can't divide by zero"
x `safeDiv` y = Right (x `div` y)
```

Very similar to `Maybe`, but much more useful. Doesn't leave you in the dark as
to how the computation failed.

## Fun with Algebraic Data Types

> See [here](http://stackoverflow.com/questions/9190352/abusing-the-algebra-of-algebraic-data-types-why-does-this-work)
for the original SO question.

Remember we said that data types form an algebra, with operations 'sum' and
'product'? Let's flesh out this idea a bit more.

Assuming we represent 'sum' (as in *sum type*) by '$$ + $$' and 'product' (as in
*product type*) by '$$ \cdot $$', we can now convert types into a form of
shorthand.

```haskell
data List a = Nil | Cons a (List a)
```

This is a simple recursive linked list data type. `Nil` is a unit type, and
`Cons a` is a partially applied type referring to a single element in the list.
Here's the new representation:

$$
\begin{align}
L = 1 + x \cdot L
\end{align}
$$

So, $$ L $$ is the list, '$$ 1 $$' is `Nil`, and $$ x $$ is a single element in
the list. This may look unintuitive, but make sure to remember that the symbols
$$ + $$ and $$ \times $$ are not representative of their effect on numbers, but
rather they are representative of their general behaviour and interoperability
(i.e. distributivity).

Let's pretend these aren't types, and that we can just manipulate these like
regular mathematical expressions. Ignore all rigour and just see what happens.

$$
\begin{align}
L &= 1 + x \cdot L\\
\Rightarrow \quad 1 &= (1 - x) \cdot L\\
\Rightarrow \quad L &= \frac{1}{1 - x}\\
&= 1 + x + x^2 + x^3 + \cdots
\end{align}
$$

That last part is only sometimes justified ($$ \lvert x \rvert < 1 $$, as you
may remember) but we're not concerned with that for now. Look at what this says.
It says that a list is empty, or it has one element, or it has two elements, or
... on forever, which is exactly what a linked list is!

We can do the same thing for binary trees:

```haskell
data Tree a = Nil | Node a (Tree a) (Tree a)
```

... which can be represented as:

$$
\begin{align}
T &= 1 + x \cdot T^2\\
\Rightarrow \quad 0 &= x \cdot T^2 - T + 1\\
\Rightarrow \quad T &= \frac{1 - \sqrt{1 - 4x}}{2x}\\
&= 1 + x + 2x^2 + 5x^3 + 14x^4 + \cdots
\end{align}
$$

Hmm, what's going on here? Also, square rooting types? What's the square
root of a *type*? All of this can be defined, but for now don't think about it
yet, we're experimenting!

To see why this makes sense, we'll ask ourselves: how many different binary
trees can we make using zero items? One (technically). How many can we make
using one? One. How many can we make using two items? Two. Three items? Five.
See what's happening?

As you may be guessing, the $$ (\text{coefficient}, \text{power}) $$ pairs tell
us how many distinct binary tree structures we can make with
'$$ \text{power} $$' nodes. It's easy to verify that 3 nodes allows exactly 5
distinct tree shapes. Try list all 14 you can get from 4 nodes.

Feel free to play around with more data types to see what you can find, and make
sure to use [Wolfram|Alpha](http://www.wolframalpha.com/) to help with finding
Taylor series expansions like the ones I have above if you need. If you want to
explore further, be sure to look into category theory (warning: requires a lot
of mathematical maturity).

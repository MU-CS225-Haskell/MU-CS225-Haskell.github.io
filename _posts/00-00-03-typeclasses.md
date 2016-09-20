---
layout: post
title: 03 - Typeclasses
date: 2016-07-29
---

<h3>Contents</h3> [//]:# (This ensures that the TOC doesn't include this header)

* TOC
{:toc}

I'd rather not launch into defining typeclasses just yet. I'd rather motivate
their existence first.

## Quick Motivating Example

Let's consider the following function:

```haskell
reverse :: [Int] -> [Int]
reverse = foldl (flip (:)) []
```

This will take a list of integers and reverse that list. That's fine, but it's
a little restricted in its usefulness. Clearly we can reverse more than just
integers, we can technically reverse any list of distinguishable objects. We can
represent this by using a type variable:

```haskell
reverse :: [a] -> [a]
reverse = ...
```

Much better! Here, `a` is the type variable, it refers to any type at all.

`[a] -> [a]` will be the inferred type for this function if you leave out the
type signture, try `GHCi> :t foldl (flip (:)) []` in the REPL.

But say we want to sort a list. For example, can we do this using a mergesort
algorithm (here for reference, *hint hint*):

```haskell
sort :: [a] -> [a]
sort xs =
  case tail xs of
    [] -> xs
    _  -> merge (sort left) (sort right)

      where (left, right) = splitAt (length xs `div` 2) xs

            merge    []     rs  = rs
            merge    ls     []  = ls
            merge (l:ls) (r:rs)
              | l < r     = l : merge    ls (r:rs)
              | otherwise = r : merge (r:rs)   rs
```

Nope! Why? Well, it doesn't always make sense be able to order *every* type. For
example, it doesn't make sense to order complex numbers. So instead, we need to
restrict the type signature to just those types that *can* be ordered. This
ensures that our function is still general enough to sort a list of any
orderable type. For this, we use the `Ord` typeclass:

```haskell
sort :: Ord a => [a] -> [a]
sort = ...
```

This will now compile!

OK, so typeclasses seem to define a template for the behaviour of a type. The
`Num` typeclass gives a template for how a number should behave:

```haskell
GHCi> :i Num
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  	-- Defined in 'GHC.Num'
instance Num Word -- Defined in 'GHC.Num'
instance Num Integer -- Defined in 'GHC.Num'
instance Num Int -- Defined in 'GHC.Num'
instance Num Float -- Defined in 'GHC.Float'
instance Num Double -- Defined in 'GHC.Float'

-- ^^^ These are all the types which have their behaviour with respect to
--     Num defined.
```

If we have a type, and wish to reasonably call it a number, it must support
these functions. We implement this behaviour using instance declarations.

To really understand how all this works, we'll create our own data type, called
`Complex`, to represent complex numbers.

### The Complex Numbers

Create a new project: `stack new complex`, you'll have two directories in there,
`app` and `src`. In `src`, delete the file `Lib.hs` and create a new file called
`Complex.hs`. Then go into `complex.cabal` and replace any references to `Lib`
with `Complex`. Also in `app/Main.hs`, delete everything and just write:

```haskell
import Complex

main :: IO ()
main = undefined
```

Now move back into `src/Complex.hs` and put `module Complex where` at the top.
We'll be working in this file from now on.

Let's start by creating the data type:

```haskell
infix 6 :+
data Complex a = a :+ a
  deriving (Eq, Show, Read)
```

Cool, that's all we need to do. By declaring `infix 6` we're defining `:+` to
be a non-associative data constructor with fixity 6. If you're worried about
efficiency, you can define it like this instead:

```haskell
data Complex a = !a :+ !a
```

This forces each argument to `:+` to be strict, which is a good idea since `a`
will only ever be a numeric type.

The `deriving` part automatically implements simple typeclasses like `Eq` so you
don't have to.

Let's implement some functionality! First we'll define two convenience
functions, `re` and `im`:

```haskell
-- | Extracts real part of a complex number
re :: RealFloat a => Complex a -> a
re (a :+ _) = a

-- | Extracts imaginary part of a complex number
im :: RealFloat a => Complex a -> a
im (_ :+ b) = b
```

We'll need these a lot. Now we can make `Complex a` an instance of the `Num`
typeclass:

```haskell
instance RealFloat a => Num (Complex a) where
  -- | Addition
  (a :+ b) + (c :+ d) = (a + c) :+ (b + d)
  -- | Multiplication
  (a :+ b) * (c :+ d) = (a*c - b*d) :+ (b*c + a*d)
  -- | Negation
  negate (a :+ b) = negate a :+ negate b
  -- | Absolute value
  abs (a :+ b) = sqrt (a^2 + b^2) :+ 0
  -- | Signum
  signum   (0 :+ 0) = 0 :+ 0
  signum z@(a :+ b) = a/r :+ b/r where r = re (abs z)
  -- | Conversion from integers
  fromInteger n = fromInteger n :+ 0
```

What we're doing here is telling the compiler how to deal with `Complex` numbers
in the context of `Num`. Any function which only needs a `Num a` typeclass
constraint can now accept `Complex` numbers as arguments.

Let's add more functionality:

```haskell
magnitude :: RealFloat a => Complex a -> a
magnitude = re . abs

conjugate :: RealFloat a => Complex a -> Complex a
conjugate (a :+ b) = a :+ (-b)

phase :: RealFloat a => Complex a -> a
phase (0 :+ 0) = 0
phase (a :+ b) = atan2 y x

-- | Fractional instances
instance RealFloat a => Fractional (Complex a) where
  (a :+ b) / z@(c :+ d) = (a*c + b*d)/k :+ (b*c - a*d)/k
                            where k = (magnitude z)^2
  fromRational a = fromRational a :+ 0
```

OK, that should be enough. By implementing these typeclasses, we're implementing
behaviour on `Complex` numbers that allows us to integrate them seamlessly into
existing numerical libraries (which invariably use numeric typeclasses). Check
out [Data.Complex](http://hackage.haskell.org/package/base-4.6.0.0/docs/src/Data-Complex.html)
for the full source, it only takes ~150 lines to define all base functionality
for the type!

### The List Type

To showcase some typeclasses that aren't numeric, we'll try define our own list
type and make sure it integrates seamlessly with the existing Haskell ecosystem.

First, let's see what typeclasses the built in list type is an instance of:

```haskell
GHCi> :i []
data [] a = [] | a : [a]

instance Eq a => Eq [a]      -- derivable
instance Monad []
instance Functor []
instance Ord a => Ord [a]    -- derivable
instance Read a => Read [a]  -- derivable
instance Show a => Show [a]  -- derivable
instance Applicative []
instance Foldable []
instance Traversable []
instance Monoid [a]
```

That's a lot, but thankfully we only have to provide explicit instances for
those that do not have 'derivable' next to them.

Here's the definition for `List`:

```haskell
data List a = Nil | Cons a (List a)
  deriving (Eq, Ord, Show, Read)
```

We've seen this at the end of the previous topic, in the 'Fun with Algebraic
Data Types' section. A list is either empty, or it's a single value prepended
onto another list.

Let's start with `Functor`.

#### Functor

The Functor typeclass (loosely) describes types that have a container-like
structure to them. We only need to define one function for a complete `Functor`
instance, `fmap`, which has type<br/>`(a -> b) -> f a -> f b`. Our `List` type can
be considered a container, in particular `List a` is a container for `a`s. Note
that a `Functor` must be a type constructor that takes only one argument.

For our `List` type, the only implementation of `fmap` that really makes sense
is to have it map a function over every value in the list. We'll implement this
functionality here:

```haskell
instance Functor List where
  fmap _ []          = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
```

This shouldn't be too hard to follow, especially if you've seen and understand
the definition of `map`.

More generally, `fmap` is a function that it takes a function and lifts it into
the context of the Functor. Think of `fmap (+1)` as a new function which
operates on whatever Functor we're currently using.

For example

```haskell
GHCi> fmap (+1) (Just 4)
Just 5

GHCi> fmap (+1) [1,2,3]
[2,3,4]

GHCi> fmap (+1) (Left "Error")
Left "Error"

GHCi> fmap (+1) (Right 3)
Right 4
```

Now we'll move onto `Monoid`.

#### Monoid

The `Monoid` typeclass encapsulates the behaviour of a mathematical structure of
the same name. A monoid is a triple $$ (M,\cdot,e) $$, where $$ M $$ is a set,
$$ \cdot $$ is an [associative](https://en.wikipedia.org/wiki/Associative_property)
binary operator defined on that set, and $$ e $$ is the
[identity element](https://en.wikipedia.org/wiki/Identity_element) with respect
to that operator.

If that sounds a little confusing, don't worry too much, the idea is very
simple. It's easier to see examples of monoids than to scratch your head looking
at the definition. One example is `([a], ++, [])`, that is, lists are a monoid
when coupled with `++` (which is associative) and `[]`, the empty list, which is
the identity with respect to `++`.

We need a `Monoid` typeclass because there are lots of types that support this
type of behaviour, and being a monoid, while not immediately interesting, leads
to some very useful functionality later on.

Let's write an instance of `Monoid` for `List a`. We need to define two things,
`mappend`, which is the monoid operator, and `mempty`, which is the identity
with respect to that operator.

```haskell
instance Monoid (List a) where
  mempty = Nil
  Nil         `mappend` ys = ys
  (Cons x xs) `mappend` ys = Cons x (xs `mappend` ys)
```

If you look at the definition of `++` in the Standard Prelude, they're
essentially the same thing. The monoid type tells Haskell how to combine our
lists using an associative operation (think folding).

We'll also import `Data.Monoid`, since it exports `<>`, an infix synonym for
`mappend`. We'll use it instead of `mappend` since it makes the code more
readable.

Now we'll look at `Foldable`.

#### Foldable

The `Foldable` typeclass describes container types that can be collapsed into a
single value. The main example of this is, of course, the list. Any time you use
`foldl`, `foldr`, `sum`, `product`, etc., you're collapsing a list into a single
value.

Only that's not *exactly* it. We have `foldl` and `foldr` as separate functions
for a reason. Since they can accept non-associative operators as arguments,
they can (and often do) behave differently. They also require a starting value,
otherwise, on empty lists, they will fail. The `Foldable` typeclass, however,
makes use of the `Monoid` typeclass to provide a more extensible solution by
enforcing a `Monoid` instance on the contained type before attempting to fold
it. Once we have a container of `Monoids`, the starting value and folding
function are decided for us.

Let's see the instance for `List`:

```haskell
instance Foldable List where
  foldMap _ Nil         = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs
```

What's this function `f` that's being passed? This function has type `a -> m`,
where `m` is a `Monoid`. It 'creates' a monoid out of values that may not have
a direct `Monoid` instance available, then folds that list into a single value
using the `mappend` function defined for that monoid. For example, the function
`sum` takes a `Foldable` container and sums its contents. Let's see the
definition:

```haskell
sum :: (Num a, Foldable f) => f a -> a
sum = getSum . foldMap Sum
```

`Sum` is a `newtype` wrapper, defined as `newtype Sum a = Sum { getSum :: a }`.
All it does is specialise `Num` types to a single `Monoid` instance. We need
this since there are at least two obvious instances for numbers: we either
couple them with addition and zero, or multiplication and one. Both form valid
`Monoid` instances. Indeed, there also exists a `Product` type of the same kind:

```haskell
product :: (Num a, Foldable t) => t a -> a
product = getProduct . foldMap Product
```

Now that we have a `Foldable` instance for our list, we have access to a great
deal of useful built-ins. See
[here](http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Foldable.html)
for more information.

Now we'll look at `Traversable`.

#### Traversable

`Traversable` defines a class of data structures that can be traversed from left
to right, performing an action on each element. Notice how I say 'performing an
action'. This is not the same as computing a value. An action is typically
described in terms of types which fall into typeclasses called `Applicative` and
`Monad`, which we'll discuss in the next section. We won't implement them for
our list until we understand what they're used for.

`Traversable` in itself is an easy typeclass to understand, however the way it
is defined is not, unless you understand the `Applicative` typeclass. For now,
copy the rest of the code from
[here](https://github.com/MU-CS225-Haskell/CS225-examples/blob/master/linked-list/src/LinkedList.hs)
and try out the new list type. It also features a better `Show` instance so it's
easier to read the output.

One interesting thing to notice is that this took only took 25 significant lines
of code to (almost) completely link our new list type to the entire Haskell
ecosystem. We could almost call it geniunely useful!

OK, let's move onto the big boys: `Applicative` and `Monad`.

## Applicative and Monad

Even though `Applicative` is a subclass of `Monad` (in the same way that `Eq` is
a subclass of `Ord`: all orderable types must be equatable), we'll cover `Monad`
first, since it is the more powerful abstraction.

### Monad

Let's first imagine a scenario wherein we have lots of nice, safe functions
(made safe by the `Maybe` type):

```haskell
divideBy :: (Real a, Floating b) => a -> a -> Maybe b
divideBy 0 _ = Nothing
divideBy x y = Just $ y // x
  where (//) = (/) `on` realToFrac

safeSqrt :: (Real a, Floating b) => a -> Maybe b
safeSqrt n
  | n < 0     = Nothing
  | otherwise = Just $ sqrt $ realToFrac n

safeMaximum :: (Ord a, Foldable t) => t a -> Maybe a
safeMaximum xs
  | null xs   = Nothing
  | otherwise = Just $ maximum xs
```

OK, so these are all basically just safety nets around existing functions (I'm
not very imaginative) with some `realToFrac` sprinkled in so they can accept
more types (not really the best idea, good for testing though), but they'll do.

With the original functions, we could do things like:

```haskell
-- Them typeclass constraints
foo :: (Real a, Floating b, Foldable t) => t a -> b
foo = sqrt . sqrt . (// 3) . (*17) . maximum
  where (//) = (/) `on` realToFrac
```

But how the hell are we supposed to do that with our functions?

```haskell
horribleButSafeFoo :: (Real a, Floating b, Foldable t) => t a -> Maybe b
horribleButSafeFoo xs =
  case safeMaximum xs of
    Nothing -> Nothing
    Just x  ->
      case divideBy 3 (x*17) of
        Nothing -> Nothing
        Just x' ->
          case safeSqrt x' of
            Nothing  -> Nothing
            Just x'' -> safeSqrt x''
```

If that doesn't make you physically ill, I don't know what will.

This is mainly awkward because we have lots of functions of the form
`a -> Maybe b`, so we can't just compose them normally. But we *do* know how we
*should* be combining them, so let's abstract this idea into a function. It'll
take a value of type `Maybe a`, pass it to a function of type `a -> Maybe b` in
a way that makes sense, and will then spit out a `Maybe b`. So our combining
function needs to have type `Maybe a -> (a -> Maybe b) -> Maybe b`.

Let's define it.

```haskell
infixl 1 |>
(|>) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing |> _ = Nothing
Just x  |> f = f x
```

If we had `Nothing` before, something failed, so we want to just pass `Nothing`
through regardless of the function we're passing it into. If we get `Just x`, we
want to apply `f` to `x`.

But there's still one problem: what if we want to use a regular `a -> b`
function? We'll need to convert it into an `a -> Maybe b` function. We'll call
this function `lift` (i.e. we're 'lifting' the function into a `Maybe` context).
It's really easy to define this:

```haskell
lift :: (a -> b) -> a -> Maybe b
lift f x = Just (f x)
```

We just wrap the result in a `Just` (since presumably `f` can't fail).

Now we can write functions like this:

```haskell
safeFoo :: (Real a, Floating b, Foldable t) => t a -> Maybe b
safeFoo xs = safeMaximum xs
          |> lift (*17)
          |> divideBy 3
          |> safeSqrt
          |> safeSqrt
```

Much better! Well, apart from all the 'safe' prefixes, which won't be present in
real code.

We could create a combinator for `Either` too:

```haskell
infixl 1 |>>
(|>>) :: Either a b -> (b -> Either a c) -> Either a c
Left err |>> _ = Left err
Right x  |>> f = f x
```

This works in much the same way, honouring the errors of the previous
computations. It's getting a little annoying having to define all these new
operators though. We'll fix this soon.

#### Pattern?

See the pattern? It's certainly easy to see how you could abstract this operator
over multiple failure-oriented types, but we can go more general than that.

We can think of this 'error checking' as being one specific example of a
*computational context*, the context being 'error checking'. The thing that both
of these examples have in common is that they provide an extra context that
exists on top of simply computing the value. But there are a lot of ways that
this can work!

Types that support a 'computation with effects' or 'computation with context'
type behaviour are called **monads**, which in Haskell is abstracted into a
typeclass, appropriately called `Monad`.

To implement `Monad` for a given type, we only need to specify how to combine
functions of type `a -> m b` where `m` is a monad, and also how to lift values
into the context of the monad (i.e. `a -> m a`). These two functions are called
**bind** and **return**, respectively. 'bind' is represented by the infix
operator `>>=`, with fixity `infixl 1`, and `return` is just `return`.

Let's see the `Monad` instance for `Maybe` and `Either a`:

```haskell
instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just x  >>= f = f x

instance Monad (Either a) where
  Left err >>= _ = Left err
  Right x  >>= f = f x
```

We don't actually need an explicit implementation for `return` since it's the
same as `pure` from the `Applicative` typeclass.

### Applicative

The `Applicative` typeclass defines an abstraction that is more powerful than
`Functor`, but not quite as powerful as `Monad`. Depending on the context, it
may be easier/clearer to simply use an `Applicative` instance, but usually
you'll just use `Monad`.

Functor allowed us to apply a function of type `a -> b` to a value of type
`f a`, returning an `f b`, however it does not allow us to apply a function
which is already in the `Functor` context to a value of type `f a`. I.e., in
`Functor` we have no mechanism that allows us to apply `f (a -> b)` to a value
of type `f a`. `Applicative` gives us that power.

It is difficult to gain true intuition for Applicative until we see them in
action, but I'd rather defer this for a bit. Parsers are one particular case
where the applicative style can be a lot clearer (we'll see this if we look at
the parsec package).

### List revisited

Let's look back at the `Monad` instance for `List`. Hmm, in what way could
`List` provide a computational context? One way is through *nondeterminism*.

We can think of a function `a -> List b` as being a computation that returns
multiple possible results (finding complex roots is a good example). Chaining
computations of this sort together would involve generating lots of values, then
applying each possible value to the next function, which would then generate
even more values. Let's see the implementation:

```haskell
instance Monad List where
  Nil       >>= _ = Nil
  Cons x xs >>= f = f x <> (xs >>= f)
```

We'll break this down with an example. Consider this highly unrealistic one:

```haskell
[1,2,3] >>= replicate 3 >>= replicate 3
```

We'll use regular built-in lists, since the implementation is the same. Let's
work this out using the definition (noting that `<>` is `++` for lists):

```haskell
    [1,2,3] >>= replicate 2 >>= replicate 2
==> ([1,2,3] >>= replicate 2) >>= replicate 2
==> ([1,1] ++ ([2,3] >>= replicate 2)) >>= replicate 2
==> ([1,1] ++ ([2,2] ++ ([3] >>= replicate 2))) >>= replicate 2
==> ([1,1] ++ ([2,2] ++ ([3,3] ++ ([] >>= replicate 2)))) >>= replicate 2
==> ([1,1] ++ ([2,2] ++ ([3,3] ++ ([])))) >>= replicate 2
==> [1,1,2,2,3,3] >>= replicate 2
==> ...
==> [1,1,1,1,2,2,2,2,3,3,3,3]
```

If your eyes can make it through the brackets (helps if you know Lisp) then this
should make sense.

### Custom Logger

Suppose we wanted to create a `Logger` data type, which would perform a
computation, but also describe that computation and store it in a log.
Computation with effects, sounds like a great use case for `Monad`!

I've implemented the logger
[here](https://github.com/MU-CS225-Haskell/CS225-examples/blob/master/logger/src/Logger.hs),
make sure to read through the code carefully.

Really we'd like to jump into creating our `Monad` instance, but since `Functor`
and `Applicative` are subclasses of `Monad`, we need to implement them as well.
Luckily, once we have a `Monad` instance written, it is possible to define
`Functor` and `Applicative` in terms of that instance (see the source for more
details).

```haskell
instance Monad Logger where
  l >>= f =
    let (v,  l1) = runLogger l
        (v', l2) = runLogger (f v)
    in  Logger (v', l1 <> l2)
```

Similar idea: we apply `f` to `v` to get `v'` (the result), then append the new
log generated by `f v` to the initial log. But the astute Haskeller will notice
that this is another valid instance:

```haskell
instance Monad Logger where
  Logger (v, l) >>= f = Logger (id, l) <*> f v
```

So, in a sense, we don't actually need the full power of monads here, since they
can be defined in terms of the applicative style, but it's easier to use monads
in this case.

### I/O

There's a type we've been neglecting to mention throughout these notes: the `IO`
type. You may have noticed that most I/O is done using `do` notation. You know
what this means: `IO` is a monad!

I won't dwell excessively on `IO`. You should treat something of type `IO a` as
being a value `a` which is linked in some way to I/O. Things like `getLine`
(which has type `IO String`) can be thought of as being a simple container
holding a string, only the container is a representation of everything outside
your program. (Philosophical question: if `IO` represents the outside world, in
what sense is `IO` really a container?)

One thing that might surprise you is that Haskell I/O *is* purely functional, in
the sense that `IO` actions (which are, in other languages, often nasty and
difficult to reason about) are combined in a purely functional way. It is only
when the code is *executed* that effects take place.

But surely I/O in Haskell must break referential transparency? Well, no. Notice
that functions like `getLine` are not actually functions, they are *values*. In
fact, Haskell's main function is really just a big `IO` data type with `()` as
the value. Be sure to read through [this](http://chris-taylor.github.io/blog/2013/02/09/io-is-not-a-side-effect/)
blog post for more info on this, if you're interested. Also [here](https://wiki.haskell.org/Referential_transparency).

Regardless of this, we needn't completely understand how it is referentially
transparent in order to use it.

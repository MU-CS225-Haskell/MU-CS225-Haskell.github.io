---
layout: page
permalink: /8c4d1cf0c47810e21abf/
title: Exam Prep
visible: false
---

As far as I'm concerned, the programming assessment for this module is finished.
The Haskell questions on the CS225 exam paper will test theoretical knowledge.

Things you'll need to know:

- What is a typeclass? Make sure you can give examples and explain them well.
- What is a monoid? You should be aware of the mathematical definition,
otherwise it may be difficult, if not impossible, to reason about the details.
- Converting code from `do`-notation to code that uses `>>=` and lambda
expressions. You'll also need to be able to 'tidy up' monadic code. This will be
explained in detail below.
- Any other questions will only require knowledge of basic Haskell syntax and
language features such as laziness and referential transparency.

An example of desugaring `do`-notation:

```haskell
main = do
  x <- action1
  y <- action2
  z <- action3
  let t = f x y z
  print t

main =
  action1 >>= \x ->
  action2 >>= \y ->
  action3 >>= \z ->
  let t = f x y z in print t
```

Ordinarily it's difficult to reduce this much further, but we can with a
little knowledge of some library functions:

```haskell
main = print =<< liftM3 f action1 action2 action3
```

Much nicer! Here's a longer example, taken from HackerRank. This is the default
starting code they give you for most of the problems in the 'Algorithms'
section. It is *disgusting*.

```haskell
import Control.Applicative
import Control.Monad
import System.IO

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    arr_temp <- getLine
    let arr = map read $ words arr_temp :: [Int]

getMultipleLines :: Int -> IO [String]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        xs <- getMultipleLines (n-1)
        let ret = (x:xs)
        return ret
```

You long for the sweet release of death. I understand. Nevertheless, we must
overcome such temptations and fix this mess.

Let's first get rid of the `getMultipleLines` function because it's completely
pointless and not used anywhere (you can replace it with `replicateM n getLine`
if you want). We can also get rid of the Control.Monad and System.IO imports
because they also aren't used (unless you decide to change `getMultipleLines`).

```haskell
import Control.Applicative
import Control.Monad

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    arr_temp <- getLine
    let arr = map read $ words arr_temp :: [Int]

getMultipleLines :: Int -> IO [String]
getMultipleLines n = replicateM n getLine
```

OK, we won't desugar the main function quite yet, instead we'll take a look at a
little shortcut. Say we have:

```haskell
main = do
  x <- action
  let y = f x
  -- ...
```

We can utilise `fmap` (or `<$>`) to make this a lot shorter.

```haskell
main = do
  y <- f <$> action
  -- or
  y <- fmap f action
```

This utilises the Functor instance for `IO`. It also makes much clearer what we
actually want to do: perform the action and apply the function to it. Using this
to refactor the main function:

```haskell
main :: IO ()
main = do
    n <- read <$> getLine
    arr <- map read . words <$> getLine
```

Much better! This is probably as clear as it's going to get, but if you want to
do the standard desugaring this will transform into

```haskell
main :: IO ()
main =
  read <$> getLine             >>= \n ->
  map read . words <$> getLine >>= \arr ->
  -- ...
```

Which, depending on the rest of the code, could potentially be shortened further.
In the exam I may ask for you to completely get rid of the `do` keyword, which
absolutely will require that you desugar as above.

To be clear, desugared is not always better, but generally speaking it's a good
idea to be aware of how `do`-notation desugars to encourage cleaner code. There
is a tendency among the newly enlightened to think that pointfree + desugared
`>>=` filled code is the only way, and everyone else is a heathenous buffoon.
This is obviously wrong in a lot of cases. Take for example:

```haskell
f x = x^2 + 2*x + 1
```

Easy. Now consider the pointfree version which uses the Applicative instance
for functions:

```haskell
f = ((+) <$> (^2) <*> (*2)) . (+1)
```

What do you mean it's unclear? HEATHEN.

Basically, write clear code. The syntax in Haskell allows you to write very
clear and readable code, don't become that guy who writes crap like the above to
show off (definitely not in production code because your team will hate you).

Anyway, good luck in the exams!

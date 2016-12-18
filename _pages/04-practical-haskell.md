---
layout: page
title: Practical I/O
permalink: /do-a-haskell/
visible: true
---

It's assumed here that you don't know much but just want to get a program
running that can actually interact with the real world. Maybe you don't
understand monads yet (or even know what they are) but just want to do some I/O.
Well, dear reader, you have come to the right place! All you need to know is the
function composition operator, '`.`', and how to define functions and type
signatures.

Luckily, you don't need to know anything about monads to do I/O in Haskell. Just
because `IO` is a monad does not mean you need to know what that entails.
Technically `IO` is also a
[strong lax monoidal functor](https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Applicative.html),
but you clearly don't need to care about that to make effective use of it.

True, I/O in Haskell does make use of `do`-notation, which is not just for I/O,
it's for all monads, but again, we can use it without understanding every
detail.

Oh and a note before we start, I/O is pretty weird in the REPL (certainly if
you're using Linux, don't know about Windows or OSX) so try to test I/O stuff in
files if you can.

<h3>Contents</h3> [//]:# (This ensures that the TOC doesn't include this header)

* TOC
{:toc}

## HackerRank I/O

HackerRank always likes to give comically unrealistic input sets (for good
reason, in fairness), so it's a good idea to know how you can get that input in
Haskell.

One of the most common ways to do this is to just use the `getContents` function
(which throws all of the input into a single string, which you can then parse),
but there are other specialised ones like `getLine`, `readLn`, etc., which may
be better depending on the context. It's worth noting that you won't be reading
from files, you'll be reading from '[stdin](http://stackoverflow.com/a/3385261)'.

Imagine we wish to read in two things: `n`, a number on its own line, and `ns`,
a line containing `n` space seperated integers. For example:

```
6
-4 3 -9 0 4 1
```

The number on its own is easy, we simply use `readLn`, which reads the entire
line as a string and attempts to read it as some Haskell data type. We can't
have Haskell guessing what type that is, so we should specify (for now).

```haskell
main = do
  n <- readLn :: IO Int
  -- ...
```

Think (for now) of `<-` as unwrapping the `Int` from the `IO Int`, which is what
`readLn` returns. This is kind of wrong theoretically, but it's good to think
about it this way when still gaining intuition.

Now to read in the next line, we'll use `getLine`, which just reads the next
line in as a string, and doesn't try to convert it into anything else.

```haskell
main = do
  n <- readLn :: IO Int
  rawStr <- getLine
  -- ...
```

Now lets define a function called `parseLine` which will take our string and
convert it into a list of numbers.

```haskell
parseLine :: String -> [Int]
parseLine = map read . words
```

So it applies `words` to the string, then applies `map read`. Here's what that
does:

```
                  "1 2 3 4"
Apply `words`:    ["1","2","3","4"]
Apply `map read`: [1,2,3,4]
```

`words` just splits the string by whitespace, then `map read` applies the
function `read` to each element in the list. Since the type signature for
`parseLine` is `String -> [Int]`, `read` knows it has to try to parse each
element as an `Int`.

Now we can do this:

```haskell
main = do
  n <- readLn :: IO Int
  rawStr <- getLine
  let ns = parseLine rawStr
  -- ...
```

The `let` binding is what we need to use if we want to bind values to names
outside of I/O. `parseLine` is a pure function, so we don't need to 'extract'
it from the `IO` wrapper because it *has* no `IO` wrapper.

This is fine, but we can make it more terse:

```haskell
main = do
  n  <- readLn :: IO Int
  ns <- parseLine <$> getLine
  -- ...
```

Much better! Anyone wondering what `<$>` does can go and learn about typeclasses
and functors (`<$>` is an infix synonym for `fmap`), but practically speaking,
it allows you to apply the function `parseLine` to the boxed value in `getLine`.

By the way, in case you didn't notice, we won't need `n` at all! HackerRank
gives you this in case you're using a language like C, since it makes it
significantly easier to read the line of space seperated things if you know how
many things there are. In Haskell, you just use `getLine` and be done with it.

To show that you just want to throw out the first number, you can just write
this:

```haskell
main = do
  getLine  -- Consumes first line, but doesn't bind it to anything
  ns <- parseLine <$> getLine
  -- ...
```

Sometimes people prefer to write `_ <- getLine` instead of just `getLine` to
emphasize that we're throwing the input out.

Let's try another example. This time we want to read in a matrix, e.g.

```
1 2 3
3 1 2
2 3 1
```

We're going to ignore the fact that you really should read this in as an actual
matrix data type and just read it in as a list of lists. That is to say, we want
to go from the above to this:

```
[[1,2,3],[3,1,2],[2,3,1]]
```

Thankfully, the code is very similar! I'll just write the whole thing out in one
go:

```haskell
main = do
  mat <- parseMatrix <$> getContents
  -- ...

parseMatrix :: String -> [[Int]]
parseMatrix = map (map read . words) . lines
```

The I/O side of things should make sense (`getContents` just reads everything in
as one big string), but we'll go through `parseMatrix`.

`lines` splits the string by the newline character(s) (differs based on OS). So
after we call `lines` we get `["1 2 3", "3 1 2", "2 3 1"]`. Then we want to
apply `map read . words` to each element in this list. But wait! We've seen this
function before, it's just `parseLine` from above. Thus we can rewrite this:

```haskell
parseLine :: String -> [Int]
parseLine = map read . words

parseMatrix :: String -> [[Int]]
parseMatrix = map parseLine . lines
```

It should be very clear what this does now.

If you know exactly how much things you need to read in (on one line), this can
be useful:

```haskell
main = do
  -- This will bind the first 3 Ints on the line to the names 'a', 'b', and 'c',
  -- respectively. Helpful if you know exactly how many Ints are on the line.
  [a, b, c] <- parseLine <$> getLine
```

___

<br/>

More to come!

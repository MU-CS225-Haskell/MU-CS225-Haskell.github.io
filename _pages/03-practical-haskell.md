---
layout: page
title: Practical I/O
permalink: /do-a-haskell/
visible: true
---

I/O is a thorny issue. It is, by its nature, difficult to reason about, since
there is rarely any guarantee in advance that an I/O action will succeed or
fail. The creators of Haskell knew this, so they created the `IO` data type.
There is really nothing special about this type: it's just like `Bool` or `[]`
or `Seq` or any other data type. We will explore this type in more detail in due
course.

Here, we'll go through some very quick examples of reading simple data into your
program so you can start using Haskell quasi-practically without trying to
understand exactly how I/O works. If you already know some imperative
programming then this should be easy to follow.

<h3>Contents</h3> [//]:# (This ensures that the TOC doesn't include this header)

* TOC
{:toc}

## HackerRank I/O

Suppose we want to read in two things: `n`, a number on its own line, and `ns`,
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

So it applies `words` to the string, then applies `map read`. `words` will split
the string by spaces, and `read` will try to convert the string into some
Haskell data type. Here's what that does:

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

This is fine, but we can make it a little easier to read:

```haskell
main = do
  n  <- readLn :: IO Int
  ns <- parseLine <$> getLine
  -- ...
```

Much better! Practically speaking, `<$>` allows you to apply the function
`parseLine` to the boxed value in `getLine`.

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

More to come! -->

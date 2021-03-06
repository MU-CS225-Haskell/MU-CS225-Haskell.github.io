---
layout: page
title: Practical Haskell
permalink: /do-a-haskell/
visible: true
---

## Simple I/O

I/O is a thorny issue. It is, by its nature, difficult to reason about, since
there is rarely any guarantee in advance that an I/O action will succeed or
fail. The creators of Haskell knew this, so they created the `IO` data type.
There is really nothing special about this type: it's just like `Bool` or `[]`
or `Seq` or any other data type. We will explore this type in more detail in due
course.

Here, we'll go through some very quick examples of reading simple data into your
program so you can start using Haskell quasi-practically without trying to
understand exactly how I/O works. If you already know some imperative
programming languages then this should be easy to follow. This should cover most
HackerRank-esque use cases.

<h3>Contents</h3> [//]:# (This ensures that the TOC doesn't include this header)

* TOC
{:toc}

### HackerRank I/O

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

### More Complex Data

To parse data much more complicated than the above, it's a good idea to use some
of Haskell's varied parser combinator libraries. We'll use `megaparsec` in this
example. Even if the data is way simpler than in this example, I'd probably
still recommend using `megaparsec`, or maybe `attoparsec` if you need raw speed.
Haskell's base string parsing functionality is completely awful (in particular,
the `Read` typeclass is *painful*), so, if for nothing else other than code
clarity and correctness, always use a parser combinator library.

We're going to try to write a parser to automatically analyse
[Heroku](https://www.heroku.com/) log files. Here's an example from their
[specification](https://devcenter.heroku.com/articles/logging):

```log
2010-09-16T15:13:46.677020+00:00 app[web.1]: Processing PostController#list (for 208.39.138.12 at 2010-09-16 15:13:46) [GET]
2010-09-16T15:13:46.677023+00:00 app[web.1]: Rendering template within layouts/application
2010-09-16T15:13:46.677902+00:00 app[web.1]: Rendering post/list
2010-09-16T15:13:46.678990+00:00 app[web.1]: Rendered includes/_header (0.1ms)
2010-09-16T15:13:46.698234+00:00 app[web.1]: Completed in 74ms (View: 31, DB: 40) | 200 OK [http://myapp.heroku.com/]
2010-09-16T15:13:46.723498+00:00 heroku[router]: at=info method=GET path="/posts" host=myapp.herokuapp.com" fwd="204.204.204.204" dyno=web.1 connect=1ms service=18ms status=200 bytes=975
2010-09-16T15:13:47.893472+00:00 app[worker.1]: 2 jobs processed at 16.6761 j/s, 0 failed ...
```

Each line has

1. a timestamp
1. a source (where the log message came from, either your own 'app' or 'heroku')
1. a dyno ('web', 'worker', or 'router'), and
1. the message itself

We want to create a data type called `HerokuLog` which stores this information.

First thing we need to do is write datatypes that can store each component
piece, write parsers for these individual types, then combine these parsers to
create the overall `HerokuLog` parser.

Let's go ahead and import the `megaparsec` library and set up the parser.

```haskell
-- This is stuff we'll write later that we're going to export.
module HerokuLog
  ( herokuLog
  , parse
  , parseErrorPretty
  ) where

import Data.Time
import Text.Megaparsec
import Text.Megaparsec.Lexer (integer)

type Parser = Parsec Dec String
```

This sets up a parser with default error handling capabilities (`Dec`) which
parses `String`s.

#### Timestamp

Let's set about parsing the timestamp. The syntax is very easy to follow, so
I'll explain by example. We want to parse something of the form
`2010-09-16T15:13:46.677020+00:00`, i.e.
`[year]-[month]-[day]T[hour]:[minute]:[second].[millisecond]+[diff]`. For
simplicity, we're not going to care too much about the 'millisecond' and 'diff'
components, we'll just focus on parsing the information before that. Our `Timestamp`
data type is just going to be a synonym for the `LocalTime` type found in the `time`
package.

```haskell
type Timestamp = LocalTime

timestamp :: Parser Timestamp
timestamp = do
  -- parse exactly four digit characters
  yyyy <- count 4 digitChar
  -- parse a hyphen
  char '-'
  mm <- count 2 digitChar
  char '-'
  dd <- count 2 digitChar
  char 'T'
  hh <- count 2 digitChar
  char ':'
  mm' <- count 2 digitChar
  char ':'
  ss <- count 2 digitChar
  -- We don't care about the next few chars, but we still
  -- need to consume them.
  char '.' >> count 6 digitChar
  char '+' >> count 2 digitChar
  char ':' >> count 2 digitChar
  -- Finally we package everything up into a LocalTime type
  -- from the 'time' package. You can read the docs for more
  -- information on how to use the library.
  return $ LocalTime
    { localDay = fromGregorian (read yyyy) (read mm) (read dd)
    , localTimeOfDay = TimeOfDay (read hh) (read mm') (read ss)
    }
```

This is quite long, but reads well. We can very explicitly tell the parser
exactly what to parse and when, before packaging it up into a data type to
return.

#### Source

This one is easy. We just need to check if we see 'app' or 'heroku', otherwise
the parse fails.

```haskell
data Source = App | Heroku
  deriving Show

source :: Parser Source
source = app <|> heroku
  where
    app = do
      string "app"
      return App

    heroku = do
      string "heroku"
      return Heroku
```

The `<|>` operator will try the first parser first, and if that fails, try the
second. The first parser we have, called `app`, will check for the string 'app'
and return `App` if that succeeds. The second parser, `heroku`, will check for
the string 'heroku' and return `Heroku` if that succeeds. Easy!

#### Dyno

This one follows a similar pattern to above, except we need to know the number
of the dyno, as well as the name (except in the case of the router dyno)

```haskell
data Dyno =
    Worker Integer
  | Web Integer
  | Router
  deriving Show

dyno :: Parser Dyno
dyno = worker <|> web <|> router
  where
    worker = do
      string "worker"
      char '.'
      n <- integer
      return $ Worker n

    web = do
      string "web"
      char '.'
      n <- integer
      return $ Web n

    router = do
      string "router"
      return Router
```

#### Message

This one doesn't really require any work, parsing a string to a string is trivial.
We give a type synonym for clarity.

```haskell
type Message = String
```

#### Heroku Log

First, we need to be able to parse a single line of the log file, then we can
just repeatedly apply the single line parser to parse the whole file. The `HerokuLogLine`
data type is just a combination of all the types we had before:

```haskell
data HerokuLogLine = HerokuLogLine
  { hlTimestamp :: Timestamp
  , hlSource    :: Source
  , hlDyno      :: Dyno
  , hlMessage   :: Message
  } deriving Show
```

The parser is just a simple combination of the parsers we wrote beforehand.

```haskell
herokuLogLine :: Parser HerokuLogLine
herokuLogLine = do
  time <- timestamp
  space
  srcName <- source
  char '['
  dynoName <- dyno
  char ']'
  string ": "
  -- we need 'printChar' here so we don't end up consuming a
  -- newline character by accident.
  msg <- many printChar
  return $ HerokuLogLine time srcName dynoName msg
```

Now we just need to finish it off by writing a parser that can parse the whole
file at once. The whole log file is really just a list of `HerokuLogLine`, so
that's what we'll call it.

```haskell
type HerokuLog = [HerokuLogLine]

herokuLog :: Parser HerokuLog
herokuLog = many $ herokuLogLine <* optional eol
```

This will repeatedly parse the lines of the file until it reaches the end.

To test these, import the module into ghci with `:l HerokuLog` and run
`parseTest [parser] [string]` (e.g. `parseTest herokuLogLine
"2010-09-16T15:13:46.677902+00:00 app[web.1]: Rendering post/list"`) Here's the
main module that imports and uses these parsers.

```haskell
module Main where

import HerokuLog

main :: IO ()
main = do
  logFile <- readFile "resources/heroku.log"
  case parse herokuLog "" logFile of
    Left err   -> putStr $ parseErrorPretty err
    Right logs -> mapM_ print logs
```

I've stored my sample log in a folder called `resources` called `heroku.log`.
Feel free to do the same.

## Haskell in the wild

I've often wondered to myself what the best way to sell Haskell is. A lot of the
theoretical details of the language interest me greatly, and so I often find
myself selling it by appealing to such details. I can appreciate that this may
not be true of everyone else. Before adopting a new language, there's probably
only one thing you really want to know: will I be more productive using this
language?

There are a lot of things that factor into productivity in a language. You want
to be able to express as much ideas as possible in the clearest way possible.
You don't want bugs. You want to be able to refactor sections of your code
quickly, easily, and without breaking everything. Hopefully I can demonstrate
that Haskell can do all of these things.

To whet your appetite, consider the following application ([source](http://haskell-servant.github.io/posts/2015-08-05-content-types.html))

```haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Codec.Picture
import           Data.Proxy
import           Network.Wai
import           Network.Wai.Handler.Warp

import           Servant
import           Servant.JuicyPixels

type FORMATS = '[BMP, GIF, JPEG 50, PNG, TIFF, RADIANCE, TGA]

type ConversionApi
   = ReqBody FORMATS DynamicImage
  :> Post FORMATS DynamicImage

conversion :: Application
conversion = serve (Proxy :: Proxy ConversionApi) server
  where server = return

main :: IO ()
main = run 3000 conversion
```

Those 25 lines of code define a full web-based image-converter. You post an
image in any one of the formats listed in the `FORMATS` type (with appropriate
headers), and it will spit back the image converted to some other specified
format.

The concision is mind-boggling. As amazing an example as this is, it would be
silly to be sold on concision alone. However, concision is a very important
factor in the clear expression of ideas.

### Handling Failure

Consider the function `head :: [a] -> a` exported by Prelude. The type signature
for this function says "give me a list of `a`s and I'll give you an `a`". But
this is wrong, clearly, since you probably know `head []` throws an exception:
`Prelude.head: empty list`. In fact, there is no meaningful value that head
could possibly return if you give it an empty list.

Pure functions that cannot return a value for some subset of inputs (usually
called partial functions) are not uncommon. We need a way to ensure that any
pure function we write always returns a value for any input we give it.

The easiest way to do this is to write:

```haskell
headMaybe :: [a] -> Maybe a
headMaybe []    = Nothing
headMaybe (x:_) = Just x
```

but this has a few problems. Suppose we have a sequence of actions that all
return `Maybe a`, for some type `a`.

```haskell
... = do
  xs <- lookup key hashmap
  n  <- headMaybe xs
  return (x + 1)
```

Suppose we run this computation and we get `Nothing`. Why? Did the lookup fail?
Did the headMaybe function fail? There's no way to tell. For proper error
reporting, `Maybe` doesn't fit the bill.

Suppose instead we use the `Either` type.

```haskell
data CustomException
  = ...
  | EmptyListException
  | ...

headE :: [a] -> Either CustomException a
headE []    = Left EmptyListException
headE (x:_) = Right x
```

This is much better! Now if we have a similar computation to the above (assuming
lookup also returns `Either`), if it fails we'll get `Left [specific
exception]`, so we'll be able to figure out why it failed.

However, there are still problems with this. Suppose you have two separate
exception types, `CustomException` and `OtherException`. It turns out we can't
nicely compose these two exceptions in the same `do` block. So perhaps we need a
better solution.

### The MonadThrow typeclass

```haskell
import Control.Exception.Safe (throw)

instance Show CustomException where
  show EmptyListException = "Cannot take head of empty list!"

tryHead :: MonadThrow m => [a] -> m a
tryHead []    = throw EmptyListException
tryHead (x:_) = return x
```

With the `MonadThrow` typeclass, we regain composability but there's no longer a
good way to tell what errors can possibly be thrown from a function just by
looking at the type signature.

```haskell
λ> tryHead [] :: IO Int
*** Exception: EmptyListException

λ> tryHead [] :: Maybe Int
Nothing

λ> tryHead [] :: Either E.SomeException Int
Left EmptyListException
```

There is always the case that you end up using some functions that return `Maybe`
or `Either`. If you want to use these functions in `MonadThrow` code, you can
use case analysis, or you could define some wrapper functions, e.g.

```haskell
infixr 0 ?#
(?#) :: (MonadThrow m, Exception e) => Maybe a -> e -> m a
Nothing ?# e = throw e
Just x  ?# _ = return x

... = do
  xs <- lookup key hashmap ?# KeyNotFound key
  n  <- tryHead xs
  return (x + 1)
```

### The Lensed-Reader pattern

Coming Soon&trade;

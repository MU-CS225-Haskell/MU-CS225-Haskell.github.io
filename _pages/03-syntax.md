---
layout: page
title: Syntax
permalink: /syntax/
---

This will be a quick specification for common syntactic constructs in Haskell,
including some more complicated ones that are not part of the standard langauge
specification. It may be a good idea to reference this page every now and again
if you see something you're unfamiliar with.

Nothing here is in any particular order, so be sure to flip between sections if
you see things you don't understand yet. Remember, this is a *reference*, not a
guide that leads you from zero to Haskell.

___

<br/>

<h3>Contents</h3> [//]:# (This ensures that the TOC doesn't include this header)

* TOC
{:toc}

# Basic Constructs

## Comments

`--` precedes a single line comment, `{-  -}` defines a multiline comment. E.g.

```haskell
-- Single line

{-
Multiline.
-}

{-
 - Often multiline comments
 - are styled like this.
 -}
```

In my opinion, you should only use single line comments, even for multiple
lines.

Another way to comment, which is a bit different, is to use literate Haskell
(extension `.lhs` instead of `.hs`). In `.lhs` files, comments are first class,
code must be tagged, which is the opposite to `.hs` files. For example:

```lhaskell
This is a comment. No tags needed.
First we'll import 'filterM' from the Control.Monad module.

> import Control.Monad (filterM)

Now we can use it in this function:

> powerset :: [a] -> [[a]]
> powerset = filterM (const [False, True])

... and then try it out in the main function. Notice you can
still use regular comments in code blocks if you want to.

> main = print $ powerset [1,2]  -- This should return [[], [2], [1], [1,2]]
```

This may seem like something you'll never need to do, but actually when writing
libraries or executable tutorials (where you're writing more comments than
code), it makes a lot of sense to use `.lhs` files.

## `where` and `let`

These expressions allow you to define anonymous values or functions that only
exist within the scope of the original function. Their semantics differ somewhat
but we don't really need to care about that right now.

For example (you may want to reference the section on
[pattern matching on lists](#pattern-matching-on-lists)):

```haskell
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) =
  let smaller = filter (< x) xs
      bigger = filter (>= x) xs
  in smaller ++ [x] ++ bigger
```

`smaller` and `bigger` are now just synonyms for each of those filter
expressions.

Similarly, we could have done this:

```haskell
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = smaller ++ [x] ++ bigger
  where smaller = filter (< x) xs
        bigger = filter (>= x) xs
```

This means exactly the same thing. Make sure that everything after `let` or
`where` is indented the same amount! E.g., this won't compile:

```haskell
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = smaller ++ [x] ++ bigger
  where smaller = filter (< x) xs
         bigger = filter (>= x) xs   -- compile error
```

So be careful of that. Also, `in` has to be indented the same amount as `let`.

Often defining subfunctions is either very helpful or even necessary to define
specific functions while retaining any reasonable degree of clarity. Consider
this function, which calculates the probability that any two people in a room
will share a birthday, given `n` people in the room.

```haskell
birthday :: Int -> Double
birthday n = 1 - fromRational (birthday' n)
  where birthday' n
          | n <= 1    = 1
          | otherwise = ((365 - n + 1) % 365) * birthday' (n - 1)
```

Take a look at the
[Data.Ratio](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Ratio.html)
module to see how this library works. I'm using it to mitigate the imprecision
caused by repeated floating point multiplication.

This function lends itself incredibly well to a recursive formula; the only
nitpick is that we're asked for the probability that two people share a
birthday, however it's far more convenient to work out the probability that *no*
two people share a birthday. Thankfully, we can define a subfunction to do just
that, and set the `birthday` function to call one minus the result of that
function.


## Pattern Matching and Case Expressions

### Pattern Matching

Let's define a completely useless function, for the purpose of example:

```haskell
equals17 :: Int -> Int
equals17 17 = True
equals17 x  = False
```

This checks if a number is 17 and returns True if it is, otherwise it returns
False. We'd say we're matching on the pattern '17'. In the second part, we're
matching on the pattern `x`, which means "match anything, and call it `x`".

Since we aren't particularly interested in the actual value of what we matched
(we don't use `x` anywhere), we can write it like this:

```haskell
equals17 :: Int -> Int
equals17 17 = True
equals17 _  = False
```

This is called wildcard matching.

Here's another example:

```haskell
-- Using the Natural type here ensures that if a negative number is passed to
-- fib, an error is thrown rather than the function infinitely recursing. Not
-- the best solution, but better than nothing.
fib :: Natural -> Natural
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

This corresponds exactly with the mathematical definition of the Fibonacci
sequence (see [here]({{ site.url }}/2016/07/29/getting-started.html#recursion)
for a discussion on recursion). The first two terms are 0 and 1, and every other
term is just the sum of the previous two.

Pattern matching is a very powerful concept, allowing you to dig deep into
complicated nested data structures with ease.

Let's do something a bit more complicated to show off its power. Say we want to
pull the right node from the left subtree of a binary tree, or if it doesn't
exist, the leftmost node of that tree. If we get an empty tree, just return that
tree, since that will technically be the leftmost subtree.

Graphically:

```
                Node
               /
           Node
          /    \
         __     here!
               /    \
              __    __
```

... or if that doesn't exist:

```
                Node
               /    \
           Node     __
          /    \
       ...     Nil
       /
     here!
    /    \
  Nil    __
```

Let's define the data structure, then pattern match on it to get the nodes we
want (be sure to reference the section on [`where` and `let`](#where-and-let))

```haskell
module Trees where

-- Defines a recursive binary tree data structure which holds values of type 'a'
data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving Show

-- getNode takes a tree and returns a tree
getNode :: Tree a -> Tree a

-- First, match the empty tree
getNode Nil = Nil

-- Now match for the right subtree in the left subtree of the supplied tree
getNode (Node _ (Node _ _ subtree) _) = subtree

-- If this match fails (i.e., that node doesn't exist), we get the leftmost node
getNode t = getLeftmost t
  where getLeftmost (Node a Nil subtree) = Node a Nil subtree
        getLeftmost (Node _ l _)         = getLeftmost l
```

Cool! Feel free to try this code out if you like.

### Case Expressions

The pattern matching we see above is just syntactic sugar for the more general
**case** expression. Here are the same functions written using a case
expression.

```haskell
equals17 :: Int -> Int
equals17 x =
  case x of
    17 -> True
    _  -> False

getNode :: Tree a -> Tree a
getNode t =
  case t of
    Nil                         -> Nil
    Node _ (Node _ _ subtree) _ -> subtree
    t                           -> getLeftmost t
      where getLeftmost t =
        case t of
          Node a Nil subtree -> Node a Nil subtree
          Node _ l _         -> getLeftmost l
```

In this case (hehe), these functions aren't anywhere near as readable as when
we defined them using regular pattern matching. However, since case expressions
are more general than pattern matching (and so strictly more powerful), they can
pattern match on more complicated structures which may need *views* in order to
dig into that structure. For example:

```haskell
func :: Thing -> OtherThing
func x =
  case view x of
    SomeValue      -> ...
    SomeOtherValue -> ...
```

Here, `view` is some function that allows you to see the structure of something
you may not necessarily be able to directly pattern match on. This is pretty
common when you're looking at `Seq`, `Text` or `ByteString`. These are types you
cannot match on explicitly since the internal structure is hidden, so usually
you'll do something like this:

```haskell
-- Example with ByteStrings
naiveLength :: ByteString -> Int
naiveLength x =
  case uncons x of
    Nothing      -> 0  -- Empty ByteString
    Just (_, xs) -> 1 + naiveLength xs

-- Example with Seq
sumRightToLeft :: Seq Int -> Int
sumRightToLeft s =
  case viewr s of
    (xs >: x) -> x + sumRightToLeft xs
    EmptyR    -> 0
```

In the first function, `uncons` is the view that we get into the structure of
the ByteString. If we uncons (i.e. pop the first element off the ByteString) and
get Nothing, the ByteString was empty, and we can say the length is 0. If we
get something, we discard that something, add one to the length and process
the rest of the list. The second function should make sense without much
explanation.

Another way of doing this, which is a bit clearer (certainly less verbose), is
with ViewPatterns.

```haskell
{-# LANGUAGE ViewPatterns #-}

naiveLength :: ByteString -> Int
naiveLength (uncons -> Nothing)      = 0
naiveLength (uncons -> Just (_, xs)) = 1 + naiveLength xs

sumRightToLeft :: Seq Int -> Int
sumRightToLeft (viewr -> (xs >: x)) = x + sumRightToLeft xs
sumRightToLeft (viewr -> EmptyR)    = 0
```

### Pattern Matching on Lists

Let's define a `head` function (already in Prelude) which takes a list and
returns the first element in that list. We'll do so safely using `Maybe`, since
there is always the chance that our list won't have any elements in it, in which
case we have to return Nothing.

```haskell
head :: [a] -> Maybe a
head []    = Nothing
head (x:_) = Just x
```

... and we'll also define `tail` (also already in Prelude). Note that this
doesn't need to return a `Maybe`, since if the list is empty, we can just return
the empty list.

```haskell
tail :: [a] -> [a]
tail [] = []
tail (_:xs) = xs
```

The pattern we're using here is a way of splitting a list into the head of the
list, `x`, and the rest of the list, or the tail, `xs`. This is an extremely
common way to process lists in Haskell, you'll see it a *lot*.

#### As-patterns

What if you need to reference the entire list in a pattern like the one above?
You could always just write `(x:xs)`, but if you need to refer to it a few
times, this can get tiresome. Instead you can use an *as*-pattern:

```haskell
dupeFirst :: [a] -> [a]
dupeFirst []        = []
dupeFirst xss@(x:_) = x:xss
```

This will duplicate the first element in the list and prepend it to the list.
Here, `xss` refers to the whole list, including the element `x`. Of course, if
you'd rather not use this pattern, you can just write

```haskell
dupeFirst :: [a] -> [a]
dupeFirst []     = []
dupeFirst (x:xs) = x : x : xs
```

It's up to you which one you prefer, but I would lean towards using the first
one, since it makes it clear that you intend to use the entire list.

The function that's being used in the pattern, `:`, is called the *cons*
operator. It takes a value of type `a` and a list of `a`, and prepends the value
to the list. E.g. `3 : [4, 5] == [3, 4, 5]`. Be warned though, even though we're
using a function in a pattern here, in general you can't do this. See this
[StackOverflow question](http://stackoverflow.com/questions/31106484/pattern-matching-data-sequence-like-lists)
for an example of some guy who tried.

### Lazy Pattern Matching

You may, in extremely rare cases, see a `~` before some pattern to be matched.
This utilises lazy pattern matching (see
[here]({{ site.url }}/2016/07/29/getting-started.html#laziness) for more
discussion on that). You should never need to use it, but for completeness I
felt it should at least be mentioned.

## Guards

Guards are for when we want to check for boolean conditions rather than exact
patterns. We'll take the definition for `filter` as an example:

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) -- Note that there's no '=' sign here
  | p x       = x : filter p xs
  | otherwise =     filter p xs
```

Here, `p` is a predicate, i.e. a function that takes a value and returns true or
false. A guarded expression evaluates everything between `|` and `=`; if it's
true, we execute everything after `=`, otherwise we drop to the next guard.
Example usage: `filter even [1,2,3,4,5,6] == [2,4,6]`. Indeed, we can also look
at the definition of `even`:

```haskell
even :: Integral a => a -> Bool
even n
  | n `rem` 2 == 0 = True  -- using backticks makes the function infix
  | otherwise      = False
```

Simple enough. `otherwise` isn't anything special, it's just an alias for
`True`. No really, this is the definition in the Standard Prelude:

```haskell
otherwise = True
```

It just makes guarded expressions a little easier to read.

## Ranges

Haskell does ranges pretty well. If you want to have a list of numbers from 1 to
10,000, just write `[1..10000]`. This is syntactic sugar over the function
`enumFromTo`.

You can also specify a step: `[1,3..10000]` will give all odd numbers from 1 to
10,000 (not including 10,000 of course).

You should note that specifying a step only works for arithmetic sequences, you
can't expect `[1,2..16]` to give you `[1,2,4,8,16]`, or indeed any other
geometric series. As a result, you can only supply one parameter for the step.
It's actually impossible, given any starting sequence, to uniquely determine
the geometric series that follows, since there are infinitely many.

## List Comprehensions

Like Python's list comprehensions? They were inspired by Haskell!

If we have two lists, `xs = [1,2,3]` and `ys = [4,5,6]`, then
`[x + y | x <- xs, y <- ys]` gives `[5,6,7,6,7,8,7,8,9]`, which adds each
combination of elements in both lists together. It's basically just mathematical
set notation, but in a programming language.

Here's a `toUpperCase` function using list comprehensions:

```haskell
toUpperCase s = [toUpper c | c <- s]
```

> Note: `Strings` are really just `[Char]`, so we can perform the same patterns
on strings as we can on lists.

`toUpper` is defined in Data.Char, and converts an ASCII character to upper
case. Note that this really isn't the way you should write this pattern, the
following would be more idiomatic Haskell.

```haskell
toUpperCase = map toUpper
```

In general, any pattern of the form `[f x | x <- xs]` can be rewritten as
`map f xs`.

I'm not a huge fan of list comprehensions, concepts can usually be expressed
better using other methods, but they certainly can be quite handy if the use
case is right. For example, try wrapping your head around this:

```haskell
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [y:ys | y:xs' <- tails xs, ys <- combinations (n - 1) xs']
```

Fun exercise if you don't mind tearing your hair out for a bit.

## Tuples

Just like in any other language with tuples, but they're more restricted by the
type system. This is not necessarily a bad thing though.

Tuples are denoted by round brackets, e.g. `(1,2)` is a 2-tuple, and has type
`Num a => (a, a)`, however `(1,2,3)` is a 3-tuple and has type
`Num a => (a, a, a)`. This is a completely different type to the 2-tuple, and
it's important to know that when using them. The other feature is that tuples
do not require each component to be the same type, i.e.
`("hello", Left False, id)` is a valid tuple of type
`(String, Either Bool b, a -> a)`.

Some handy functions when using 2-tuples are `curry` and `uncurry`, as well as
`fst` and `snd`. Functions like `zip` return lists of tuples, and the module
Control.Arrow has lots of support for using tuples in interesting (albeit
fairly complex) ways.

# More Complex Constructs

## Syntax in Type Signatures

Let's pick apart what the type signature of a function means, syntactically:

```haskell
foo :: (Class a) => a -> f a -> String -> Bool
```

- `foo` is the function name
- `::` can be read 'has type'
- everything between `::` and `=>` are the typeclass constraints
- Everything between the `->` arrows are potential arguments for the function
(except the last one, which is `Bool` in this case).
- `a` is a type constrained by the typeclass `Class`, that is, whatever `a`
happens to be *must* have an instance of `Class` defined for it.
- `f` is a type constructor which is not constrained by any typeclass
- `String` and `Bool` are just concrete, specific types

All of this is explained in better detail [elsewhere]({{ site.url }}/2016/07/29/typeclasses.html),
this is just meant to tell you what everything is, not explain it.

Haskell does not make any distinction between the arguments of a function and
the return type. This is because if you partially apply the function, you
actually return another function. E.g., if I take the function `+` and only
supply it one argument, say the number 3, then I get back a *function* `(+3)`,
which now only takes a single number and adds 3 to it.

```haskell
GHCi> :t (+)
Num a => a -> a -> a

GHCi> :t (+ 3)
Num a => a -> a

GHCi> :t (2 + 3)
Num a => a
```

## Monadic `do`-notation

**On the assumption that you understand monads**, I'll explain the meaning of
`do`-notation.

Any given `do` block is just syntactic sugar over the `>>=` and `>>` operators.
For example, this function asks for your first and last name, then greets you:

```haskell
name :: IO ()
name = do
  putStr "What is your first name? "
  firstName <- getLine
  putStr "And your last name? "
  lastName <- getLine
  let fullName = firstName ++ " " ++ lastName
  putStrLn $ "Pleased to meet you, " ++ fullName ++ "!"
```

This roughly translates to the following code:

```haskell
name :: IO ()
name =
  putStr "What is your first name? " >>
  getLine >>= \firstName ->
  putStr "And your last name? " >>
  getLine >>= \lastName ->
  let fullName = firstName ++ " " ++ lastName
  in putStrLn ("Pleased to meet you, " ++ fullName ++ "!")
```

Note that the lambdas which name the 'variables' in this chain actually get
threaded through the whole function, meaning they're kept in scope throughout.

This is much worse to look at in this case, but be careful not to overuse
`do`-notation if using `>>=` and `>>` would be more clear.

For example **do not** write:

```haskell
main = do
  text <- readFile "path/to/file"
  writeFile "path/to/other" text
```

... or

```haskell
main = do
  contents <- readFile "path/to/file"
  let result = process contents
  return result
```

Instead, opt for

```haskell
main = readFile "path/to/file" >>= writeFile "path/to/other"
```

... and

```haskell
main = process <$> readFile "path/to/file"
```

Also, to be clear, `do`-blocks need not *evaluate* in the order you write the
statements, however the *effects* performed by the monad will be sequenced in
order. In the first example above, `fullName` was bound before the last call to
`putStrLn`, however `fullName` may only be evaluated *after* that call (i.e.
when the value is needed).

___

<br/>

That's most of the core syntactic features of Haskell, I'll be sure to add more
here if there's demand for it. I haven't touched much on language pragmas,
since they often (as we've seen) add nonstandard language features, so I'd be
here all day explaining them.

---
layout: page
title: Assignments
permalink: /assignments/
---

<h3>Contents</h3> [//]:# (This ensures that the TOC doesn't include this header)

* TOC
{:toc}

### Submissions

I'm not picky on how you'd like to submit the homeworks, there's no official
process, just get the code to me somehow and I'll correct it. You can create a
Gist on GitHub, send via pastebin, send me a zipped folder, whatever you like.
Considering most of you will be using stack, zipping the files up and emailing
it to me is probably the easiest option for you.

Each assignment is due one week after its assigned. You'll receive written
feedback on each assignment, suggestions on how your code could be improved, and
obviously a percentage mark. I'll be marking the assignments based on a number
of factors, one of the most important (and most overlooked) of which is
*readability*. This means writing clear code, but also means adding comments to
explain what each thing does, and using proper spacing and proper indentation
(which reminds me: *NO TABS*, use spaces instead).

## Assignment 1

> The goal of this assignment is to get a good handle on recursion, abstract
data types, and pattern matching.

- **Q1**: Complete Exercise 5, found [here](http://www.cis.upenn.edu/~cis194/spring13/hw/01-intro.pdf) (PDF).
- **Q2**: Create a binary search tree data type that holds `Int`s and implement
functions that will traverse the tree in a certain order and collect the results
into a list (details below)

Sample binary tree:

```
   3
 /   \
1     5
 \   / \
  2 4   6
```

Notice that at any given node, every element to the right is larger, and every
element to the left is smaller. This must be satisfied. For example, this is
*not* a valid binary search tree:

```
   3
 /   \
2     5
 \   / \
  1 4   6
```

... since 1 is to the right of 2, whereas it should be to the left.

This is a template for the program you should write.

```haskell
data IntTree = ...
  deriving Show  -- Allows us to print the tree to the console

-- Traverse the tree in the regular binary tree order
-- i.e. for the sample tree, should return [1,2,3,4,5,6].
inOrder :: IntTree -> [Int]

-- Traverse the tree from top to bottom, printing each row from left to right
-- i.e. for the sample tree, should return [3,1,5,2,4,6].
topDown :: IntTree -> [Int]

-- Same as above but, start from the bottom and work up, printing left to right
-- i.e. for the sample tree, should return [2,4,6,1,5,3].
bottomUp :: IntTree -> [Int]
```

### Solution

#### **Tower of Hanoi**

Pretty much everyone who submitted got the solution for this, so well done
there:

```haskell
type Peg = String
type Move = (Peg, Peg)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _   _   _   = []
hanoi n src dst tmp = hanoi (n - 1) src tmp dst
                   ++ [(src,dst)]
                   ++ hanoi (n - 1) tmp dst src
```

In the zero disk case, the tower is already solved vacuously. In the case where
you have `n` disks, you use the solution for `n - 1` disks to move all but the
largest disk onto the `tmp` peg, then move the large disk from `src` to `dst`,
then use the same `n - 1` solution to move the tower from `tmp` to `dst`.

#### **Binary Tree**

Your defintion of the `IntTree` should have looked something like this:

```haskell
data IntTree = Nil | Node Int IntTree IntTree
```

You could have had a slightly different representation but as long as it can be
made to behave like a binary tree then that's fine.

Most people got the `inOrder` traversal:

```haskell
inOrder :: IntTree -> [Int]
inOrder Nil          = []
inOrder (Node a l r) = inOrder l ++ [a] ++ inOrder r
```

Few managed the `topDown` and `bottomUp` solutions, so we'll go through a few
potential candidates.

One of the more obvious ways to do this is to assign to each node a level, then
traverse this list picking out the values in order of level.

```haskell
assignLevel :: IntTree -> [(Int, Int)]
assignLevel = assignLevel' 0
  where
    assignLevel' _ Nil          = []
    assignLevel' n (Node a l r) = assignLevel' (n + 1) l
                               ++ [(a,n)]
                               ++ assignLevel' (n + 1) r
```

So we set `assignLevel` to call another function `assignLevel'` with the initial
level set to zero, then it traverses the tree, pairing each value with the level
it's on.

We can then proceed in one of two ways:

```haskell
-- First way
topDown tree = map fst $ sortOn snd $ assignLevel tree

-- Second way
topDown tree = [value | level <- [0 .. maxLevel]
                      , (value,level') <- assignLevel tree
                      , level == l]
  where
    maxLevel = maximum $ map snd $ assignLevel tree
```

In the first way we simply sort the list, using the property that Haskell
sorting is *safe* meaning that `sortOn even [1,2,3,4,5,6,7,8]` *always* sorts
to `[1,3,5,7,2,4,6,8]` (i.e. under this sort, 1, 3, 5, and 7 are identical under
the sort, but their positions relative to each other don't get shuffled). An
unsafe sort could give `[1,5,7,3,8,4,2,6]`, which is sorted by the property we
wanted, but the numbers that were identical under the sort have been shuffled.

In `assignLevel`, note that we traverse the tree in order. Thus the list of
integer pairs we get will be already sorted by value. If we then sort them by
level (`sortOn snd`), we still have that each *level* is sorted, so the tree
will be traversed from left to right per level.

The second function does pretty much the same thing, and also relies on the fact
that the `assignLevel` function traverses the tree in order. It iterates through
each level from 0 to whatever the maximum level is, and extracts the
`(value,level')` only if the level of the node we're at (`level'`) is the same
as current level we're on in the first iterator (`level`).

Both solutions solve the problem quite well. The bottomUp functions are
analogous:

```haskell
-- First way
bottomUp tree = map fst $ sortOn (negate . snd) $ assignLevel tree

-- Second way
bottomUp tree = [value | level <- [maxLevel, maxLevel - 1 .. 0]
                       , (value,level') <- assignLevel tree
                       , level == l]
  where
    maxLevel = maximum $ map snd $ assignLevel tree
```

However, there is a way to solve this problem without needing to assign an
explicit numeric level to the tree:

```haskell
topDown :: IntTree -> [Int]
topDown tree = go [tree]
  where
    go               []  = []
    go (Nil        : ns) = go ns
    go (Node a l r : ns) = a : go (ns ++ [l,r])

bottomUp :: IntTree -> [Int]
bottomUp tree = go [tree]
  where
    go               []  = []
    go (Nil        : ns) = go ns
    go (Node a l r : ns) = go (ns ++ [r,l]) ++ [a]
```

This works by processing a list (you can think of it as a queue in this case) of
nodes: if we see a `Node a l r` at the front of the list, we take `a`, then we
append the left and right subtrees to the list. These need to processed after
everything else since they sit one level down on the tree. If there is anything
else in the `ns` list that needs processing, it will do those first, then it
will move on to lower levels. A worked example of `topDown`:

```haskell
-- The sample tree from the beginning
sample = Node 3 (Node 1 Nil (Node 2 Nil Nil)) (Node 5 (Node 4 Nil Nil) (Node 6 Nil Nil))

topDown sample = go [sample]
               = go (Node 3 l r : [])
               = 3 : go ([] ++ [l,r])
               = 3 : go [l,r]
               = 3 : go (Node 1 Nil r' : [r])
               = 3 : 1 : go ([r] ++ [Nil,r'])
               = 3 : 1 : go [r,Nil,r']
               = 3 : 1 : go (Node 5 l' r'' : [Nil,r'])
               = 3 : 1 : 5 : go [Nil,r',l',r'']
               = 3 : 1 : 5 : go (Nil : [r',l',r''])
               = 3 : 1 : 5 : go [r',l',r'']
               = 3 : 1 : 5 : go (Node 2 Nil Nil : [l',r''])
               = 3 : 1 : 5 : 2 : go [l',r'',Nil,Nil]
               = 3 : 1 : 5 : 2 : go (Node 4 Nil Nil : [r'',Nil,Nil])
               = 3 : 1 : 5 : 2 : 4 : go [r'',Nil,Nil,Nil,Nil]
               = 3 : 1 : 5 : 2 : 4 : go (Node 6 Nil Nil : [Nil,Nil,Nil,Nil])
               = 3 : 1 : 5 : 2 : 4 : 6 : go [Nil,Nil,Nil,Nil,Nil,Nil]
               = ...
               = 3 : 1 : 5 : 2 : 4 : 6 : go []
               = 3 : 1 : 5 : 2 : 4 : 6 : []
               = [3,1,5,2,4,6]
```

Phew! Hopefully you now have a good idea of how this works. `bottomUp` is
analogous.

BUT, this is incredibly inefficient! Appending to the end of a list in Haskell
requires traversal of the entire left list before the appending can take place.

We can fix this by rewriting the function using Haskell's Sequence type, which
supports efficient appending and concatenation of sequences:

```haskell
{-# LANGUAGE ViewPatterns #-}

import Data.Foldable (toList)
import Data.Sequence (Seq, (<|), (|>), ViewL((:<)), (><))
import qualified Data.Sequence as Seq

topDown :: IntTree -> Seq Int
topDown tree = go $ Seq.singleton tree
  where
    go (Seq.viewl -> Node a l r :< ns) = a <| go (ns |> l |> r)
    go (Seq.viewl -> Nil        :< ns) = go ns
    go (null      -> True            ) = Seq.empty

bottomUp :: IntTree -> Seq Int
bottomUp tree = go $ Seq.singleton tree
  where
    go (Seq.viewl -> Node a l r :< ns) = go (ns |> r |> l) |> a
    go (Seq.viewl -> Nil        :< ns) = go ns
    go (null      -> True            ) = Seq.empty
```

This does effectively the same thing but much more efficiently (see the docs
on Haskell sequences and also ViewPatterns).

## Assignment 2

Some of these questions are quite tough so be sure to work together, use any
resources you can find, do research, etc. Also, please do email me if something
is unclear!

### Recursive Patterns

Before attempting these questions, mess around with the functions `foldl` and
`foldr`, get a feel for how they work. Also be sure to have a look at
[this](https://wiki.haskell.org/Fold) page.

Using the following data type

```haskell
data Bit = Zero | One
  deriving Show
```

answer the following questions:

1. Implement an `xor :: Bit -> Bit -> Bit` function
which computes the exclusive-or of two bits. Then implement a function that
takes a list of `Bit`s and returns `One` if and only if there are an odd number
of `One`s in the list. NOTE: You cannot use explicit recursion to answer this
question! You must implement the function using either `foldl` or `foldr`.
2. Implement `filter` and `map` using `foldr`. As a hint for `map`, note that
`foldr (:) [] xs == xs`, for all lists.

### Typeclasses

Recall that a monoid is a 3-tuple $$ (M, \cdot, e) $$, where $$ M $$ is a set,
$$ \cdot $$ is an associative binary operator, and $$ e \in M $$ is an element
satisfying $$ e \cdot x = x \cdot e = x $$, for all $$ x \in M $$.

1. Explain why $$ \mathbb{N} = \{1,2,3,...\} $$ is *not* a monoid under
addition.
2. Note that $$ \text{End}(A) $$ for some set $$ A $$ represents the set of all
endomorphisms on the set $$ A $$, i.e. the set of all functions from $$ A $$ to
$$ A $$. Can $$ \text{End}(A) $$ form a monoid? If not, why not? And if so, what
is the associative binary operator? What is the identity?
3. Implement a useful `Functor` instance for the `Failable` type below. Recall
that the `Functor` typeclass represents the class of all types that can behave
like containers (in a broad sense). In order to implement a `Functor` instance,
we need only define the behaviour of a single function, `fmap`, which should
take any function from `a` to `b` and inject it into the functor context. The
definition of `Failable` is

```haskell
data Failable a = Failure String | OK a
```

Think of this type as a error handling type that can give us error messages
describing how the computation failed. Some example usage:

```haskell
head' :: [a] -> Failable a
head' []    = Failure "Took head of empty list."
head' (x:_) = OK x

sqrt' :: Double -> Failable Double
sqrt' n
  | n < 0     = Failure "Took sqrt of negative number"
  | otherwise = OK (sqrt n)

parseJSON :: String -> Failable JObject
parseJSON s
  | {- no parse -}         = Failure "Couldn't parse token at ..."
  | {- parsed correctly -} = OK parsedObject
```

You get the idea. We need to be able to apply functions to values wrapped in
`Failable`. Here's a template:

```haskell
instance Functor Failable where
  fmap f (Failure s) = ???
  fmap f (OK val)    = ???
```

### Solution

These were the solutions to the problems due for today:

#### **Recursive Patterns**

```haskell
data Bit = Zero | One
  deriving (Show)

xor :: Bit -> Bit -> Bit
xor Zero Zero = Zero
xor One  One  = Zero
xor _    _    = One

xorList :: [Bit] -> Bit
xorList = foldr xor Zero

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x -> if p x then (x:) else id) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []
```

Rather than explaining these (which I already did in class), it would be more
instructive to copy the code somewhere and play around with it. Try changing
some things and see what happens.

#### **Typeclasses**

1. No, since $$ 0 \notin \mathbb{N} $$, which is the identity with respect to '$$ + $$'.
2. Yes, $$ \left( \text{End}(A), \circ, \text{id}_A \right) $$ is a monoid. See
[here](https://hackage.haskell.org/package/base-4.9.0.0/docs/src/Data.Monoid.html#Endo)
for the real implementation of this monoid in Haskell.
3. See below.

```haskell
data Failable a = Failure String | OK a
  deriving Show

instance Functor Failable where
  fmap _ (Failure s) = Failure s
  fmap f (OK v)      = OK (f v)
```

This `Functor` instance is simple enough. If we try to apply a function to a
failed computation, we just don't bother computing it and return the same error
message as before, otherwise we apply the function `f` to the value inside `OK`.

## Assignment 3 (HackerRank)

Here were the solutions I had for the HackerRank lab.

### Leap Years

```haskell
main = isLeapYear <$> readLn >>= print

isLeapYear :: Integer -> Bool
isLeapYear n = (n %? 4) && (not (n %? 100) || (n %? 400))

-- | a %? b == a `isDivisibleBy` b
(%?) :: Integer -> Integer -> Bool
a %? b = a `rem` b == 0
```

I like to define `%?` as a shortcut for 'is divisible by'. Sadly, `|` is taken
so we can't use that.

If the use of `<$>` and `>>=` is confusing you, look at the `Functor` and
`Monad` typeclass definitions (note that `<$>` is an infix synonym for `fmap`)
and recall that `IO` is an instance of both `Functor` and `Monad`.

The following might be more readable (since it can (kind of) be read left to
right):

```haskell
main = print =<< isLeapYear <$> readLn
```

`=<<` is just `>>=` with its arguments flipped.

### Number Words

Everyone had good solutions to this, so I decided to opt for a different one so
you can see another way that this could be done.

```haskell
import Data.Char
import Data.List

main = w2s <$> readLn >>= print

w2s :: Int -> String
w2s n = concat $ intersperse "-" [table !! digitToInt d | d <- show n]
  where table = [ "zero", "one", "two", "three", "four", "five"
                , "six", "seven", "eight", "nine", "ten" ]
```

`show n` converts the number to a list of characters, which we then convert to
their respective numbers by indexing a lookup table. Then we intersperse hyphens
between the words and stitch it together with `concat`.

### Base Conversion

```haskell
import Control.Monad

main = do
  tcs <- readLn
  replicateM_ tcs $ do
    [n,b,b'] <- map read . words <$> getLine
    print $ unDigits $ convertBase b b' $ digits n

type Base   = Integer
type Digits = [Integer] -- always reversed

digits :: Integer -> Digits
digits = toBase 10

unDigits :: Digits -> Integer
digits = fromBase 10

fromBase :: Base -> Digits -> Integer
fromBase b ds = foldr (((*b) .) . (+)) 0 ds `quot` b

toBase :: Base -> Integer -> Digits
toBase _ 0 = []
toBase b n = let (q,r) = n `quotRem` b
             in  r : toBase b q

convertBase :: Base -> Base -> Digits -> Digits
convertBase b b' = toBase b' . fromBase b
```

Again, everyone who submitted had good answers. Note the pointfree mess inside
the `fromBase` function: `((*b) .) . (+)`. Let's expand this.

```
((*b) .) . (+) ==> \x -> ((*b) .) ((+) x)
               ==> \x -> (\f -> (*b) . f) (x+)
               ==> \x -> (\f z -> b * f z) (\y -> x + y)
               ==> \x -> (\z -> b * (z + x))
               ==> \x z -> b * (z + x)
```

So if we assume we have a number with digits $$ d_n \cdots d_0 $$ in some base
$$ b $$, we get the following procedure by folding:

$$
\begin{align}
b \cdot (0 + d_n) &= bd_n\\
\rightarrow \quad b \cdot (bd_n + d_{n - 1}) &= b^2d_n + bd_{n - 1}\\
\rightarrow \quad b \cdot (b^2d_n + bd_{n - 1} + d_{n - 2}) &= b^3d_n + b^2d_{n - 1} + bd_{n - 2}\\
&\vdots\\
b^{n + 1}d_n + \cdots& + b^2d_1 + bd_0
\end{align}
$$

Notice that we're off by a factor of $$ b $$; that's why we have to divide by
$$ b $$ at the end.

## Assignment 4

This assignment was to solve the problems located [here](https://www.schoolofhaskell.com/user/DanBurton/20-intermediate-exercises).
These are tough questions, so I'll go through a selection of them to show the
line of thinking that is required to answer them. The answers to these exercises
can be found [here](https://gist.github.com/SilverSylvester/3152cacc29d0c5a72423826faa6f86f2).

### Exercise 3

```haskell
class Fluffy f where
  furry :: (a -> b) -> f a -> f b

instance Fluffy ((->) t) where
  furry = error "todo"
```

So we have a typeclass called `Furry` which accepts a type parameter `f` and
implements a single function `furry :: (a -> b) -> f a -> f b`. How do we make
this work for `((->) t)`?

First thing to note is that `f == (->) t` in this case, so we can see what type
`furry` should have by replacing every instace of `f` with `(->) t`.

```haskell
furry :: (a -> b) -> ((->) t a) -> ((->) t b)
```

Note also that `(->) t` is really just `(t ->)`, but you can't write it like
that if the `->` operator has only one type operand. In this case, we have two so
we can rewrite this type signature with `->` infix:

```haskell
furry :: (a -> b) -> (t -> a) -> (t -> b)
```

OK, that looks better. Let's give `furry` its arguments (as we can see, it takes
two functions as arguments):

```haskell
instance Fluffy ((->) t) where
  furry f g = error "todo"
```

How do we combine `f` and `g` so that we get a function of type `t -> b`? If you
can't see the answer yet, consider the futher rewritten type signature for `furry`:

```haskell
furry :: (a -> b) -> (t -> a) -> t -> b
```

We just dropped the brackets from the last two terms. Now we can think of `furry`
as a function that takes two functions (`f` and `g`) and a value of type `t`,
and then returns a value of type `b`. How to we transform the value of type
`t` into a value of type `b`? Notice that we're effectively forced to conclude
only one thing: we apply `g` first to obtain a value of type `a`, then we apply
`f` to obtain a value of type `b`. Here's how that works:

```haskell
furry f g x = f (g x)
```

Notice that this is just the definition of function composition, so we can
rewrite this as:

```haskell
furry = (.)
```

### Exercise 7

```haskell
class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a

instance Misty [] where
  banana = error "todo"
  unicorn = error "todo"
```

For this one, we just do the same thing as with the last one: rewrite the type
signatures for the specialised instances and see what we can do from there.
Here `m == []`:

```haskell
banana :: (a -> [] b) -> [] a -> [] b
unicorn :: a -> [] a
-- which is the same as:
banana :: (a -> [b]) -> [a] -> [b]
unicorn :: a -> [a]
```

The definition for `unicorn` should be reasonably obvious. We need to take a
value of type `a` into a list. Easiest way to do that is just to put it in a list.

```haskell
unicorn x = [x]
```

`banana` is a little trickier. Using the function (let's call it `f :: (a -> [b])`)
we need to take `[a]` to `[b]`. It won't be as easy as just mapping the function
over the list, since we would end up with `[[b]]`, not `[b]`, which is what we
need. However, we could use the function `concat :: [[a]] -> [a]` to flatten it:

```haskell
banana f xs = concat (map f xs)
```

Notice that this is the same as:

```haskell
banana f xs = concatMap f xs
```

... which we can then eta-reduce further to get:


```haskell
banana = concatMap
```

### Exercise 9

```haskell
instance Misty ((->) t) where
  banana = error "todo"
  unicorn = error "todo"
```

We'll implement `unicorn` first. Rewriting the type signature, we get:

```haskell
unicorn :: a -> (t -> a)
-- which is the same as
unicorn :: a -> t -> a
```

So `unicorn` accepts two arguments of type `a` and `t` respectively and returns
a value of type `a`. Notice that there is nothing at all that we can do with the
value of type `t`. Any attempts to use it will result in a type error (try it).
So the only logical explanation is to not use that argument. As for the value of
type `a`, there's nothing we can do with that value except to simply return it.
Overall we get:

```haskell
unicorn x _ = x
```

Notice that this is the definition of the function `const`, so we can rewrite:

```haskell
unicorn = const
```

Now on to `banana`. Let's rewrite its type signature:

```haskell
banana :: (a -> t -> b) -> (t -> a) -> t -> b
```

... so assuming `banana`s arguments are `f`, `g` and `x`, we have:

```haskell
f :: a -> t -> b
g :: t -> a
x :: t
```

We eventually want a value of type `b`, so we need to use `f` to get there
(since its return type is `b`). In order to get to `b`, we must supply `f` two
values: one value of type `a` and another of type `t`. `x` is the value of type
`t` so we're done there. To get a value of type `a`, we just need to apply `g`
to `x`, i.e. `g x :: a`. Putting it all together:

```haskell
banana f g x = f (g x) x
```

### Exercise 12

```haskell
jellybean :: Misty m => m (m a) -> m a
jellybean x = error "todo"
```

In these questions, where we aren't implementing any typeclass instances, we
need to implement these functions independently of any specific types. That is
to say, we need to use `banana` and `unicorn`.

We have a value of type `m (m a)` which we need to convert to an `m a`. Let's
see how we might be able to put `banana` to use here.

We definitely can't supply `x :: m (m a)` as the first argument to `banana`, so
we'll assume it's the second argument. We also need the return type to be `m a`,
so we'll set `b = a`. We'll update the type signature accordingly:

```haskell
-- Regular type signature
banana :: (a -> m b) -> m a -> m b
-- Replacing all instances of `a` with `m a`, and `b` with `a`
banana :: (m a -> m a) -> m (m a) -> m a
```

There is only one function we can choose for `m a -> m a`, and that's `id` (the
identity function). We will then have:

```haskell
banana id :: m (m a) -> m a
```

and so

```haskell
jellybean x = banana id x
-- which is the same as
jellybean = banana id
```

### Exercise 13

```haskell
apple :: Misty m => m a -> m (a -> b) -> m b
apple x f = error "todo"
```

This looks a lot like the definition of `furry'` from exercise 6, the only
difference being the order of the arguments, and that instead of `f :: a -> b`,
we have `f :: m (a -> b)`. That is to say, we'd like to write `furry' f x` and
be done with it, but we can't. Let's write out all the relevant type signatures:

```haskell
x      :: m a
f      :: m (a -> b)
furry' :: (a -> b) -> m a -> m b
banana :: (a -> m b) -> m a -> m b
```

Since we can't use `furry'` to get to `m b`, we'll try `banana`. It may appear
at first that `x` should be the second argument of `banana`, but instead we will
assume that `f` is the second argument of `banana` and update the type signature:

```haskell
-- a == (a -> b)
\h -> banana h f :: ((a -> b) -> m b) -> m b
```

All that now remains is to produce a function of type `(a -> b) -> m b`. If we
supply `x` as the second argument to `furry'`, we get the required type signature,
so `\g -> furry' g x :: (a -> b) -> m b` will do this. Putting it all together:

```haskell
apple x f = banana (\g -> furry' g x) f
-- which is the same as
apple x = banana (\g -> furry' g x)
```

### Exercise 14

```haskell
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
```

Since we're dealing with lists, we'll use that to our advantage by splitting the
problem into the base case and recursive case.

```haskell
moppy [] _ = unicorn []
```

This should be reasonably clear. Now we'll focus on the recursive case:

```haskell
moppy (x:xs) f = error "todo"
```

Here are all the relevant type signatures:

```haskell
x      :: a
xs     :: [a]
f      :: a -> m b
furry' :: (a -> b) -> m a -> m b
apple  :: m a -> m (a -> b) -> m b

moppy  :: [a] -> (a -> m b) -> m [b]
```

As you may have guessed, `moppy xs f` is going to be the recursive call, so we
need to include this. We should also change all instances of

```haskell
x      :: a
xs     :: [a]
f      :: a -> m b
furry' :: (a -> b) -> m a -> m b
apple  :: m a -> m (a -> b) -> m b

moppy xs f :: m [b]

moppy  :: [a] -> (a -> m b) -> m [b]
```

Notice that `apple (moppy xs f) :: m ([b] -> c) -> m c`. So we need to produce
a function of type `m ([b] -> c)`, for some `c`. In particular, we should pick
`c == [b]`, which means we need `m ([b] -> [b])`. The only value we can work
with is `f x :: m b`, so we need to use `furry'` to take `m b` to `m ([b] -> [b])`.
To do this, we need to use `(:) :: b -> [b] -> [b]` with `furry'` and `f x`.
Altogether, `furry' (:) (f x) :: m ([b] -> [b])`, which means that if we supply
this to the second argument that `apple` takes, we get the desired result.

```haskell
moppy [] _     = unicorn []
moppy f (x:xs) = apple (moppy xs f) (furry' (:) (f x))
```

### Exercise 19/20

The main difficulty with these exercises is the fact that `State` contains a
function, which we can't pattern match on directly. However, we can work around
this.

```haskell
newtype State s a = State { state :: s -> (s, a) }
```

For the `Fluffy` instance, the trick is to use `let` bindings to extract the
info you need.

```haskell
furry f (State st) = error "todo"

f  :: a -> b
st :: s -> (s, a)
```

We need to return `s -> (s, b)` (wrapped in `State`). Since we're returning a
function, we can assume the existence of one of the parameters it takes (we'll
call it `s`; it also has type `s` so don't let that confuse you). Our new list
of type signatures is

```haskell
s  :: s
f  :: a -> b
st :: s -> (s, a)

st s :: (s, a)
```

Ah, now we have access to the values in the tuple through `st s`. Once we
have these values, it's just like a regular `Fluffy` instance, apply the
function `f` to the right value:

```haskell
furry f (State st) = State $ \s -> let (s', a) = st s in (s', f a)
```

The `Misty` instance is a bit more complicated again. The `unicorn` definition
is reasonably easy:

```haskell
unicorn t = State $ \s -> (s, t)
```

The `banana` instance uses the same trick to extract the tuple from the `State`:

```haskell
banana f (State st) = State $ \s -> let (s', a) = st s in ...
```

The only issue is what comes after `in` (it should have type `(s, b)`). We could
do the same as last time if `f :: a -> b`, but this time we have
`f :: a -> State s b`. Here's how you do it:

First apply `f` to `a`, which gives `f a :: State s b`. Now apply `state`, which
gives `state (f a) :: s -> (s, b)`, and finally apply `s` which gives
`state (f a) s :: (s, b)`. Altogether we have

```haskell
banana f (State st) = State $ \s -> let (s', a) = st s in state (f a) s
```

If there are any questions in here you wanted me to cover, send me an email and
I'll update the list.

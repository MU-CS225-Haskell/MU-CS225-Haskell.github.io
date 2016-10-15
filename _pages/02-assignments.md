---
layout: page
title: Assignments
permalink: /assignments/
---

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
bottomUp tree = map fst $ sortOn snd $ assignLevel tree

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
2. Implement `filter` using `foldr`, and `map` using
`foldl`. As a hint for `map`, use this implementation of reverse as inspiration:
`reverse xs = foldl (flip (:)) [] xs`

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
that in order to implement a `Functor` instance, we need only define the
behaviour of a single function, `fmap`, which should take a function and inject
it into the functor context. The definition of `Failable` is

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

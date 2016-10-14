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

...

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

Recall that a monoid is 3-tuple $$ (M, \cdot, e) $$, where $$ M $$ is a set,
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

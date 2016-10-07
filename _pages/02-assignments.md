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
-- i.e. for the sample tree, should return [3,2,5,1,4,6].
topDown :: IntTree -> [Int]

-- Same as above but, start from the bottom and work up, printing left to right
-- i.e. for the sample tree, should return [1,4,6,2,5,3].
bottomUp :: IntTree -> [Int]
```

---
layout: page
title: About
permalink: /about/
visible: true
---

This course assumes no knowledge of programming (although it does, of course,
presume basic computer literacy), since even if you have programmed before,
unless it was using a purely functional programming language, then learning
Haskell will feel like a totally new experience anyway.

Haskell has a reputation for being a difficult language to learn. The first
reason for this is the one stated above: programmers feel like they *should* be
in a better position to learn Haskell than non-programmers, but this is rarely
the case. The second reason is that the existing resources for Haskell are
scattered, and aimed primarily at either academic audiences or people who have
already had plenty of experience programming, or who already have a good idea of
how functional programming works. The third reason is because most of the
resources that are *not* aimed at people with a background in functional
programming are of a very 'get-rich-quick' style, a style that is all too
pervasive in programming pedagogy. Such tutorials will try to convince you that
a certain difficult concept is actually very easy if you just use *this*
analogy, or if you think about *this* example, irrespective of your prerequisite
knowledge.

They are wrong. You will *not* learn Haskell this way; in fact, if you convince
yourself that there *must* be some easier way to learn Haskell, then you doom
yourself to consistently being frustrated with the inherent uncertainty of
programming without really knowing what you're doing. You can get away with this
in Python. You can't in Haskell.

Let me be clear: there is no 'learning Haskell the easy way.' In this sense,
Haskell *is* difficult to learn. One must be thorough, much more so than if one
was learning Python. It's like learning mathematics. You learn by doing. You
learn by banging your head against a wall until it makes sense. But most
importantly, you learn by *internalizing the abstractions*. You should not be
thinking, 'ah, a monad is like [x],' you should be thinking, 'ah, [x] is like a
monad.' That's the entire point of abstractions in the first place.

Having said that, just because Haskell is harder to learn does not make it more
difficult to use. In fact, Haskell is *easier* to use than most programming
languages, once learned correctly, because it helps you write correct code. Not
only will you make fewer mistakes, but many of the mistakes that you could have
made in other programming languages cannot be made in Haskell, because they are
impossible to make by virtue of the language's design.

The single best piece of introductory writing on Haskell is [Haskell Programming
from First Principles](http://haskellbook.com/) by Chris Allen and Julie
Moronuki. It has a price tag attached, but it is extremely comprehensive (over
1000 pages in length!), so the cost is justified. There is a pretty generous
sample available on their website if you want to get a feel for it. It will
*not* be necessary to buy the book; this course is completely self-contained,
save for a few potential references to free, widely-available external
resources.

---
<br/>

<h3>Contents</h3> [//]:# (This ensures that the TOC doesn't include this header)

- TOC
{:toc}

## Course Outline

### Lecture 1

- Meta-lecture, preliminary reading materials, website location.
- No lab but make sure that the computers all have (or *can* have) stack
  installed on them.

### Assignment 1

Read Chapter 2 of HPFP and do whatever exercises I choose. These assignments
can't really be corrected, and therefore they will not be graded. You are
expected to do them as preliminaries for the rest of the course. ***Do not
skip them.***

### Lecture 2

- `Char`, `String`, `[]`, and functions that operate on these types.
- Lab: Simple string manipulation questions.

### Assignment 2

Extension of the lab.

### Lecture 3

- Looking at existing types, simple data declarations (sum/product types),
  data/type constructors, numeric types, simple typeclasses.
- Lab: exercises from Ch. 4 of HPFP.

### Assignment 3

Extension of the lab.

### Lecture 4

- Continued discussion on types: type system, currying (necessary for
  understanding the type constructor `->`), ad-hoc polymorphism, type inference,
  manually constraining types.
- Lab: [TODO]

### Assignment 4

Extension of the lab.

### Lecture 5

- Detailed look at typeclasses: `Num`, `Eq`, `Ord`, `Enum`, `Show`, `Read`, etc.
- Lab: [TODO]

### Assignment 5

Extension of the lab.

### Lecture 6

- Functional patterns: pattern matching, anonymous functions, case expressions,
  higher order functions guards, function composition, pointfree style.
- Lab: [TODO]

### Assignment 6

Extension of the lab.

### Lecture 7

- Recursion, bottom type, examples: factorial, Fibonacci, other more complex
  examples.
- Lab: [TODO]

### Assignment 7

Extension of the lab.

### Lecture 8

- Detailed discussion of lists: pattern matching, list syntactic sugar,
  transforming/filtering/zipping lists,`fold[l|r]`, `scan[l|r]`, associativity
  of operators and how that affects the folding process, introduction to
  monoids.
- Lab: [TODO]

### Assignment 8

Extension of the lab.

### Lecture 9

- More detailed look at algebraic datatypes: data/type constructors,
  correspondence between kinds -> types and types -> data, `newtype`,
  higher-kinded types, examples: lists, binary trees, `Maybe`, `Either`.
- Lab: [TODO]

### Assignment 9

Extension of the lab.

### Lecture 10

- Path to monad satori begins: monoids, semigroups, functors.
- Lab: [TODO]

### Assignment 10

Extension of the lab.

### Lecture 11

- Applicative and Monad.
- Lab: [TODO]

### Assignment 11

Extension of the lab.

### Lecture 12

- Real applications of Monoid, Functor, Applicative and Monad. Examples of
  commonplace monads: Reader, State, Parser Combinators. Short discussion of
  monad transformers.
- Lab: [TODO]

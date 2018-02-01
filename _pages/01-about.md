---
layout: page
title: About
permalink: /about/
visible: true
---

This course assumes no knowledge of Haskell whatsoever, however it will assume
that you understand how to program at an intermediate level in some imperative
programming language, like Python or Java.

<!-- Haskell has a reputation for being a difficult language to learn. The first
reason for this is the one stated above: programmers feel like they *should* be
in a better position to learn Haskell than non-programmers, but this is rarely
the case. The second reason is that the existing resources for Haskell are
scattered, and aimed primarily at either academic audiences or people who have
already had plenty of experience programming, or who already have a good idea of
how functional programming works, although this situation has improved
dramatically over the past 10 years. The third reason is because most of the
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
in Python. You can't in Haskell. -->

Haskell is mostly difficult to learn because its computational model is unlike
those found in most other programming languages. As such, it might take you a
while to internalise this model. Don't worry too much about it, just keep
writing Haskell until you get used to it.

The upside of this is that Haskell is, in my opinion, *easier* to use than many
programming languages, once learned correctly, because it helps you write
correct code. Not only will you make fewer mistakes, but many of the mistakes
that you could have made in other programming languages cannot be made in
Haskell, because they are impossible to make by virtue of the language's design.

The single best piece of introductory writing on Haskell is [Haskell Programming
from First Principles](http://haskellbook.com/) by Chris Allen and Julie
Moronuki. It has a price tag attached, but it is extremely comprehensive (over
1000 pages in length!), so you may find that the cost is justified. There is a
pretty generous sample available on their website if you want to get a feel for
it. It will *not* be necessary to buy the book; this course is completely
self-contained, save for a few potential references to free, widely-available
external resources.

---
<br/>

<h3>Contents</h3> [//]:# (This ensures that the TOC doesn't include this header)

- TOC
{:toc}

<br/>

---
<br/>

## Course Outline

### Lecture 1

- Preliminary reading materials, website location.
- Lambda calculus and Haskell's model of computation.
- No practical work, just have stack installed for next time

### Lecture 2

- `Char`, `String`, `[]`, and functions that operate on these types.
<!-- - Lab: Simple string manipulation questions. -->

### Lecture 3

- Looking at existing types, simple data declarations (sum/product types),
  data/type constructors, numeric types, simple typeclasses.
<!-- - Lab: exercises from Ch. 4 of HPFP. -->

### Lecture 4

- Continued discussion on types: type system, currying (necessary for
  understanding the type constructor `->`), ad-hoc polymorphism, type inference,
  manually constraining types.

<!-- ### Lecture 6

- Functional patterns: pattern matching, anonymous functions, case expressions,
  higher order functions, guards, function composition, pointfree style.
- Lab: [TODO] -->

### Lecture 5

- Recursion, bottom type, examples: factorial, Fibonacci, other more complex
  examples.

### Lecture 6

- Detailed discussion of lists: pattern matching, list syntactic sugar,
  transforming, filtering, zipping.
- `fold[lr]`, `scan[lr]`, associativity of operators and how that affects the
  folding process
- Introduction to monoids.

### Lecture 7

- More detailed look at algebraic datatypes: data/type constructors,
  correspondence between 'kinds -> types' and 'types -> data,' `newtype`,
  higher-kinded types, examples: lists, binary trees, `Maybe`, `Either`.

### Lecture 8

- Path to enlightenment begins: monoids, semigroups, functors.

### Lecture 9

- Applicative (monoid + functor) and Monad (applicative + some nice extra
  stuff).

### Lecture 10

- Real applications of Monoid, Functor, Applicative and Monad
- Examples of commonplace monads: Reader, State, Parser Combinators.
- Short discussion of monad transformers, quick look at the lensed-reader
  pattern.

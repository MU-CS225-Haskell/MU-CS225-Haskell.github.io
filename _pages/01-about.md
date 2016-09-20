---
layout: page
title: About
permalink: /about/
---

This course is an introduction to functional programming through Haskell. While
this course does focus on teaching you Haskell, it is more useful to view it as
an introduction to writing and reasoning about programs written in the
functional style, regardless of the language used. Since Haskell is a *purely*
functional programming language (i.e. only functional, no non-functional
features are present), it is, in my opinion, the most obvious choice for this
task.

Also, if the prospect of having your
[mind blown](http://www.reactiongifs.com/r/2013/10/tim-and-eric-mind-blown.gif)
in directions you didn't even know existed, you'll love Haskell. This usually
happens to people who come from an imperative programming background. Even if
you decide that Haskell's not for you, and that you'll never use it again, it
will at least make you a better programmer.

And hey, if you're sold on functional programming, but don't feel like you'll
connect with Haskell, there's always Clojure, Scala, OCaml, F#, etc., whatever
suits you! I would especially recommend Clojure if you're looking for something
that'll bend your brain a bit, but which deviates significantly from Haskell in
its approach.

This material is best considered **supplementary** to [this](http://www.cis.upenn.edu/~cis194/spring13/)
course material (taught at UPenn). That is to say, don't expect this website to
have all the information, because it doesn't.

<h3>Contents</h3> [//]:# (This ensures that the TOC doesn't include this header)

* TOC
{:toc}

## Haskell

Haskell is a purely functional, declarative language which is statically,
stongly, and implicitly typed. Let's deconstruct this word-salad:

- **Functional** means that computation is constructed using only pure
functions (i.e. ones that perform no side-effects that can be observed outside
the function context). Like functions in maths. $$ f(x) = x^2 $$ does nothing
but accept input and return a result, it doesn't also print to a terminal or
write to a file or communicate with other programs. The function is not
dependent in any way on factors *outside* the function to compute its result.

- **Declarative** languages require only that you specify *what* any given
function should do, not necessarily *how* exactly it should do it. This gives
Haskell a very 'high-level' feel. All you do is tell the compile what you want
done, and the compiler will figure out the best way to do it.

- **Static typing** means that the type of an expression is known and statically
checked at compile time (like Java), in contrast to dynamic typing, where types
are checked dynamically at runtime (like Python).

- **Strongly typed** languages make it difficult to coerce the type system into
allowing you to change the type of an expression. As such, it's a little
difficult to define what is really meant by a strongly typed language, but I can
certainly say that if *any* language is to be considered strongly typed, it's
Haskell. Note that coercing types is technically possible using
[unsafeCoerce](https://hackage.haskell.org/package/base-4.9.0.0/docs/Unsafe-Coerce.html),
but be warned that your code *will* break if you use it without knowing exactly
what you're doing (you *don't*). Using it nullifies the entire point of using
Haskell in the first place. From the docs:

> "Needless to say, if you use this function, it is your responsibility to ensure
that the old and new types have identical internal representations, in order to
prevent runtime corruption."

- **Implicitly typed** languages allow you to declare values without specifying
their type, the type system can infer which types you mean from context. Haskell
supports an extremely powerful form of type inference called *Hindley-Milner*
type inference, so as long as you aren't using some of Haskell's more, shall we
say, 'interesting' (read: insane) features, the type system can usually figure
out the types you want from context, unless it is genuinely ambiguous.

## Functional Programming

Functional programming languages (in particular, *purely* functional languages)
treat computation as the evaluation of mathematical functions. By mathematical,
I don't mean that the functions are 'doing maths', I mean that the functions are
like those defined in mathematics: a one-to-one or many-to-one relationship
between two sets (you'll probably see this definition a few times in your maths
modules).

Since functions are so critical in functional programming, they are considered
first class objects, meaning we can treat them like any other value. We can pass
them as arguments to functions, return them as results, store them in lists,
anything you could do with any other value.

One of the most widely used functions in Haskell is `map`. This function takes
a function and a list, and applies that function to every element in the list.
I.e. `map increment [1,2,3]` (assuming `increment` does what you expect it to
do) returns `[2,3,4]`. Riveting! This ability to pass functions as arguments
allows for much more useful abstractions, and ultimately, much cleaner code.

The Wikipedia page for [functional programming](https://en.wikipedia.org/wiki/Functional_programming)
is a great read, I suggest you indulge your curiosity.

## Resources

There are a lot of extremely comprehensive tutorials written for Haskell. Here's
a small list of some recommended and not-so-recommended resources:

#### **Recommended**

- [CIS 194: Introduction to Haskell (Spring 2013)](http://www.cis.upenn.edu/~cis194/spring13/)
-- fantastic resource, doesn't hold your hand, perfectly paced and pretty much
covers everything I'd like to cover for this course. Also includes exercises.
This is the recommended resource for this module.

- [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/)
-- really comprehensive resource, deals with a *huge* amount of topics in a
concise but understandable way, including text editor setup, using stack, etc.
In my opinion, you'll need to have a reasonably good grasp on Haskell before you
can get the best out of this resource, but blast away anyway if you're up for a
challenge.

- [The Typeclassopedia](https://wiki.haskell.org/Typeclassopedia) --
*the* definitive way to grok Haskell's typeclasses. You shouldn't attempt to
read this without the necessary experience, and you *definitely* shouldn't go
near it until we've covered typeclasses.

#### **Not recommended**

I'm including resources that are not recommended because, in my opinion,
learning from a bad resource is worse than not learning at all. I still
encourage you to read them, but be aware that they are not where you should be
drawing core understanding of the concepts from; they are not gospel.

- [Learn You A Haskell](http://learnyouahaskell.com/) -- this used to be *the*
definitive entry point for any beginner to functional programming wanting to use
Haskell. And for getting across the simple ideas, it does an *excellent* job. In
fact, it's the book I used when I learned Haskell first. However, based on this
experience, I can't recommend it. It doesn't do as good a job as I'd like of
getting across the more complicated ideas, as I realised when I went on to learn
more about what Haskell had to offer on my own. Also, it is quite outdated, a
good few things have changed since the book was written. As such, a few of the
examples in the book no longer work. As a supplementary resource, it's fine, but
don't rely on it.

- A year ago, I also may have recommended [Real World Haskell](http://book.realworldhaskell.org/),
however given the age of the book (8 years!), much of the examples are now
quite outdated, so I can no longer recommend it for beginners. I encourage you
to have a look at it if you're interested, just don't expect the code snippets
to always work.

## Contact Me

If something doesn't make sense, or if you need clarification on a particular
point, please [email me](mailto:conor.reynolds.2015@mumail.ie), I'll respond
within the day. You'll also find my email on the footer of every page.

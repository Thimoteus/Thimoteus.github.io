---
layout: post
title:  "Better than QuickCheck, not as good as a proof assistant (Part 1)"
permalink: /:title.html
categories: purescript
---

# Intro

Suppose you've defined a data type, and you know you'll want to give it some
typeclass instances. You also want to make sure its instances are [law-abiding](http://blog.functorial.com/posts/2015-12-06-Counterexamples.html).

It seems there tend to be two approaches: a [quickcheck](https://github.com/purescript/purescript-quickcheck)
/[strongcheck](https://github.com/purescript-contrib/purescript-strongcheck)
approach on one extreme, and on the other, using a [proof assistant](https://arxiv.org/pdf/1512.02102.pdf).

Not many people talk about a middle-of-the-road approach, which is using yourself
as a proof assistant. After all, [people have been doing it for centuries](https://en.wikipedia.org/wiki/Mathematics).

# Examples

Let's start with something simple: we'll define a simple data-type, and give it
an instance of a relatively easy-to-understand typeclass.

```haskell
data NonEmptyArray a = NonEmptyArray a (Array a)

infixr 6 NonEmptyArray as :

instance semigroupNonEmptyArray :: Semigroup (NonEmptyArray a) where
  append (x : _) (_ : ys) = (x : ys)
```

Now, this probably isn't the definition most people have when they think of appending
two non-empty arrays together. But we're not interested in what most people think,
we're interested in whether this is a law-abiding class.

The only law for a semigroup is that of its operation's associativity:

```
∀ a b c: a <> (b <> c) = (a <> b) <> c
```

If you have doubts about whether the instance we defined is law-abiding, I encourage
you to try and come up with an answer (either a counterexample or a proof) before
reading on.

## Simple example continued

Our general strategy will be to rewrite the law in terms of our data-type definitions,
and "follow the definitions" until we can see that both sides of the equation are
in fact equal (or something goes horribly, horribly wrong).

Left-hand side:

1. (x : xs) <> ((y : ys) <> (z : zs)) = -- Definition of the law's LHS
2. (x : xs) <> (y : zs) = -- Definition of `<>` applied to the inner parens
3. (x : zs) -- Definition of `<>`

Right-hand side:

1. ((x : xs) <> (y : ys)) <> (z : zs) = -- Definitition of the law's RHS
2. (x : ys) <> (z : zs) = -- Defintion to `<>` applied to inner parents
3. (x : zs) -- Definition of `<>`

[Question (harder): How many nonequivalent law-abiding instances are there for this data type?]

## Slightly harder

Semigroups are among the simpler typeclasses to think about: they talk about types
with simple kinds, they only have one law, and that law is something most people
are familiar with already (If I have to multiply 5, 7, and 2 in my head, I find
it easier to figure out (5*2)*7 instead of 5*(2*7)).

Let's look at a slightly tougher typeclass, the one that characterizes functors:

```haskell
instance functorNonEmptyArray :: Functor NonEmptyArray where
  map f (x : _) = f x : []
```

We need to prove two laws:

```
1. map f <<< map g = map (f <<< g)
2. map id = id
```

As before, if you're unsure as to whether this is law-abiding I encourage you to
try and figure it out yourself first.

## Slightly harder continued

Let's start with the second law, since it looks like it may be simpler to work with:

LHS:

1. map id = -- Definition of LHS
2. \ (x : xs) -> map id (x : xs) = -- rewriting pointfully
3. \ (x : xs) -> id x : [] - -- definition of `map`
4. \ (x : xs) -> x : [] -- definition of `id`

RHS:

1. id = -- Definition of RHS
2. \ (x : xs) -> (x : xs) -- rewrite pointfully

Now it should be clear what our answer is: this instance is not lawful, and as a
counterexample we can use any `(x : xs)` where `xs` is an array with more than 0
elements.

[Question (easier): what's the (only) correct instance, up to equivalence?]

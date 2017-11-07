---
layout: post
title:  "RowToList, records as trees, and double-induction"
permalink: /:title.html
categories: purescript
---

# Intro (you can skip this)

About two weeks ago, I wanted to write a little command-line program with PureScript.
I didn't feel like hand-writing another arg-parser, and there weren't any pure PS
libraries for dealing with args (there's a wrapper around "yargs" but I prefer not
to have to `npm i somelib` whenever I can avoid it).

So, I reluctantly began writing my own general(ish)-purpose command-line option
parsing [library](https://github.com/Thimoteus/purescript-optlicative). Once I
realized I didn't have a story for parsing commands, I thought for a while about
how to deal with a `--help` flag being called on different commands. `--help` is
unusual in that it tends to be shared among all commands, so you can for example
run all of the following:

* `stack --help`
* `stack config --help`
* `stack config set --help`

Each of which will print out a different help string. Moreover, in addition to
this depth of command chains, there's also breadth -- you can call `stack run --help`
for example, as another first-level command.

So I needed to have a way of dealing with arbitrary depth and breadth.

# Everything's a row

It turns out rows can also have depth and breadth:

```purescript
type SomeRow =
  ( x :: Int
  , y ::
    ( y1 :: String
    , y2 :: Boolean
    )
  , z :: Void -> Unit
  )
```

In this case, there's a maximum depth of 2 and maximum breadth of 3. So what if
I tried matching the actual command path (like `config set`) against a row-type
that holds all possible help strings?

We could do something like this:

```purescript
type StackExample =
  ( config :: Help "some help message"
    ( set :: Help "some other help message" () )
  , run :: Help "third help message" ()
  )
```

# Classy command parsing

The output for our help-message-returning-function should be `Maybe String`, since
it's possible that the command path isn't reflected by the row. We should also
already have access to a list of commands. Finally, we'll need a way to pass
around information about the row so the compiler can do fun things with it:

```purescript
class Commando (row :: # Type) where
  commando :: forall proxy. proxy row -> List String -> Maybe String
```

So far so good. We'll also delegate all the heavy-lifting to a class that works
on `RowList`s, so there's only one actual instance of `Commando`:

```purescript
instance commandoInst ::
  ( RowToList row list
  , RLHelp list row
  ) => Commando row where
    commando _ xs = rlHelp (RLProxy :: RLProxy list) (RProxy :: RProxy row) xs
```

[Note: at this point, I'm not even sure if I needed both `RLProxy` and `RProxy`.
But using both gives the desired results, so I'll stick with it.]

So the "real" class is `RLHelp`, which has one method `rlHelp`:

```purescript
class RLHelp
  (rl :: RowList)
  (row :: # Type)
  | rl -> row
  where
    rlHelp :: RLProxy rl -> RProxy row -> List String -> Maybe String
```

Since the rowlist is supposed to be equivalent to the row, we have the fun-dep
`rl -> row`. Since we don't have `row` on the left-hand side of
a fun-dep, we can use row literals in our instances all we want. Noice!

```purescript
instance basisRlHelp :: RLHelp Nil () where
  rlHelp _ _ _ = Nothing
```

This part's easy: If the row is empty, there are no possible commands to match,
hence no command help, so we just give `Nothing`.

The trickier part will, of course, be the `Cons` case. I'll paste it in then go
through it piece-by-piece:

```purescript
instance ihRlHelp ::
  ( IsSymbol k
  , IsSymbol h
  , RLHelp tail rowtail
  , RLHelp list' row'
  , RowCons k (Help h row') rowtail row
  , RowLacks k rowtail
  , RowToList rowtail tail
  , RowToList row (Cons k (Help h row') tail)
  , RowToList row' list'
  ) => RLHelp (Cons k (Help h row') tail) row where ...
```

The first two lines for the superclass are easy, they just say that `k`
(representing the field name, or "key") and `h` (the help text) are `Symbol`s.

The third line is the typical inductive hypothesis that accompanies instances
making use of `RowToList`: It requires that the instance already exist for
the rowlist's tail and the row's tail.

The fourth line is an atypical inductive hypothesis: This is induction on the
depth of the row. So if the instance we're currently looking at is at depth
`n`, the fourth line requires us to have an instance at depth `n + 1` through the
current node.

Line five expresses the relationship between the current node and the ones at
the next-depth: it says our key has a type in which the next-level row appears,
and that this together with the rest of the row (the row tail, as it were) make
up the entirety of the row that's still to be computed.

Six merely expresses the fact that our current node's key doesn't appear elsewhere
at the current level.

Seven requires the row's tail to be equivalent to the rowlist's tail,
eight, the whole row to the whole tail, and nine the next-level row to the next-level
tail.

Finally, the actual instance applies to the whole rowlist (expressed in terms of
its head and tail) and the whole row.

# The implementation

```purescript
rlHelp _ _ args@(x : Nil) =
  if x == reflectSymbol (SProxy :: SProxy k)
    then Just (reflectSymbol (SProxy :: SProxy h))
    else rlHelp (RLProxy :: RLProxy tail) (RProxy :: RProxy rowtail) args
rlHelp _ _ args@(x : xs) =
  if x == reflectSymbol (SProxy :: SProxy k)
    then rlHelp (RLProxy :: RLProxy list') (RProxy :: RProxy row') xs
    else rlHelp (RLProxy :: RLProxy tail) (RProxy :: RProxy rowtail) args
rlHelp _ _ _ = Nothing
```

The first case is when there's exactly one command left in the command path.
If this final command matches the node-in-focus's key, we've found the final match.
Then we can return the help text given, and all is well.

If it doesn't match, we need to recurse on the tail -- the rest of the nodes at
the same level. In this case, we keep the singleton command path intact.

The second case is when there's more than one command left. That means we can't
have possibly reached the end of the algorithm.

If the head of the command path matches the key of our node, then we recurse
into the node -- we go to the next deepest level, passing only the tail of the
command path.

If it doesn't, we keep the command path intact but recurse sideways into the tail.

The last case is when we've exhausted the command path. If we had found a match
it would have appeared by virtue of the first case, but since it didn't that means
the command entered does not match with any path through the row. In that case
we can't give back any help text, so we return `Nothing`.

# Final thoughts

* This technique seems powerful. It's essentially folding a tree, but
done with type-level machinery instead of a normal ADT.
* Double-induction on trees isn't unheard of in math -- for example, proving
cut-elimination for classical logic sequent trees requires dealing with both
depth and breadth. It's also a lot more tedious than what we just did, despite
the lack of compiler to yell at you when you mess up.
* Type-level recursion schemes, anyone?

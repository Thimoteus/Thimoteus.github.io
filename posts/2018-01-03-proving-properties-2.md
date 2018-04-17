---
layout: post
title:  "Better than QuickCheck, not as good as a proof assistant (Part 2)"
permalink: /:title.html
categories: purescript
---

# A digression into logic

In the [previous part](2017-12-29-proving-properties.md) of this series, we looked
at a simple data type and two typeclasses, proving/disproving that the instances
we wrote for those typeclasses followed their respective laws.

Those examples were fairly simple to reason about, because they relied on equational
reasoning -- all we did was substitute terms by their definitions until two sides
of an equality were syntactically identical.

But sometimes we need a litte more power in our arsenal. This interlude is meant
to be a lightning-fast, nowhere-near-comprehensive introduction to classical propositional
logic and natural deduction.

## Propositions

Atomic propositions are sort of like named constants in math, or an argument in
a function body. They refer to definite statements, but we may not know what statement
that is (in fact, for our purposes it doesn't matter). As an example, we could
treat the statement "It's raining outside" as an atomic proposition and give it
a name (say, 𝑃). We can also refer to "true" and "false", typically written as
⊤ and ⊥.

## Connectives

Propositions can be constructed out of other propositions using connectives.
They have familiar names: "and", "or", "implies", "not". If we were to try and
model the syntax of propositional logic with code, it may look something like this:

```haskell
type Atom = Char

data Proposition
  = Atom Atom
  | And Proposition Proposition
  | Or Proposition Proposition
  | Implies Proposition Proposition
  | Not Proposition
```

You might write "and" as "∧", "or" as "∨", "implies" as "→" and "not" as "¬".

## What is a natural deduction proof?

Broadly speaking, a proof is a finite ordered list of propositions such that:

1. It has a (possibly empty) starting list of propositions, called assumptions.
2. Each subsequent proposition is formed from previously occurring ones by means
of a rule, or is a restatement of a previously-occurring proposition.

The last proposition in a proof is the conclusion.

So in order to prove things, we need rules (in the previous post we only had the
rule of substituting definitions) for generating new propositions.


## Rules of introduction

1. Conjunction (aka "and")

Suppose we have a proof such that a proposition 𝑃 occurs at line i, and a proposition
𝑄 occurs at line j. Then at a new line, we can assert 𝑃 ∧ 𝑄. We already proved
each one separately, so we can treat them both as a single proposition.

2. Disjunction (aka "or")

Suppose our proof has proposition 𝑃 at line i. Then on a new line, we can assert
𝑃 ∨ 𝑄, for any proposition 𝑄 that we like. After all, since we know 𝑃 is true,
surely either 𝑃 is true or 𝑄 is true: this is because for the disjunction to be
true, it is enough that one of the disjuncts is true.

3. Implication (aka "if-then")

This one's a bit more exotic. At any point in the proof, we can assume a new proposition,
but the assumption "costs" us. Essentially, we need to be able to justify the use
of the assumption in order to "pay it off". Implication introduction is one way
to do that.

Supposing we've assumed a new proposition 𝑃, then derived a bunch of things from
it, ending in 𝑄. Then we can justify the assumption of 𝑃 by concluding 𝑃 → 𝑄.

As an example:

1. 𝑃 (Assumption)
2. | 𝑄 (Assumption in a subproof)
3. | 𝑃 ∧ 𝑄 (Conjunction introduction)
4. 𝑄 → (𝑃 ∧ 𝑄) (Implication introduction)

Note the use of a "|" character to separate the subproof from the rest.

4. Negation (aka "not")

This is a special case of implication introduction. Suppose we have a subproof
that starts with 𝑃 and ends in a contradiction (the symbol of which is "⊥").
Then we can conclude the premise of the subproof was faulty, so it must be wrong.
In other words, we conclude ¬𝑃.

(Side note: compare with a function type `P -> Void`)

## Rules of elimination

1. Conjunction (actually two rules)

Suppose we have at line i the proposition 𝑃 ∧ 𝑄. Then a) on a new line we may infer
𝑃, b) on a new line we may infer 𝑄.

This is because if we know both of two things are true, we surely know each one
is true by itself.

2. Disjunction

On line i we have the proposition 𝑃 ∨ 𝑄. Then, suppose we have a subproof that
assumes only 𝑃 and concludes 𝑅. Furthermore, we start a new subproof (at the same
"level" as the previous one) that assumes only 𝑄 and concludes 𝑅. Then we may
conclude 𝑅.

To illustrate:

1. 𝐴 -- Assumption
2. 𝑃 ∨ 𝑄 -- Assumption
3. | 𝑃 -- Premise
4. | 𝐴 -- Subproof conclusion
5. | 𝑄 -- New subproof premise
6. | 𝐴 -- New subproof conclusion
7. 𝐴 -- Conclusion

(Side note: compare with pattern matching)

3. Implication

Also known as "modus ponens". Suppose at line i we have the proposition 𝑃 → 𝑄,
and at line j we have the proposition 𝑃. Then on a new line we can conclude 𝑄.

(Question: can lines i and j be the same?)

## Some short examples

Identity

1. | 𝐴 -- Subproof assumption
2. | 𝐴 -- restatement of assumption
3. 𝐴 → 𝐴 -- Conclusion

Tastes like Curry

1. | A -- Subproof assumption
2. || B -- Subsubproof assumption
3. || 𝐴 ∧ 𝐵 -- Conjunction intro
4. | 𝐵 → (𝐴 ∧ 𝐵) -- Implication intro
5. 𝐴 → (𝐵 → (𝐴 ∧ 𝐵)) -- Implication intro

Explosion

1. P \land \lnot P

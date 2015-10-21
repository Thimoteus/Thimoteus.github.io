---
layout: post
title: Write Yourself a Lisp (With Purescript!) Part 3
---

## Recursive parsing, fixpoints and nested expressions

Because Purescript is not evaluated lazily like Haskell is, this will bite us if we naively try to port recursive Haskell parsing to Purescript.

Let's start with writing a parser for lists:

```purescript
parseList :: SParser LispVal -> SParser LispVal
parseList pars = List <$> pars `sepBy` whiteSpace
```

Unlike Haskell, we need to accept another parser as an argument (later we'll make use of this in `parseExpr`).

`sepBy` is a normal parser function, but we apply it infix with backticks to improve readability: as it is, you can read it as "parseList takes a parser and returns a parser that wraps a List of the input parser separated by whitespace".

Now we'll move to dotted lists:

```purescript
parseDottedList :: SParser LispVal -> SParser LispVal
parseDottedList pars = do
  init <- pars `endBy` whiteSpace
  last <- char '.' *> whiteSpace *> pars
  return $ DottedList init last
```

Later, we will use "naked" lists only for function application (example: `(+ 2 3)`). In order to create a list literal then, we'll introduce "clothes" for lists in the form of a single quote:

```purescript
parseQuoted :: SParser LispVal -> SParser LispVal
parseQuoted pars = do
  string "'"
  x <- pars
  return $ List $ (Atom "quote") : x : Nil
```

I prefer using `string` here instead of `char`, since otherwise we would have to escape the quote with `char '\''`, though both will get the job done.

Now we'll edit `parseExpr` to use our new parsers. This is where we use fixed points to parse nested expressions; first, import `Control.Lazy`.

```purescript
parseExpr :: SParser LispVal
parseExpr = fix $ \p -> (parseAtom
                     <|> parseString
                     <|> parseInt
                     <|> parseQuoted p
                     <|> (do
                         char '('
                         x <- try (parseList p) <|> parseDottedList p
                         char ')'
                         return x))
```

## Exercises

1. Everything from WYAS: Note for vector support, JS native Arrays are probably the best bet.
2. Try parsing some nested lists by changing what `args` is in the `main` function.

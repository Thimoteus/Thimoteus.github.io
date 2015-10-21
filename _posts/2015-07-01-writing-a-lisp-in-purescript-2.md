---
layout: post
title: Write Yourself a Lisp (With Purescript!) Part 2
---

## Writing the parser

Install `purescript-parsing` by running `pulp dep install purescript-parsing --save`. In a file (which can be `src/Main.purs` or another file if you prefer), import the library in the top level:

```purescript
module Lisp.Parser where

import Prelude

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String

import Data.String
```

As you can see, the top-most line is a module declaration, which is followed by module imports. You can also do qualified imports which help when you need to use two modules that export similarly-named functions. You could for example change the `Data.String` import to `import qualified Data.String as S`. Just keep in mind that any reference to functions from that module will need to be preceded by the qualified name: `S.toCharArray` for example.

The `Parser` type in `purescript-parsing` is more general than the one in Haskell's parsec. We can make it easier by defining a type synonym:

```purescript
type SParser a = Parser String a
```

That is, `SParser` represents a parser that reads `String`s and outputs a type `a`. We define the symbols for our language:

```purescript
symbol :: SParser Char
symbol = oneOf $ toCharArray "!#$%&|*+-/:<=>?@^_~"
```

The `$` operator is defined as function application: `f $ x = f x`. This allows us to remove parentheses, so we don't write `oneOf (toCharArray ...)` instead. The `toCharArray` function's type signature is `String -> Array Char`, which we need since `oneOf` takes an array of `Char`s as input. Unlike Haskell, PureScript `String`s are not arrays or lists of `Char`s.

We'll also have to define parsers for letters and digits since they don't come predefined in our parsing library:

```purescript
digit :: SParser Char
digit = oneOf $ toCharArray "0123456789"

letter :: SParser Char
letter = oneOf $ toCharArray "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
```

Our reading function is a bit different as well:

```purescript
readExpr :: String -> String
readExpr input = case runParser input symbol of
                      Left err -> "No match: " ++ show err
                      Right _ -> "Found value"
```

The `case ... of` construction lets us pattern-match on the result of the computation in ellipsis. The return type of the computation is `Either ParseError a`, signifying a parsing error (on the left) or a successful parse (on the right).

Once again, we need to import the "Left" and "Right" type constructors from `Data.Either` in order to make this compile.

Open up the `src/Main.purs` file (if it isn't already) and change the body of `main` to the following:

```purescript
main = do
  let args = "!"
  log $ readExpr args
```

Here, we assign the string `"!"` to `args` then pass it to `readExpr` and print the result to the console. You can run this with `pulp run`.

The "do" notation is actually syntactic sugar for working with monads. It's not much different from Haskell, so instead of writing another explanation about monads (seriously, the Haskell community is known for the number of its monad tutorials) I'll redirect you [here](https://www.reddit.com/r/haskell/search?q=monad+tutorial&sort=top&restrict_sr=on&t=all).

## Whitespace

Our parsing library already has a parser defined for whitespace, so we don't have to make our own. Edit `readExpr` to use the `whiteSpace` parser:

```purescript
readExpr :: String -> String
readExpr input = case runParser input (whiteSpace *> symbol) of
                      Left err -> "No match: " ++ show err
                      Right _ -> "Found value"
```

The function `*>` comes from Control.Apply. It and its brother `<*` are related to another function, `const :: forall a b. a -> b -> a`.

Since this works for *all* types `a, b` there's only one possible function with this type signature: projection onto the first argument. This might not seem very useful, but functions in PureScript are curried --- meaning if you only supply some but not all of the arguments in the type signature to a function, it will return a function that accepts the remaining arguments. A canonical example is `plusTwo n = ((+) 2) n`. Hence, a partially-applied `const` takes any argument, ignores it and returns a value `a`, justifying the name `const`.

## Lisp Values

Let's define data types for our parser so it can transform text to something useful. Before proceeding, we have to make a design decision. In Haskell, bracket notation like `[1, 2, 3]` represents [lists](https://en.wikipedia.org/wiki/List_(abstract_data_type) while in PureScript, bracket notation is used for [arrays](https://en.wikipedia.org/wiki/Array_data_type). To keep in line with lisp semantics, let's use Purescript `List`s. If it isn't available yet, install `purescript-lists` and import it at the top of your current module.

Now we can define some basic lisp values:

```purescript
data LispVal = Atom String
             | List (List LispVal)
             | DottedList (List LispVal) LispVal
             | Int Int
             | String String
             | Bool Boolean
```

Note the double use of `List` in our definition. [This is not ambiguous](http://stackoverflow.com/a/18205862), since one is a *type constructor* while the other is a *data constructor*.

Now we'll write parsers that transform text into `LispVal`s.

Let's start with strings. We want them to be identified by surrounding double-quotes, but we don't want the quotes to be stored. Since our parser will be reading strings anyway, this is one of the simpler ones, although we will need to import `Data.Char` as well (I've imported it qualified as `C`).

```purescript
parseString :: SParser LispVal
parseString = do
  char '"'
  x <- many (noneOf ['"'])
  char '"'
  return $ String (foldMap C.toString x)
```

This code almost explains itself: the parser is looking for an opening double quote, followed by many characters which aren't a double quote, and terminated by a quote.

The `many` function is provided by `Data.List`, so it creates a `List` of `Char`s. However, there is no function in our libraries which converts from lists of `Char`s to a `String`, so we have to write our own. A simple attempt would be to convert the list to an array of characters, then use `C.fromCharArray`. However, this seems kind of clunky so we're using `foldMap` from `Data.Foldable`. Its type signature is `(Foldable f, Monoid m) => (a -> m) -> f a -> m`. In our case we need `List`s to be foldable, and `String`s to be monoids --- and luckily for us, they are. Essentially, `foldMap` goes through a foldable structure, applying its first argument (a function from the structure's elements to a monoid) successively and building up the result with the monoid's binary operator. As a result, our code looks neater than converting from `List`s to `Array`s and then to a `String`.

Now let's parse our variables:

```purescript
parseAtom :: SParser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> symbol <|> digit
  let atom = foldMap C.toString $ first:rest
  return $ case atom of
                "true" -> Bool true
                "false" -> Bool false
                _ -> Atom atom
```

Again, this is almost self-explanatory. An `Atom` is a letter or a symbol followed by many letters, symbols or digits. If the atom we're parsing is literally `true` or `false` then turn them into `Bool`s, otherwise into `Atom`s. The `<|>` [combinator has signature](http://pursuit.purescript.org/packages/purescript-control/0.3.0/docs/Control.Alt#d:%28%3C%7C%3E) `f a -> f a -> f a`, and in our context it means to try parsing the left, and if that failed parse the right.

To parse numbers, we'll need to write a function that converts Purescript strings to Purescript ints. You could do this completely in Purescript with pattern matching on digits, but that's unnecessarily complex. Instead, we'll use the FFI. In a Purescript file, we'll import our foreign function:

```purescript
foreign import str2num :: String -> Int
```

And in a separate javascript file with the name of the module in which the foreign import occurs as a comment (e.g. `\\ module Lisp.Util`) put the following:

```javascript
exports.str2num = function str2num(str) {
  return (str | 0);
}
```

Luckily for us, pulp will take care of looking for javascript files we use for our FFI. Now we can define our parser for ints:

```purescript
parseInt :: SParser LispVal
parseInt = Int <<< str2num <<< foldMap C.toString <$> many1 digit
```

The `<<<` combinator is just function composition: `f <<< g = \ x -> f (g x)`, and the `\ ... -> ... ` syntax is just creating an anonymous function. `<$>` is also known as `map`, and lets us apply a function to a value wrapped in a functor. If the functor is `Array`, for instance, `map` will apply a function to each element in the array.

`many1` is not a function in the parsing library, so we have to write it:

```purescript
many1 :: forall a. Parser String a -> Parser String (List a)
many1 par = do
  x <- par
  xs <- many par
  return (x:xs)
```

Now we'll create a megaparser that combines all our smaller ones:

```purescript
parseExpr :: SParser LispVal
parseExpr = parseAtom <|> parseString <|> parseInt
```

Finally change `readExpr` to use `parseExpr`:

```purescript
readExpr :: String -> String
readExpr input = case runParser input parseExpr of
                      Left err -> "No match: " ++ show err
                      Right _ -> "Found value"
```

## Exercises

1. Everything from WYAS
2. Rewrite `parseInt` so it can read negative numbers as well.


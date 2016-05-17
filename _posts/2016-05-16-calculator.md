---
layout: post
title:  "Coding a simple calculator in Purescript"
permalink: /:title.html
categories: jekyll update
---

I feel like everyone should write a toy programming language at least once
in their lives. If you do, you might get drawn into the rabbit hole of adding
features, syntax sugar, a compiler, a REPL, etc. etc., and along the way
you'll probably learn a lot about language design.

This post isn't about implementing a toy programming language, but rather
something a little simpler: a calculator. While it might not be Turing
complete, it touches on many of the same concepts (lexing, parsing, type
checking, evaluating, etc). And in the process, we'll become familiar with
a lot of what makes Purescript's ecosystem so great, from parser combinators,
to pure and functional error handling, to pattern matching and more.

## Preliminaries

I'll assume you have Purescript, bower and pulp installed. Let's start by
making a new project directory:

    > mkdir calculator && cd calculator

Then we'll initialize the project with some defaults:

    > pulp init
    > bower i --save purescript-parsing purescript-maps purescript-simple-repl

## Language definition

We'll start by defining our language's abstract syntax tree:

```haskell
module Syntax where

import Prelude

data Expr = Lit Lit
          | Var Name
          | Binop Binop Expr Expr
          | Unop Unop Expr

type Name = String

data Binop = Add | Sub | Mul | Div | Less | Eql | Or

data Unop = Negate | Not

data Lit = Int Int | Bool Boolean
```

Expressions will be made up of literals, variables, and operators of either one
or two expressions.

Binary operators will be addition, subtraction, multiplication, division, order
comparison, equality and boolean disjunction. One can imagine how to extend this
to include other operators.

Unary operators will be numeric and boolean negation.

Literals are just integers and booleans, represented by Purescript `Int`s and
`Boolean`s.

While we're at it, let's define a way to turn expressions to `String`s:

```haskell
instance showExpr :: Show Expr where
  show (Lit l) = show l
  show (Var n) = n
  show (Binop op e1 e2) = show e1 <> show op <> show e2
  show (Unop op e1) = show op <> show e1

instance showBinop :: Show Binop where
  show Add = " + "
  show Sub = " - "
  show Mul = "*"
  show Div = "/"
  show Less = " < "
  show Eql = " = "
  show Or = " || "

instance showUnop :: Show Unop where
  show Negate = "-"
  show Not = "~"

instance showLit :: Show Lit where
  show (Int n) = show n
  show (Bool b) = show b
```

these are typeclass instances. I like to think of `Show` as a predicate on
types, so that if a type has a `Show` instance, it can be `show`n: that is,
turned into a `String`. The type of `show` is `forall a. Show a => String`.  In
other words, `show` takes a single argument, and as long as the type of that
argument has the correct instance, it will typecheck.

We can also derive equality for a custom type, and since it's a one-liner, why
not?

```haskell
derive instance eqExpr :: Eq Expr
derive instance eqBinop :: Eq Binop
derive instance eqUnop :: Eq Unop
derive instance eqLit :: Eq Lit
```

Now we'll define the commands we'll be able to give our calculator:

```haskell
data Cmd = Assign Name Expr | Eval Expr

infix 0 Assign as :=
```

The only new concept here is that of a custom infix operator. We'll use the `:=`
symbol to both denote assignment in our source files, and when we're interacting
with the calculator. We've defined it here to not associate either way (we could
have written `infixr` or `infixl` instead) and to bind weakly, at precedence 0.

Finally we'll define a function to get a textual representation of a literal's
type:

```haskell
typeof :: Lit -> String
typeof (Int _) = "int"
typeof (Bool _) = "bool"
```

When pattern matching on type constructors that take one argument or more, it's
important to use parentheses: if we had instead written `typeof Int _ = ...`
the compiler would yell at us, because it understands this as saying that
`typeof` takes two arguments, but the type signature we gave it only has one.
Also, the underscore here denotes that we don't care what the argument is.
They're useful for drawing attention to the important parts of a pattern match:
in our case, the constructor used.

## Lexing and parsing

### The lexer

The `purescript-parsing` package is unsurpassed for its ability to let us deal
with tokens instead of text. We can essentially give it a specification for our
language and it'll give us back a host of parser combinators that make it easy
to parse our language declaratively:

```haskell
module Token where

import Control.Alt ((<|>))

import Text.Parsing.Parser.Token (LanguageDef, GenLanguageDef(..), TokenParser, makeTokenParser, letter, alphaNum)
import Text.Parsing.Parser.String (char, oneOf)

languageDef :: LanguageDef
languageDef = LanguageDef
  { commentStart: ""
  , commentEnd: ""
  , commentLine: "#"
  , nestedComments: false
  , identStart: letter
  , identLetter: alphaNum <|> char '\''
  , opStart: oneOf ['-', ':', '+', '*', '/', '|', '=', '~', '<']
  , opLetter: oneOf ['=', '|']
  , reservedNames: ["true", "false"]
  , reservedOpNames: [":=", "+", "-", "*", "/", "||", "=", "~", "<"]
  , caseSensitive: true
}

token :: TokenParser
token = makeTokenParser languageDef
```

That's it for this module. The `<|>` combinator for parsers says to use the left
parser first, and if that fails then the second. If you wanted to extend this
language definition to a more advanced language, you might want to have a more
involved definition. The `commentStart` and `commentEnd` fields are for
multi-line comments, denoting the starting and ending markers. In Purescript for
example, those are `{-` and `-}`.

Any field with `Start` in it denotes which characters that parser is allowed to
start with, and the corresponding `Letter` parser denotes which characters the
tail can take.

`reservedNames` holds which strings are reserved keywords, like `module` or
`case` in Purescript.

### The parser

The parser is one of the most exciting modules to write, but it's also
exceedingly easy to make a mistake, even with Purescript's strong type system.

```haskell
module Parser where

import Prelude
import Token (token)
import Syntax (Lit(..), Expr(..), Binop(..), Unop(..), Cmd(..), (:=))

import Control.Alt ((<|>))
import Control.Lazy (fix)

import Data.Functor (($>))
import Data.Identity (Identity)

import Text.Parsing.Parser (Parser), runParser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.Expr (OperatorTable, Assoc(..), Operator(..), buildExprParser)
```

Since the general parser type is sort of complicated, we'll use a type synonym
to make our lives easier:

```haskell
type P a = Parser String a
```

This is actually a nested type synonym, since `Parser` is itself a synonym. All
our parsers from here on out will be of the type `P a` for some `a`.

We'll start by extracting some of the pre-made parsers from our token module:

```haskell
parens :: forall a. P a -> P a
parens = token.parens

reservedOp :: String -> P Unit
reservedOp = token.reservedOp

reserved :: String -> P Unit
reserved = token.reserved

identifier :: P String
identifier = token.identifier
```

Literals are some of the most basic parsers we have:

```haskell
int :: P Lit
int = Int <$> token.integer

bool :: P Lit
bool = reserved "true" $> Bool true <|> reserved "false" $> Bool false
```

The `int` parser uses the `<$>` combinator, which is an infix alias for `map`.
All it does is map the `Int` type constructor to the integer parser from our
token record. The integer parser is one of the parsers we get just from the
language definition, and in fact it doesn't rely on any of the options we gave
in our language definition -- we essentially get the ability to parse integers
"for free".

The `bool` parser is a tad more complicated. We've already met the alt
combinator `<|>`, so the only new things here are how we're using `reserved` and
`$>`. We already defined `reserve` above, and it's actually another parser we
got from the lexer. It checks for whichever argument we give it and returns a
`Unit`. This is because the lexer isn't sure how we're using reserved words --
we might use them purely as syntactic constructs, or they might represent
values, or something else -- so the lexer can only tell us whether one of these
reserved words has occurred or not. It's up to us to determine what to do when
it has, which is where `$>` comes in. Its type signature is
`forall f. Functor f => f a -> b -> f b`. Specialized to our `P` type synonym,
it has the signature `forall a b. P a -> b -> P b`. All it does is parse an `a`,
ignore it, then return a `b` inside a parser.

We're somewhat lucky in that the operators play well with each other, without
needing to use parentheses. Sometimes the precedence won't work out like you're
hoping and you need to parenthesize your expression to get things right.

A `Lit` isn't an `Expr`, but it can be embedded via the `Lit` type constructor:

```haskell
lit :: P Expr
lit = Lit <$> int <|> Lit <$> bool
```

Variables are deceptively simple to deal with:

```haskell
var :: P Expr
var = Var <$> identifier
```

All that's left is to handle operators and parentheses. Because Purescript is
evaluated strictly (in fact, it's evaluated by compiling into Javascript and
then running the output), any parsers we want that can be used recursively will
have to take an argument, unless if that parser comes in the "initial step" of
recursive parsing.

```haskell
term :: P Expr -> P Expr
term p = parens p <|> lit <|> var
```

The sole argument to `term` represents the "overall" `Expr` parser. Here you can
see that using parentheses lines up with how we think of them. That is, our
`term` parser will look for expressions in parentheses first, and only if it
doesn't see a parenthesis will it try to parse a literal value, then a variable.

Before we get to the ultimate `Expr` parser, we have to deal with operators.
We do so through the use of an operator table:

```haskell
table :: OperatorTable Identity String Expr
table =
  [ [ Prefix (reservedOp "~" $> Unop Not)
    , Prefix (reservedOp "-" $> Unop Negate) ]
  , [ Infix (reservedOp "*" $> Binop Mul) AssocLeft
    , Infix (reservedOp "/" $> Binop Div) AssocLeft ]
  , [ Infix (reservedOp "+" $> Binop Add) AssocLeft
    , Infix (reservedOp "-" $> Binop Sub) AssocLeft ]
  , [ Infix (reservedOp "<" $> Binop Less) AssocRight
    , Infix (reservedOp "=" $> Binop Eql) AssocRight ]
  , [ Infix (reservedOp "||" $> Binop Or) AssocRight ] ]
```

An operator table is an array of arrays: the first array in the table
represents those operators with the highest precedence, while the last is for
those with the lowest. As you can see here, our unary operators all have higher
precedence than our binary ones.

Note that for binary operators we also have to give an associativity.

Now we can define the most general `Expr` parser:

```haskell
expr :: P Expr
expr = fix allExprs
  where
    allExprs p = buildExprParser table (term p)
```

Two things are happening here: we're incorporating our operator table into our
expressions, and dealing with recursive parsing in a strict language.

1. The type of `fix :: forall l. Lazy l => (l -> l) -> l`. This gives us the
  least fixed point of a given function. In a lazy language like Haskell, you
  can define it as `fix f = f (fix f)`. Yes, it is related to the y-combinator
  from lambda calculus.
2. `buildExprParser` takes an operator table and a parser, and gives back a new
  parser. Dealing with operators is mostly pain-free. All we really had to think
  about was our operators' precedences and associativity. `buildExprParser`
  takes care of all the plumbing for us.

Lastly, we need to be able to parse calculator commands:

```haskell
def :: P Cmd
def = do
  name <- identifier
  reservedOp ":="
  t <- expr
  pure (name := t)
```

This is the first time we've used `do` notation, but it's simple to understand.
The code reads imperatively: bind "name" to an identifier, check for the
reserved operator ":=", parse an expression and bind it to "t", then give back
"name" assigned to "t".

Evaluation is even simpler:

```haskell
eval :: P Cmd
eval = Eval <$> expr
```

And we combine the two:

```haskell
cmd :: P Cmd
cmd = try def <|> evalExpr
```

The `try` combinator resets the stream in case of failure.

We'll write a function that takes in text as input and gives a `Cmd` as output,
but first a detour through error-handling, since our parser might fail.

## Error handling

Error handling in Purescript can be done purely. This means we'll treat errors
as just normal values, there won't be anything exceptional about them. We've
already considered that our parser might fail. We might also get a type mismatch
when trying to evaluate an expression, or we might reference a variable that
doesn't exist. We'll also include a general-purpose error:

```haskell
module Error where

import Prelude
import Syntax (Lit, Name)

import Data.Either (Either(..))

data Error = ParseError String
           | TypeMismatch Lit String
           | UnknownValue Name
           | TheImpossibleHappened String
```

We'll also want the ability to see our errors as text:

```haskell
instance showError :: Show Error where
  show (ParseError s) = "Parse error: " <> s
  show (TypeMismatch l s) = "Type mismatch: expecting " <> s <> "but found" <> show l
  show (UnknownValue n) = "Unknown value: " <> n
  show (TheImpossibleHappened msg) = "The impossible happened: " <> msg
```

And we'll define a type synonym for dealing with errors:

```haskell
type Expect a = Either Error a
```

The `Either` type is used for values that can be either one of two types: a
type on the `Left` or one on the `Right`. Its definition is
`data Either a b = Left a | Right b`. By convention (and, I suppose, our
own definition) values on the `Left` are considered to be errors.

We'll also define a helper function for throwing errors:

```haskell
throw :: forall a. Error -> Expect a
throw = Left
```

and helper functions for each of the error constructors:

```haskell
parseError :: forall a. String -> Expect a
parseError = throw <<< ParseError

typeMismatch :: forall a. Lit -> String -> Expect a
typeMismatch l = throw <<< TypeMismatch l

unknownValue :: forall a. Name -> Expect a
unknownValue = throw <<< UnknownValue

theImpossibleHappened :: forall a. String -> Expect a
theImpossibleHappened = throw <<< TheImpossibleHappened
```

All of our definitions here make use of function composition. As a consequence,
three of our definitions don't even reference their arguments. The resulting
definitions read smoothly: `parseError` is just defined as throwing a
`ParseError`.

### Back to parsing

Now we can write our function that takes an input `String` and (we expect) gives
us a `Cmd`:

```haskell
parse :: String -> Expect Cmd
parse = case runParser s cmd of
  Left (ParseError e) = parseError e.message
  Right c -> pure c
```

## Evaluation

We get to the guts of our calculator program: evaluation!

```haskell
module Eval where

import Prelude
import Syntax (Expr(..), Binop(..), Unop(..), Lit(..), typeof)
import Error (Expect, typeMismatch, unknownValue)

import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty, lookup)
```

We'll want to keep track of which variables we've defined in an environment,
so we'll use a string map:

```haskell
type Env = StrMap Expr

initEnv :: Env
initEnv = empty
```

Our main evaluation function mostly delegates to helper functions:

```haskell
eval :: Env -> Expr -> Expect Expr
eval env = case _ of
  n@(Lit (Int _)) -> pure n
  b@(Lit (Bool _)) -> pure b
  Var name -> case lookup name env of
    Just val -> pure val
    _ -> unknownValue name
  Binop op e1 e2 -> evalBinop env op e1 e2
  Unop op e -> evalUnop env op e
```

We also want functions to turn a normal Purescript integer/boolean to an
expression in our error-handling type:

```haskell
raiseInt :: Int -> Expect Expr
raiseInt = pure <<< Lit <<< Int

raiseBool :: Boolean -> Expect Expr
raiseBool = pure <<< Lit <<< Bool
```

On to the helper functions:

```haskell
evalBinop :: Env -> Binop -> Expr -> Expr -> Expect Expr
evalBinop env op e1 e2 = case op of
  Add -> evalArithBinop add env e1 e2
  Sub -> evalArithBinop sub env e1 e2
  Mul -> evalArithBinop mul env e1 e2
  Div -> evalArithBinop div env e1 e2
  Or -> evalOr env e1 e2
  Less -> evalLess env e1 e2
  Eql -> evalEql env e1 e2
```

We also define a type synonym so we don't have to type as much. This might not
help so much now, but it's useful should you decide to add support for more
operators.

```haskell
type EvalBinop = Env -> Expr -> Expr -> Expect Expr

evalArithBinop :: (forall s. (Ring s, ModuloSemiring s) => s -> s -> s) -> EvalBinop
evalArithBinop op env e1 e2 = case e1, e2 of
  Lit (Int n), Lit (Int m) -> raiseInt $ op e1 e2
  Lit t@(Bool _), _ -> typeMismatch t "int"
  _, Lit t@(Bool _) -> typeMismatch t "int"
  _, _ -> do
    e <- eval env e1
    e' <- eval env e2
    evalArithBinop op env e e'
```

`evalArithBinop` uses somewhat of an advanced type system feature. The first
argument is a rank 2 type, which means `evalArithBinop` takes in a polymorphic
function as an argument and is allowed to use it polymorphically instead of
implicitly using it as a monomorphic function. If we reduce the signature's rank
the compiler will yell at us:

```haskell
evalArithBinop :: forall s. (Ring s, ModuloSemiring s) => (s -> s -> s) -> EvalBinop
```
    > Could not match type Int with type s0

While we could eschew the `Semiring` class and function for an argument of type
`Int -> Int -> Int`, this more general version will work in case we want to
add support for floating point arithmetic via `Number`s later.

```haskell
evalOr :: EvalBinop
evalOr env e1 e2 = case e1, e2 of
  Lit (Bool p), Lit (Bool q) -> raiseBool $ p || q
  Lit t@(Int _), _ -> typeMismatch t "bool"
  _, Lit t@(Int _) -> typeMismatch t "bool"
  _, _ -> do
    e <- eval env e1
    e' <- eval env e2
    evalOr env e e'

evalLess :: EvalBinop
evalLess env e1 e2 = case e1, e2 of
  Lit (Int n), Lit (Int m) -> raiseBool $ n < m
  Lit (Bool p), Lit (Bool q) -> raiseBool $ p < q
  Lit t1, Lit t2 -> typeMismatch t2 $ typeof t1
  _, _ -> do
    e <- eval env e1
    e' <- eval env e2
    evalLess env e e'

evalEql :: EvalBinop
evalEql env e1 e2 = case e1, e2 of
  Lit x, Lit y -> raiseBool $ x == y
  _, _ -> do
    e <- eval env e1
    e' <- eval env e2
    evalEql env e e'
```

Unary operators are pretty much what you'd expect:

```haskell
type EvalUnop = Env -> Expr -> Expect Expr

evalUnop :: Env -> Unop -> Expr -> Expect Expr
evalUnop env op e = case op of
  Not -> evalNot env e
  Negate -> evalNegate env e

evalNot :: EvalUnop
evalNot env = case _ of
  Lit (Bool b) -> raiseBool $ not b
  Lit l -> typeMismatch l "bool"
  e -> eval env e >>= evalNot env

evalNegate :: EvalUnop
evalNegate env = case _ of
  Lit (Int n) -> raiseInt (-n)
  Lit l -> typeMismatch l "int"
  e -> eval env e >>= evalNegate env
```

Instead of using `do` notation, we use its desugared form, which saves us two
lines. What we've written using `>>=` is equivalent to `do`ing

```haskell
  e -> do
    e' <- eval env e
    evalNegate env e'
```

And that's it!

## A node REPL

We get to the final part of our calculator, which is moving away from the land
of pure functions and getting our hands dirty with some effectful stuff. I've
chosen to show how to implement our calculator as a node.js CLI app, but you
could also use it to power a web calculator on a page.

```haskell
module Main where

import Prelude
import Eval (Env, initEnv, eval)
import Parser (parse)
import Syntax (Cmd(..), (:=), Expr)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.StrMap (toList, alter)
import Data.Tuple (Tuple(..))

import Node.SimpleRepl (Repl, runRepl, setPrompt, readLine, putStrLn)
import Node.ReadLine (READLINE)

main :: forall e. Eff ( console :: CONSOLE, readline :: READLINE | e ) Unit
main = runRepl do
  setPrompt "> "
  loop initEnv
```

The type of `main` says that it has effects (specifically those associated with
using the console and node's readline module) and doesn't return anything of
interest. First we set the prompt to an unfancy `> `, and start the loop with
an empty initial environment.

```haskell
loop :: forall e. Env -> Repl e Unit
loop e = do
  input <- readLine
  case input of
    "quit" -> pure unit
    _ -> do
      { env, str } <- evalCmd env input
      putStrLn str
      loop env
```

In our loop, we'll want to be able to change the environment from command to
command. Thus we give it an explicit `Env` parameter, so that subsequent calls
to `loop` can use a modified environment. We also pattern match on the input,
exiting the loop if we encounter "quit". Otherwise, we pass off the input
and environment to an `evalCmd` function, getting a (possibly changed)
environment and string to print. We print the string, and loop all over again.

```haskell
evalCmd :: forall e. Env -> String -> Repl e { env :: Env, str :: String }
evalCmd e input = case parse input of
  Left err -> pure { env: e, str: show err }
  Right (name := val) -> case eval e val of
    Left err -> pure { env: e, str: show err }
    Right expr -> do
      let env = upsert e name expr
      setPrompt $ pprint env <> "\n> "
      pure { env, str: name <> " defined" }
  Right (Eval expr) -> case eval e expr of
    Left err -> pure { env: e, str: show err }
    Right exp -> pure { env: e, str: "\x1b[34m" <> show exp <> "\x1b[0m" }
```

There are three cases to consider when parsing a command: Either

1. We get a parse error, in which case we return the given environment and show
  the error,
2. we bind a name to an expression. If the expression can't be evaluated
  correctly, we return the same environment and show the error. Otherwise, we
  either update or insert the binding to our environment, update the prompt
  to show a pretty representation of the current bound variables, and return
  the new environment with the message that the new variable has been
  successfully defined, or
3. we evaluate an expression. If there's an error, we show it. Otherwise, we
  show the evaluated expression nested between two ansi codes for blue text.

```haskell
pprint :: Env -> String
pprint e =
  let list = toList e
      untupled = map (\ (Tuple key val) -> k <> " := " <> show v) list
   in intercalate ", " untupled
```

`pprint` uses a `let` binding, which is a way to assign names to expressions
and then use those names in some larger expression. It's a handy way of
shortening what would otherwise be unreadably long lines.

This is also our first use of an anonymous function, otherwise known as a
lambda. Since the `toList` function turns a `StrMap` into a list of key-value
tuples, we want to transform each tuple into a `String`. We do this by mapping
the lambda (which pattern matches on the `Tuple`s in the list) over the list,
then join all the resulting strings with right-padded commas.

```haskell
upsert :: Env -> String -> Expr -> Env
upsert e k v = alter f k e
  where
  f _ = Just v
```

Instead of a `let` binding, we use `where` syntax here. The function argument
to `alter` lets us insert, update or delete a value in a `StrMap`. Pattern
matching on a `Nothing` corresponds to the case of not finding the given key
`String` in the map, and `Just` to the case of when the key exists. If `f`
outputs a `Nothing`, the key will not exist in the new map. If it outputs
`Just x`, the new map will associate the key with `x`.

## Final

Assuming we've done everything correctly, we can test our calculator:

    > pulp run
    * Building project
    psc: No files found using pattern: src/**/*.js
    * Build successful.
    > 2 + 2
    4
    > x := 3
    x defined
    x := 3
    > y := 4
    y defined
    x := 3, y := 4
    > z := 5
    z defined
    x := 3, y := 4, z := 5
    > x*x + y*y = z*z
    true
    x := 3, y := 4, z := 5
    > 


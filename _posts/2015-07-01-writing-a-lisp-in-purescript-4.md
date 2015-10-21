---
layout: post
title: Write Yourself a Lisp (With Purescript!) Part 3
---

## Beginning to evaluate

Let's define a `Show` instance for our values, so we'll be able to get visual representations of them in the terminal.

```purescript
instance showLispVal :: Show LispVal where
  show (Atom s) = s
  show (List (Cons (Atom "quote") (Cons x Nil))) = "'" ++ show x
  show (List xs) = "(" ++ unwordsList xs ++ ")"
  show (DottedList xs x) = "(" ++ unwordsList xs ++ " . " ++ show x ++ ")"
  show (Int n) = show n
  show (String s) = "\"" ++ s ++ "\""
  show (Bool b)
    | b = "true"
    | otherwise = "false"
```

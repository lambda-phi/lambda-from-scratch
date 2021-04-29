# Lλmbda

A simple implementation of
[Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) with
[Hindley-Milner type inference](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) in Elm.

Lambda calculus is a very simple
[Turing complete](https://en.wikipedia.org/wiki/Turing_completeness) computational model.
This makes it very straightforward to evaluate, optimize, translage, and this makes it a very good candidate for a language intermediate representation.

Here are some examples on how to represent different expressions.

```elm
import Lambda exposing (Error, read, write)

readWrite : String -> Result Error String
readWrite txt =
    read txt |> Result.map write

-- Values
readWrite "42"   --> Ok "42"
readWrite "3.14" --> Ok "3.14"

-- Variables
readWrite "x" --> Ok "x"

-- Lambda abstraction
readWrite "λx. y"     --> Ok "λx. y"
readWrite "λx y. z"   --> Ok "λx y. z"
readWrite "λx. λy. z" --> Ok "λx y. z"

-- Application
readWrite "f x"   --> Ok "f x"
readWrite "f x y" --> Ok "f x y"

-- Typed expressions
readWrite "x : a"       --> Ok "x : a"
readWrite "x : Bool"    --> Ok "x : Bool"
readWrite "x : Maybe a" --> Ok "x : Maybe a"

-- Function types
readWrite "a -> b" --> Ok "a -> b"

-- Variable definitions
readWrite "(λx. z) y"       --> Ok "x := y; z"
readWrite "x := y; z"       --> Ok "x := y; z"
readWrite "x := (y : a); z" --> Ok "x : a = y; z"
readWrite "x : a = y; z"    --> Ok "x : a = y; z"
```

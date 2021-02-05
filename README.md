# Lλmbda

A simple implementation of
[Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) with
[Hindley-Milner type inference](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) in Elm.

Lambda calculus is a very simple
[Turing complete](https://en.wikipedia.org/wiki/Turing_completeness) computational model.
This makes it very straightforward to evaluate, optimize, translage, and this makes it a very good candidate for a language intermediate representation.

```elm
import Lambda exposing (Error(..), Expr(..), Type(..), evaluate, read, write)


-- Evaluate a text expression, and return us the result and its type.
eval : String -> Result Error String
eval txt =
    read txt
        |> Result.andThen evaluate
        |> Result.map write


-- Builtin values
eval "42" --> Ok "42"
eval "3.14" --> Ok "3.14"

-- Variables (must be defined in an abstraction)
eval "x" --> Err (VariableNotFound "x")

-- Abstraction
eval "λx.42" --> Ok "λx:a.42"
eval "λx.x" --> Ok "λx:a.x"
eval "λx.y" --> Err (VariableNotFound "y")

-- Application
eval "1 2" --> Err (TypeMismatch IntT (AbsT IntT (T "a")))
eval "λf.f 42" --> Ok "λf:@Int->a.f 42"
eval "(λx.x) 42" --> Ok "42"

-- Variable declaration (syntax sugar)
eval "x=42; x" --> Ok "42"
eval "f=λx.42; f" --> Ok "λx:a.42"
eval "f=λx.42; f 3.14" --> Ok "42"
```

# Lλmbda

A simple implementation of
[Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) with
[Hindley-Milner type inference](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system) in Elm.

Lambda calculus is a very simple
[Turing complete](https://en.wikipedia.org/wiki/Turing_completeness) computational model.
This makes it very straightforward to evaluate, optimize, translage, and this makes it a very good candidate for a language intermediate representation.

Here are some examples on how to represent different expressions.

```elm
-- TODO: enable/fix tests here
True --> True
```

```elm
import Lambda exposing (Error, read, run, write)

readWrite : String -> Result Error String
readWrite txt =
    run (read txt)
        |> Result.map write

-- Values
readWrite "42"   -- Ok "42"
readWrite "3.14" -- Ok "3.14"

-- Variables
readWrite "x" -- Ok "x"

-- Lambda abstraction
readWrite "λx. y"     -- Ok "λx. y"
readWrite "λx y. z"   -- Ok "λx y. z"
readWrite "λx. λy. z" -- Ok "λx y. z"

-- Application
readWrite "f x"   -- Ok "f x"
readWrite "f x y" -- Ok "f x y"

-- -- Typed expressions
-- readWrite "x : a"       -- Ok "x : a"
-- readWrite "x : Bool"    -- Ok "x : Bool"
-- readWrite "x : Maybe a" -- Ok "x : Maybe a"

-- Function types
readWrite "a -> b" -- Ok "a -> b"

-- -- Variable definitions
-- readWrite "(λx. z) y"       -- Ok "x := y; z"
-- readWrite "x := y; z"       -- Ok "x := y; z"
-- readWrite "x := (y : a); z" -- Ok "x : a = y; z"
-- readWrite "x : a = y; z"    -- Ok "x : a = y; z"
```

## Operator precedence

```elm
import Lambda exposing (Expr(..), read, write, withValue)

-- Reading
read "x y z"    -- withValue (App (App (Var "x") (Var "y")) (Var "z"))
read "x y ^ z"  -- withValue (exp (App (Var "x") (Var "y")) (Var "z"))
read "x y * z"  -- withValue (mul (App (Var "x") (Var "y")) (Var "z"))
read "x y + z"  -- withValue (add (App (Var "x") (Var "y")) (Var "z"))
read "x y -> z" -- withValue (Fun (App (Var "x") (Var "y")) (Var "z"))
read "x y : z"  -- withValue (TE (App (Var "x") (Var "y")) (Var "z"))
read "x λy. z"  -- withValue (App (Var "x") (Lam "y" (Var "z")))

read "x ^ y z"    -- withValue (exp (Var "x") (App (Var "y") (Var "z")))
read "x ^ y ^ z"  -- withValue (exp (Var "x") (exp (Var "y") (Var "z")))
read "x ^ y * z"  -- withValue (mul (exp (Var "x") (Var "y")) (Var "z"))
read "x ^ y + z"  -- withValue (add (exp (Var "x") (Var "y")) (Var "z"))
read "x ^ y -> z" -- withValue (Fun (exp (Var "x") (Var "y")) (Var "z"))
read "x ^ y : z"  -- withValue (TE (exp (Var "x") (Var "y")) (Var "z"))
read "x ^ λy. z"  -- withValue (exp (Var "x") (Lam "y" (Var "z")))

read "x * y z"    -- withValue (mul (Var "x") (App (Var "y") (Var "z")))
read "x * y ^ z"  -- withValue (mul (Var "x") (exp (Var "y") (Var "z")))
read "x * y * z"  -- withValue (mul (mul (Var "x") (Var "y")) (Var "z"))
read "x * y + z"  -- withValue (add (mul (Var "x") (Var "y")) (Var "z"))
read "x * y : z"  -- withValue (TE (mul (Var "x") (Var "y")) (Var "z"))
read "x * y -> z" -- withValue (Fun (mul (Var "x") (Var "y")) (Var "z"))
read "x * λy. z"  -- withValue (mul (Var "x") (Lam "y" (Var "z")))

read "x + y z"    -- withValue (add (Var "x") (App (Var "y") (Var "z")))
read "x + y ^ z"  -- withValue (add (Var "x") (exp (Var "y") (Var "z")))
read "x + y * z"  -- withValue (add (Var "x") (mul (Var "y") (Var "z")))
read "x + y + z"  -- withValue (add (add (Var "x") (Var "y")) (Var "z"))
read "x + y -> z" -- withValue (Fun (add (Var "x") (Var "y")) (Var "z"))
read "x + y : z"  -- withValue (TE (add (Var "x") (Var "y")) (Var "z"))
read "x + λy. z"  -- withValue (add (Var "x") (Lam "y" (Var "z")))

read "x -> y z"    -- withValue (Fun (Var "x") (App (Var "y") (Var "z")))
read "x -> y ^ z"  -- withValue (Fun (Var "x") (exp (Var "y") (Var "z")))
read "x -> y * z"  -- withValue (Fun (Var "x") (mul (Var "y") (Var "z")))
read "x -> y + z"  -- withValue (Fun (Var "x") (add (Var "y") (Var "z")))
read "x -> y : z"  -- withValue (TE (Fun (Var "x") (Var "y")) (Var "z"))
read "x -> y -> z" -- withValue (Fun (Var "x") (Fun (Var "y") (Var "z")))
read "x -> λy. z"  -- withValue (Fun (Var "x") (Lam "y" (Var "z")))

read "x : y z"    -- withValue (TE (Var "x") (App (Var "y") (Var "z")))
read "x : y ^ z"  -- withValue (TE (Var "x") (exp (Var "y") (Var "z")))
read "x : y * z"  -- withValue (TE (Var "x") (mul (Var "y") (Var "z")))
read "x : y + z"  -- withValue (TE (Var "x") (add (Var "y") (Var "z")))
read "x : y -> z" -- withValue (TE (Var "x") (Fun (Var "y") (Var "z")))
read "x : y : z"  -- withValue (TE (TE (Var "x") (Var "y")) (Var "z"))
read "x : λy. z"  -- withValue (TE (Var "x") (Lam "y" (Var "z")))

read "λx. y z"    -- withValue (Lam "x" (App (Var "y") (Var "z")))
read "λx. y ^ z"  -- withValue (Lam "x" (exp (Var "y") (Var "z")))
read "λx. y * z"  -- withValue (Lam "x" (mul (Var "y") (Var "z")))
read "λx. y + z"  -- withValue (Lam "x" (add (Var "y") (Var "z")))
read "λx. y -> z" -- withValue (Lam "x" (Fun (Var "y") (Var "z")))
read "λx. y : z"  -- withValue (Lam "x" (TE (Var "y") (Var "z")))
read "λx. λy. z"  -- withValue (Lam "x" (Lam "y" (Var "z")))

read "x + (y + z)" -- withValue (add (Var "x") (add (Var "y") (Var "z")))
read "(x ^ y) ^ z" -- withValue (exp (exp (Var "x") (Var "y")) (Var "z"))

-- Writing
write (App (App (Var "x") (Var "y")) (Var "z")) -- "x y z"
write (exp (App (Var "x") (Var "y")) (Var "z")) -- "x y ^ z"
write (mul (App (Var "x") (Var "y")) (Var "z")) -- "x y * z"
write (add (App (Var "x") (Var "y")) (Var "z")) -- "x y + z"
write (Fun (App (Var "x") (Var "y")) (Var "z")) -- "x y -> z"
write (TE (App (Var "x") (Var "y")) (Var "z"))  -- "x y : z"
write (App (Var "x") (Lam "y" (Var "z")))       -- "x (λy. z)"

write (exp (Var "x") (App (Var "y") (Var "z"))) -- "x ^ y z"
write (exp (Var "x") (exp (Var "y") (Var "z"))) -- "x ^ y ^ z"
write (mul (exp (Var "x") (Var "y")) (Var "z")) -- "x ^ y * z"
write (add (exp (Var "x") (Var "y")) (Var "z")) -- "x ^ y + z"
write (Fun (exp (Var "x") (Var "y")) (Var "z")) -- "x ^ y -> z"
write (TE (exp (Var "x") (Var "y")) (Var "z"))  -- "x ^ y : z"
write (exp (Var "x") (Lam "y" (Var "z")))       -- "x ^ (λy. z)"

write (mul (Var "x") (App (Var "y") (Var "z"))) -- "x * y z"
write (mul (Var "x") (exp (Var "y") (Var "z"))) -- "x * y ^ z"
write (mul (mul (Var "x") (Var "y")) (Var "z")) -- "x * y * z"
write (add (mul (Var "x") (Var "y")) (Var "z")) -- "x * y + z"
write (Fun (mul (Var "x") (Var "y")) (Var "z")) -- "x * y -> z"
write (TE (mul (Var "x") (Var "y")) (Var "z"))  -- "x * y : z"
write (mul (Var "x") (Lam "y" (Var "z")))       -- "x * (λy. z)"

write (add (Var "x") (App (Var "y") (Var "z"))) -- "x + y z"
write (add (Var "x") (exp (Var "y") (Var "z"))) -- "x + y ^ z"
write (add (Var "x") (mul (Var "y") (Var "z"))) -- "x + y * z"
write (add (add (Var "x") (Var "y")) (Var "z")) -- "x + y + z"
write (Fun (add (Var "x") (Var "y")) (Var "z")) -- "x + y -> z"
write (TE (add (Var "x") (Var "y")) (Var "z"))  -- "x + y : z"
write (add (Var "x") (Lam "y" (Var "z")))       -- "x + (λy. z)"

write (Fun (Var "x") (App (Var "y") (Var "z"))) -- "x -> y z"
write (Fun (Var "x") (exp (Var "y") (Var "z"))) -- "x -> y ^ z"
write (Fun (Var "x") (mul (Var "y") (Var "z"))) -- "x -> y * z"
write (Fun (Var "x") (add (Var "y") (Var "z"))) -- "x -> y + z"
write (Fun (Var "x") (Fun (Var "y") (Var "z"))) -- "x -> y -> z"
write (TE (Fun (Var "x") (Var "y")) (Var "z"))  -- "x -> y : z"
write (Fun (Var "x") (Lam "y" (Var "z")))       -- "x -> λy. z"

write (TE (Var "x") (App (Var "y") (Var "z"))) -- "x : y z"
write (TE (Var "x") (exp (Var "y") (Var "z"))) -- "x : y ^ z"
write (TE (Var "x") (mul (Var "y") (Var "z"))) -- "x : y * z"
write (TE (Var "x") (add (Var "y") (Var "z"))) -- "x : y + z"
write (TE (Var "x") (Fun (Var "y") (Var "z"))) -- "x : y -> z"
write (TE (TE (Var "x") (Var "y")) (Var "z"))  -- "x : y : z"
write (TE (Var "x") (Lam "y" (Var "z")))       -- "x : λy. z"

write (Lam "x" (App (Var "y") (Var "z"))) -- "λx. y z"
write (Lam "x" (exp (Var "y") (Var "z"))) -- "λx. y ^ z"
write (Lam "x" (mul (Var "y") (Var "z"))) -- "λx. y * z"
write (Lam "x" (add (Var "y") (Var "z"))) -- "λx. y + z"
write (Lam "x" (TE (Var "y") (Var "z")))  -- "λx. y : z"
write (Lam "x" (Fun (Var "y") (Var "z"))) -- "λx. y -> z"
write (Lam "x" (Lam "y" (Var "z")))       -- "λx y. z"

write (add (Var "x") (add (Var "y") (Var "z"))) -- "x + (y + z)"
write (exp (exp (Var "x") (Var "y")) (Var "z")) -- "(x ^ y) ^ z"
```

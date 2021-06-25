module Lambda exposing
    ( Error(..)
    , Term(..)
    , eval
    )

{-| A simple Lambda calculus implementation with type inference.

TODO: validate that this Elm issue works: <https://github.com/elm/compiler/issues/2186>

[Calculus of constructions](https://en.wikipedia.org/wiki/Calculus_of_constructions)

TODO: figure out how to have a "Formatter" type to write, like "Parser" to read
TODO: rename "Parser" to "Reader" and "Formatter" to "Writer" (?)

-}

import Parser


{-| A Lambda calculus Expression.

<https://www.youtube.com/watch?v=ogXlQf8lDD4>
<https://slides.yowconference.com/yowlambdajam2019/XuanyiChew-AnAlienLambdaCalculus.pdf>
<https://blog.chewxy.com/wp-content/uploads/personal/dissertation31482.pdf>

-- type Maybe a = Just a | Nothing
-- Maybe : = λa. (a -> b) -> b -> b
-- Just : Maybe a = λa. λj n. j a
-- Nothing : Maybe a = λj n. n

-}
type Term
    = Type --               Type        Type of types (Kind)
    | IntT --               Int         Integer type
    | NumT --               Num         Number type
    | Int Int --            42          Integer value
    | Num Float --          3.14        Number value
    | Var String --         x           Variable
    | Fun Term Term --      T1 -> T2    Function type
    | Lam String Term --    λx. t       Lambda abstraction
    | App Term Term --      t1 t2       Application


{-| An error from parsing or evaluation.
-}
type Error
    = SyntaxError Parser.Error
    | CannotApply Term Term


{-|

    import Lambda exposing (Error(..), Term(..), eval)

    -- Types
    eval Type --> Ok ( Type, Type )
    eval IntT --> Ok ( IntT, Type )
    eval NumT --> Ok ( NumT, Type )

    -- Values
    eval (Int 42)   --> Ok ( Int 42, IntT )
    eval (Num 3.14) --> Ok ( Num 3.14, NumT )

    -- Variable
    eval (Var "x") --> Ok ( Var "x", Var "x" )

    -- Function type
    eval (Fun (Var "a") IntT) --> Ok ( Fun (Var "a") IntT, Fun (Var "a") Type )

    -- Lambda abstraction
    eval (Lam "x" Type)                            --> Ok ( Lam "x" Type, Fun (Var "x") Type )
    eval (Lam "x" IntT)                            --> Ok ( Lam "x" IntT, Fun (Var "x") Type )
    eval (Lam "x" (Int 1))                         --> Ok ( Lam "x" (Int 1), Fun (Var "x") IntT )
    eval (Lam "x" (Var "x"))                       --> Ok ( Lam "x" (Var "x"), Fun (Var "x") (Var "x") )
    eval (Lam "x" (Fun (Var "x") IntT))            --> Ok ( Lam "x" (Fun (Var "x") IntT), Fun (Var "x") (Fun (Var "x") Type) )
    eval (Lam "x" (Lam "y" (Var "x")))             --> Ok ( Lam "x" (Lam "y" (Var "x")), Fun (Var "x") (Fun (Var "y") (Var "x")) )
    eval (Lam "x" (App (Lam "y" (Int 1)) (Int 2))) --> Ok ( Lam "x" (Int 1), Fun (Var "x") IntT )

    -- Application
    eval (App Type (Int 1))                                        --> Err (CannotApply Type (Int 1))
    eval (App IntT (Int 1))                                        --> Err (CannotApply IntT (Int 1))
    eval (App (Int 0) (Int 1))                                     --> Err (CannotApply (Int 0) (Int 1))
    eval (App (Var "x") (Int 1))                                   --> Err (CannotApply (Var "x") (Int 1))
    eval (App (Fun IntT Type) (Int 1))                             --> Err (CannotApply (Fun IntT Type) (Int 1))
    eval (App (Lam "x" Type) (Int 1))                              --> Ok ( Type, Type )
    eval (App (Lam "x" IntT) (Int 1))                              --> Ok ( IntT, Type )
    eval (App (Lam "x" (Int 0)) (Int 1))                           --> Ok ( Int 0, IntT )
    eval (App (Lam "x" (Var "x")) (Int 1))                         --> Ok ( Int 1, IntT )
    eval (App (Lam "x" (Var "y")) (Int 1))                         --> Ok ( Var "y", Var "y" )
    eval (App (Lam "x" (Fun (Var "x") Type)) (Int 1))              --> Ok ( Fun (Int 1) Type, Fun IntT Type )
    eval (App (Lam "x" (Lam "y" (Var "x"))) (Int 1))               --> Ok ( Lam "y" (Int 1), Fun (Var "y") IntT )
    eval (App (Lam "x" (App (Lam "y" (Int 0)) Type)) (Int 1))      --> Ok ( Int 0, IntT )
    eval (App (App (Lam "x" (Lam "y" (Var "x"))) (Int 1)) (Int 2)) --> Ok ( Int 1, IntT )

-}
eval : Term -> Result Error ( Term, Term )
eval term =
    case term of
        Type ->
            Ok ( Type, Type )

        IntT ->
            Ok ( IntT, Type )

        NumT ->
            Ok ( NumT, Type )

        Int n ->
            Ok ( Int n, IntT )

        Num n ->
            Ok ( Num n, NumT )

        Var x ->
            Ok ( Var x, Var x )

        Fun term1 term2 ->
            Result.map2
                (\( t1, k1 ) ( t2, k2 ) -> ( Fun t1 t2, Fun k1 k2 ))
                (eval term1)
                (eval term2)

        Lam x body ->
            Result.map
                (\( t, k ) -> ( Lam x t, Fun (Var x) k ))
                (eval (App (Lam x body) (Var x)))

        App (Lam x (Fun term1 term2)) value ->
            eval (Fun (App (Lam x term1) value) (App (Lam x term2) value))

        App (Lam x (Lam y body)) value ->
            eval (Lam y (App (Lam x body) value))

        App (Lam x (App (Lam y body) valueY)) valueX ->
            Result.andThen
                (\( valY, _ ) -> eval (App (Lam y body) valY))
                (eval (App (Lam x valueY) valueX))

        App (Lam x body) value ->
            if body == Var x then
                eval value

            else
                eval body

        App ((App _ _) as app) t2 ->
            Result.andThen
                (\( t1, _ ) -> eval (App t1 t2))
                (eval app)

        App t1 t2 ->
            Err (CannotApply t1 t2)

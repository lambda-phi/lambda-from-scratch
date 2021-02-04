module Lambda.Write exposing (type_, expression)

{-| Write Lambda types and expressions.

@docs type_, expression

-}

import Lambda exposing (Error(..), Expr(..), Type(..))


{-| Writes a Lambda type into a text representation.

    import Lambda exposing (Type(..))
    import Lambda.Write

    -- Builtin types
    Lambda.Write.type_ IntType --> "@Int"
    Lambda.Write.type_ NumType --> "@Num"

    -- Named types
    Lambda.Write.type_ (Type "T") --> "T"

    -- Abstraction types
    Lambda.Write.type_ (AbsType (Type "A") (Type "B")) --> "A->B"
    Lambda.Write.type_ (AbsType (Type "A") (AbsType (Type "B") (Type "C"))) --> "A->B->C"
    Lambda.Write.type_ (AbsType (AbsType (Type "A") (Type "B")) (Type "C")) --> "(A->B)->C"

-}
type_ : Type -> String
type_ typ =
    case typ of
        IntType ->
            "@Int"

        NumType ->
            "@Num"

        Type name ->
            name

        AbsType ((AbsType _ _) as t1) t2 ->
            "(" ++ type_ t1 ++ ")->" ++ type_ t2

        AbsType t1 t2 ->
            type_ t1 ++ "->" ++ type_ t2


{-| Writes a Lambda expression into a text representation.

    import Lambda exposing (Expr(..))
    import Lambda.Write

    -- Values
    Lambda.Write.expression (Int 42) --> "42"
    Lambda.Write.expression (Num 3.14) --> "3.14"

    -- Variables
    Lambda.Write.expression (Var "x") --> "x"

    -- Abstractions
    Lambda.Write.expression (Abs "x" (Var "y")) --> "λx.y"
    Lambda.Write.expression (Abs "x" (Abs "y" (Var "z"))) --> "λx.λy.z"

    -- Applications
    Lambda.Write.expression (App (Var "f") (Var "x")) --> "f x"
    Lambda.Write.expression (App (App (Var "f") (Var "x")) (Var "y")) --> "f x y"
    Lambda.Write.expression (App (Var "f") (App (Var "x") (Var "y"))) --> "f (x y)"
    Lambda.Write.expression (Abs "x" (App (Var "y") (Var "z"))) --> "λx.y z"

    -- Variable definitions (syntax sugar)
    Lambda.Write.expression (App (Abs "x" (Var "z")) (Var "y")) --> "x=y; z"

-}
expression : Expr -> String
expression expr =
    case expr of
        Int value ->
            String.fromInt value

        Num value ->
            String.fromFloat value

        Var name ->
            name

        Abs name outE ->
            "λ" ++ name ++ "." ++ expression outE

        App absE ((App _ _) as argE) ->
            expression absE ++ " (" ++ expression argE ++ ")"

        App (Abs name outE) value ->
            name ++ "=" ++ expression value ++ "; " ++ expression outE

        App absE argE ->
            expression absE ++ " " ++ expression argE

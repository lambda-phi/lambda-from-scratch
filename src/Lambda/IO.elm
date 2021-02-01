module Lambda.IO exposing (read, readType, readTyped, write, writeType, writeTyped)

import Lambda exposing (Expr(..), Type(..), TypedExpr(..))
import Parser exposing (Error, Parser, drop, map, oneOf, parse, succeed, take, textOf)
import Parser.Char exposing (char, digit, letter)
import Parser.Common exposing (int, number, spaces, text)
import Parser.Expression exposing (Operator, fromRight, inbetween, term)
import Parser.Sequence exposing (concat, exactly, zeroOrMore)



-- TYPES


{-|

    import Lambda exposing (Type(..))

    -- Builtin types
    readType "@Int" --> Ok IntType
    readType "@Num" --> Ok NumType

    -- Named types
    readType "T" --> Ok (Type "T")

    -- Abstraction types
    readType "A->B" --> Ok (AbsType (Type "A") (Type "B"))
    readType "A -> B -> C" --> Ok (AbsType (Type "A") (AbsType (Type "B") (Type "C")))
    readType "(A -> B) -> C" --> Ok (AbsType (AbsType (Type "A") (Type "B")) (Type "C"))
    readType "( A -> B ) -> C" --> Ok (AbsType (AbsType (Type "A") (Type "B")) (Type "C"))

    -- Invalid types
    readType "->A" |> Result.toMaybe --> Nothing

-}
readType : String -> Result Error Type
readType txt =
    parse txt type_


{-|

    import Lambda exposing (Type(..))

    -- Builtin types
    writeType IntType --> "@Int"
    writeType NumType --> "@Num"

    -- Named types
    writeType (Type "T") --> "T"

    -- Abstraction types
    writeType (AbsType (Type "A") (Type "B")) --> "A->B"
    writeType (AbsType (Type "A") (AbsType (Type "B") (Type "C"))) --> "A->B->C"
    writeType (AbsType (AbsType (Type "A") (Type "B")) (Type "C")) --> "(A->B)->C"

-}
writeType : Type -> String
writeType typ =
    case typ of
        IntType ->
            "@Int"

        NumType ->
            "@Num"

        Type name ->
            name

        AbsType ((AbsType _ _) as t1) t2 ->
            "(" ++ writeType t1 ++ ")->" ++ writeType t2

        AbsType t1 t2 ->
            writeType t1 ++ "->" ++ writeType t2



-- EXPRESSIONS


{-|

    import Lambda exposing (Expr(..))

    -- Values
    read "42" --> Ok (Int 42)
    read "3.14" --> Ok (Num 3.14)

    -- Variables
    read "x" --> Ok (Var "x")

    -- Abstractions
    read "λx.y" --> Ok (Abs "x" (Var "y"))
    read "λx.λy.z" --> Ok (Abs "x" (Abs "y" (Var "z")))

    -- Applications
    read "f x" --> Ok (App (Var "f") (Var "x"))
    read "f x y" --> Ok (App (App (Var "f") (Var "x")) (Var "y"))
    read "f (x y)" --> Ok (App (Var "f") (App (Var "x") (Var "y")))
    read "λx.y z" --> Ok (Abs "x" (App (Var "y") (Var "z")))
    read "(λx.y) z" --> Ok (App (Abs "x" (Var "y")) (Var "z"))

-}
read : String -> Result Error Expr
read txt =
    parse txt expression


{-|

    import Lambda exposing (Expr(..))

    -- Values
    write (Int 42) --> "42"
    write (Num 3.14) --> "3.14"

    -- Variables
    write (Var "x") --> "x"

    -- Abstractions
    write (Abs "x" (Var "y")) --> "λx.y"
    write (Abs "x" (Abs "y" (Var "z"))) --> "λx.λy.z"

    -- Applications
    write (App (Var "f") (Var "x")) --> "f x"
    write (App (App (Var "f") (Var "x")) (Var "y")) --> "f x y"
    write (App (Var "f") (App (Var "x") (Var "y"))) --> "f (x y)"
    write (Abs "x" (App (Var "y") (Var "z"))) --> "λx.y z"
    write (App (Abs "x" (Var "y")) (Var "z")) --> "(λx.y) z"

-}
write : Expr -> String
write expr =
    case expr of
        Int value ->
            String.fromInt value

        Num value ->
            String.fromFloat value

        Var name ->
            name

        Abs name outE ->
            "λ" ++ name ++ "." ++ write outE

        App absE ((App _ _) as argE) ->
            write absE ++ " (" ++ write argE ++ ")"

        App ((Abs _ _) as absE) argE ->
            "(" ++ write absE ++ ") " ++ write argE

        App absE argE ->
            write absE ++ " " ++ write argE



-- TYPED EXPRESSIONS


{-|

    import Lambda exposing (Type(..), TypedExpr(..))

    -- Values
    readTyped "42" --> Ok (TInt 42)
    readTyped "3.14" --> Ok (TNum 3.14)

    -- Variables
    readTyped "x:a" --> Ok (TVar "x" (Type "a"))

    -- Abstractions
    readTyped "λx:a.y:b" --> Ok (TAbs ("x", Type "a") (TVar "y" (Type "b")))
    readTyped "λx:a.λy:b.z:c" --> Ok (TAbs ("x", Type "a") (TAbs ("y", Type "b") (TVar "z" (Type "c"))))

    -- Applications
    readTyped "f:a x:b" --> Ok (TApp (TVar "f" (Type "a")) (TVar "x" (Type "b")))
    readTyped "f:a x:b y:c" --> Ok (TApp (TApp (TVar "f" (Type "a")) (TVar "x" (Type "b"))) (TVar "y" (Type "c")))
    readTyped "f:a (x:b y:c)" --> Ok (TApp (TVar "f" (Type "a")) (TApp (TVar "x" (Type "b")) (TVar "y" (Type "c"))))
    readTyped "λx:a.y:b z:c" --> Ok (TAbs ("x", Type "a") (TApp (TVar "y" (Type "b")) (TVar "z" (Type "c"))))
    readTyped "(λx:a.y:b) z:c" --> Ok (TApp (TAbs ("x", Type "a") (TVar "y" (Type "b"))) (TVar "z" (Type "c")))

-}
readTyped : String -> Result Error TypedExpr
readTyped txt =
    parse txt typedExpression


{-|

    import Lambda exposing (Type(..), TypedExpr(..))

    -- Values
    writeTyped (TInt 42) --> "42"
    writeTyped (TNum 3.14) --> "3.14"

    -- Variables
    writeTyped (TVar "x" (Type "a")) --> "x:a"

    -- Abstractions
    writeTyped (TAbs ("x", Type "a") (TVar "y" (Type "b"))) --> "λx:a.y:b"
    writeTyped (TAbs ("x", Type "a") (TAbs ("y", Type "b") (TVar "z" (Type "c")))) --> "λx:a.λy:b.z:c"

    -- Applications
    writeTyped (TApp (TVar "f" (Type "a")) (TVar "x" (Type "b"))) --> "f:a x:b"
    writeTyped (TApp (TApp (TVar "f" (Type "a")) (TVar "x" (Type "b"))) (TVar "y" (Type "c"))) --> "f:a x:b y:c"
    writeTyped (TApp (TVar "f" (Type "a")) (TApp (TVar "x" (Type "b")) (TVar "y" (Type "c")))) --> "f:a (x:b y:c)"
    writeTyped (TAbs ("x", Type "a") (TApp (TVar "y" (Type "b")) (TVar "z" (Type "c")))) --> "λx:a.y:b z:c"
    writeTyped (TApp (TAbs ("x", Type "a") (TVar "y" (Type "b"))) (TVar "z" (Type "c"))) --> "(λx:a.y:b) z:c"

-}
writeTyped : TypedExpr -> String
writeTyped typedExpr =
    case typedExpr of
        TInt value ->
            String.fromInt value

        TNum value ->
            String.fromFloat value

        TVar name typ ->
            name ++ ":" ++ writeType typ

        TAbs ( name, typ ) outE ->
            "λ" ++ name ++ ":" ++ writeType typ ++ "." ++ writeTyped outE

        TApp absE ((TApp _ _) as argE) ->
            writeTyped absE ++ " (" ++ writeTyped argE ++ ")"

        TApp ((TAbs _ _) as absE) argE ->
            "(" ++ writeTyped absE ++ ") " ++ writeTyped argE

        TApp absE argE ->
            writeTyped absE ++ " " ++ writeTyped argE



-- PARSERS


identifier : Parser String
identifier =
    concat
        [ exactly 1 letter
        , zeroOrMore (oneOf [ letter, digit, char '_' ])
        ]
        |> textOf


type_ : Parser Type
type_ =
    Parser.Expression.expression
        [ [ fromRight (text "->") AbsType ]
        , [ inbetween (char '(') (char ')') identity
          , term (map Type identifier)
          , term (map (\_ -> IntType) (text "@Int"))
          , term (map (\_ -> NumType) (text "@Num"))
          ]
        ]


expression : Parser Expr
expression =
    let
        application : Operator Expr
        application =
            Parser.Expression.InfixFromLeft
                (\expr ->
                    succeed (\right left -> App left right)
                        |> drop spaces
                        |> take expr
                )

        abstraction : Operator Expr
        abstraction =
            Parser.Expression.Prefix
                (\expr ->
                    succeed Abs
                        |> drop (char 'λ')
                        |> drop spaces
                        |> take identifier
                        |> drop spaces
                        |> drop (text ".")
                        |> drop spaces
                        |> take expr
                )
    in
    Parser.Expression.expression
        [ [ application ]
        , [ abstraction ]
        , [ inbetween (char '(') (char ')') identity
          , term (map Int int)
          , term (map Num number)
          , term (map Var identifier)
          ]
        ]


typedExpression : Parser TypedExpr
typedExpression =
    let
        variable : Parser TypedExpr
        variable =
            succeed TVar
                |> take identifier
                |> drop spaces
                |> drop (char ':')
                |> drop spaces
                |> take type_

        application : Operator TypedExpr
        application =
            Parser.Expression.InfixFromLeft
                (\expr ->
                    succeed (\right left -> TApp left right)
                        |> drop spaces
                        |> take expr
                )

        abstraction : Operator TypedExpr
        abstraction =
            Parser.Expression.Prefix
                (\expr ->
                    succeed (\name typ -> TAbs ( name, typ ))
                        |> drop (char 'λ')
                        |> drop spaces
                        |> take identifier
                        |> drop spaces
                        |> drop (char ':')
                        |> drop spaces
                        |> take type_
                        |> drop spaces
                        |> drop (text ".")
                        |> drop spaces
                        |> take expr
                )
    in
    Parser.Expression.expression
        [ [ application ]
        , [ abstraction ]
        , [ inbetween (char '(') (char ')') identity
          , term (map TInt int)
          , term (map TNum number)
          , term variable
          ]
        ]

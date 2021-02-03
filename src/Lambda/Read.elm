module Lambda.Read exposing (expression, type_)

import Lambda exposing (Error(..), Expr(..), Type(..))
import Parser exposing (Parser, drop, map, oneOf, parse, succeed, take, textOf)
import Parser.Char exposing (char, digit, letter)
import Parser.Common exposing (int, number, spaces, text)
import Parser.Expression exposing (Operator, fromRight, inbetween, term)
import Parser.Sequence exposing (concat, exactly, zeroOrMore)


{-|

    import Lambda exposing (Type(..))
    import Lambda.Read

    -- Builtin types
    Lambda.Read.type_ "@Int" --> Ok IntType
    Lambda.Read.type_ "@Num" --> Ok NumType

    -- Named types
    Lambda.Read.type_ "T" --> Ok (Type "T")

    -- Abstraction types
    Lambda.Read.type_ "A->B" --> Ok (AbsType (Type "A") (Type "B"))
    Lambda.Read.type_ "A -> B -> C" --> Ok (AbsType (Type "A") (AbsType (Type "B") (Type "C")))
    Lambda.Read.type_ "(A -> B) -> C" --> Ok (AbsType (AbsType (Type "A") (Type "B")) (Type "C"))
    Lambda.Read.type_ "( A -> B ) -> C" --> Ok (AbsType (AbsType (Type "A") (Type "B")) (Type "C"))

    -- Invalid types
    Lambda.Read.type_ "->A" |> Result.toMaybe --> Nothing

-}
type_ : String -> Result Error Type
type_ txt =
    parse txt typeParser |> Result.mapError SyntaxError


{-|

    import Lambda exposing (Expr(..))
    import Lambda.Read

    -- Values
    Lambda.Read.expression "42" --> Ok (Int 42)
    Lambda.Read.expression "3.14" --> Ok (Num 3.14)

    -- Variables
    Lambda.Read.expression "x" --> Ok (Var "x")

    -- Abstractions
    Lambda.Read.expression "λx.y" --> Ok (Abs "x" (Var "y"))
    Lambda.Read.expression "λx.λy.z" --> Ok (Abs "x" (Abs "y" (Var "z")))

    -- Applications
    Lambda.Read.expression "f x" --> Ok (App (Var "f") (Var "x"))
    Lambda.Read.expression "f x y" --> Ok (App (App (Var "f") (Var "x")) (Var "y"))
    Lambda.Read.expression "f (x y)" --> Ok (App (Var "f") (App (Var "x") (Var "y")))
    Lambda.Read.expression "λx.y z" --> Ok (Abs "x" (App (Var "y") (Var "z")))
    Lambda.Read.expression "(λx.y) z" --> Ok (App (Abs "x" (Var "y")) (Var "z"))

    -- Variable definitions (syntax sugar)
    Lambda.Read.expression "x=y; z" --> Ok (App (Abs "x" (Var "z")) (Var "y"))

-}
expression : String -> Result Error Expr
expression txt =
    parse txt expressionParser |> Result.mapError SyntaxError



-- Parsers


identifier : Parser String
identifier =
    concat
        [ exactly 1 letter
        , zeroOrMore (oneOf [ letter, digit, char '_' ])
        ]
        |> textOf


typeParser : Parser Type
typeParser =
    Parser.Expression.expression
        [ [ fromRight (text "->") AbsType ]
        , [ inbetween (char '(') (char ')') identity
          , term (map Type identifier)
          , term (map (\_ -> IntType) (text "@Int"))
          , term (map (\_ -> NumType) (text "@Num"))
          ]
        ]


expressionParser : Parser Expr
expressionParser =
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

        variableDefinition : Operator Expr
        variableDefinition =
            Parser.Expression.Prefix
                (\expr ->
                    succeed (\name value outE -> App (Abs name outE) value)
                        |> take identifier
                        |> drop spaces
                        |> drop (char '=')
                        |> drop spaces
                        |> take expr
                        |> drop spaces
                        |> drop (char ';')
                        |> drop spaces
                        |> take expr
                )
    in
    Parser.Expression.expression
        [ [ application ]
        , [ abstraction ]
        , [ inbetween (char '(') (char ')') identity
          , variableDefinition
          , term (map Int int)
          , term (map Num number)
          , term (map Var identifier)
          ]
        ]

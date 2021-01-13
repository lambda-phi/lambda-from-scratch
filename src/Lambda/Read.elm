module Lambda.Read exposing (expression, typeName, type_, variableName)

import Lambda exposing (Expression(..), Type(..))
import Parser exposing (Parser, andThenIgnore, andThenKeep, drop, map, map2, oneOf, succeed, take, textOf)
import Parser.Char exposing (char, digit, letter, lowercase, space, uppercase)
import Parser.Common exposing (int, number, spaces, text)
import Parser.Expression exposing (Operator(..), fromLeft, fromRight, inbetween, prefix, term)
import Parser.Sequence exposing (concat, exactly, oneOrMore, split, zeroOrMore, zeroOrOne)


{-|

    import Parser exposing (parse)

    -- Valid names
    parse "A" typeName --> Ok "A"
    parse "MyType" typeName --> Ok "MyType"
    parse "My_Type2" typeName --> Ok "My_Type2"

    -- Invalid names
    parse "a" typeName |> Result.toMaybe --> Nothing
    parse "myType" typeName |> Result.toMaybe --> Nothing
    parse " A" typeName |> Result.toMaybe --> Nothing

-}
typeName : Parser String
typeName =
    concat
        [ exactly 1 uppercase
        , zeroOrMore (oneOf [ letter, digit, char '_' ])
        ]
        |> textOf


{-|

    import Parser exposing (parse)

    -- Valid names
    parse "a" variableName --> Ok "a"
    parse "myVariable" variableName --> Ok "myVariable"
    parse "my_variable2" variableName --> Ok "my_variable2"

    -- Invalid names
    parse "A" variableName |> Result.toMaybe --> Nothing
    parse "MyVariable" variableName |> Result.toMaybe --> Nothing
    parse " a" variableName |> Result.toMaybe --> Nothing

-}
variableName : Parser String
variableName =
    concat
        [ exactly 1 lowercase
        , zeroOrMore (oneOf [ letter, digit, char '_' ])
        ]
        |> textOf


{-|

    import Lambda exposing (Type(..))
    import Parser exposing (parse)

    -- Named types
    parse "MyType" type_ --> Ok (Type "MyType")

    -- Generic types
    parse "myType" type_ --> Ok (GenericType "myType")

    -- Lambda types
    parse "A->B" type_ --> Ok (LambdaType (Type "A") (Type "B"))
    parse "A -> B -> C" type_ --> Ok (LambdaType (Type "A") (LambdaType (Type "B") (Type "C")))
    parse "(A -> B) -> C" type_ --> Ok (LambdaType (LambdaType (Type "A") (Type "B")) (Type "C"))
    parse "( A -> B ) -> C" type_ --> Ok (LambdaType (LambdaType (Type "A") (Type "B")) (Type "C"))

    -- Invalid types
    parse "->A" type_ |> Result.toMaybe --> Nothing

-}
type_ : Parser Type
type_ =
    Parser.Expression.expression
        [ [ fromRight (text "->") LambdaType ]
        , [ inbetween (char '(') (char ')') identity
          , term (map Type typeName)
          , term (map GenericType variableName)
          ]
        ]


{-|

    import Lambda exposing (Expression(..), Type(..))
    import Parser exposing (parse)

    -- Numeric values
    parse "42" expression --> Ok (Integer 42)
    parse "3.14" expression --> Ok (Number 3.14)

    -- Variables
    parse "x" expression --> Ok (Variable "x")
    parse "myVariable" expression --> Ok (Variable "myVariable")

    -- Lambda definitions
    parse "x:Int=>x" expression --> Ok (Lambda ("x", Type "Int") (Variable "x"))
    parse "x : Int => x" expression --> Ok (Lambda ("x", Type "Int") (Variable "x"))

    -- Lambda applications
    parse "f x" expression --> Ok (Apply (Variable "f") (Variable "x"))
    parse "f x y" expression --> Ok (Apply (Apply (Variable "f") (Variable "x")) (Variable "y"))
    parse "f (x y)" expression --> Ok (Apply (Variable "f") (Apply (Variable "x") (Variable "y")))
    parse "f ( x y )" expression --> Ok (Apply (Variable "f") (Apply (Variable "x") (Variable "y")))

    -- Built-in functions
    parse "@nop:Int;" expression --> Ok (Builtin "nop" (Type "Int") [])
    parse "@neg:Int 1;" expression --> Ok (Builtin "neg" (Type "Int") [Integer 1])
    parse "@add:Int 1 2;" expression --> Ok (Builtin "add" (Type "Int") [Integer 1, Integer 2])
    parse "@ add : Int 1 2 ;" expression --> Ok (Builtin "add" (Type "Int") [Integer 1, Integer 2])

-}
expression : Parser Expression
expression =
    let
        lambda : Operator Expression
        lambda =
            Prefix
                (\expr ->
                    succeed (\var typ -> Lambda ( var, typ ))
                        |> take variableName
                        |> drop spaces
                        |> drop (char ':')
                        |> drop spaces
                        |> take type_
                        |> drop spaces
                        |> drop (text "=>")
                        |> drop spaces
                        |> take expr
                )

        apply : Operator Expression
        apply =
            InfixFromLeft
                (\expr ->
                    succeed (\right left -> Apply left right)
                        |> drop (oneOrMore space)
                        |> take expr
                )

        builtin : Operator Expression
        builtin =
            Prefix
                (\expr ->
                    succeed Builtin
                        |> drop (char '@')
                        |> drop spaces
                        |> take variableName
                        |> drop spaces
                        |> drop (char ':')
                        |> drop spaces
                        |> take type_
                        |> drop spaces
                        |> take (zeroOrMore (expr |> andThenIgnore spaces))
                        |> drop (char ';')
                )
    in
    Parser.Expression.expression
        [ [ lambda
          , apply
          , builtin
          ]
        , [ inbetween (char '(') (char ')') identity
          , term (map Integer int)
          , term (map Number number)
          , term (map Variable variableName)
          ]
        ]

module Lambda exposing
    ( Type(..), Expr(..), TypedExpr(..), Error(..)
    , evaluate
    , read, write, writeType
    )

{-| A simple Lambda calculus implementation with type inference.

@docs Type, Expr, TypedExpr, Error

@docs evaluate

-}

import Dict exposing (Dict)
import DisjointSet exposing (DisjointSet)
import Parser exposing (Parser, drop, map, oneOf, parse, succeed, take, textOf)
import Parser.Char exposing (char, digit, letter)
import Parser.Common exposing (int, number, spaces, text)
import Parser.Expression exposing (Operator, fromRight, inbetween, term)
import Parser.Sequence exposing (concat, exactly, zeroOrMore)
import Set exposing (Set)


{-| A Lambda calculus type.
-}
type Type
    = T String -- a
    | IntT -- @Int
    | NumT -- @Num
    | AbsT Type Type -- a -> b


{-| A Lambda calculus expression.
-}
type Expr
    = IntE Int -- 42
    | NumE Float -- 3.14
    | VarE String -- x
    | AbsE String Expr -- λx.x
    | AppE Expr Expr -- f x


{-| A Lambda calculus typed expression.
-}
type TypedExpr
    = Int Int -- 42
    | Num Float -- 3.14
    | Var String Type -- x:a
    | Abs ( String, Type ) TypedExpr -- λx:a.x:a
    | App TypedExpr TypedExpr Type -- (f:a->b) (x:a)


{-| An error from parsing or evaluation.
-}
type Error
    = SyntaxError Parser.Error
    | VariableNotFound String
    | TypeMismatch Type Type


{-| Reads a Lambda expression from a text representation.

    import Lambda exposing (Expr(..), read)

    -- Values
    read "42" --> Ok (IntE 42)
    read "3.14" --> Ok (NumE 3.14)

    -- Variables
    read "x" --> Ok (VarE "x")

    -- Abstraction
    read "λx.y" --> Ok (AbsE "x" (VarE "y"))
    read "λx.λy.z" --> Ok (AbsE "x" (AbsE "y" (VarE "z")))

    -- Application
    read "f x" --> Ok (AppE (VarE "f") (VarE "x"))
    read "f x y" --> Ok (AppE (AppE (VarE "f") (VarE "x")) (VarE "y"))
    read "f (x y)" --> Ok (AppE (VarE "f") (AppE (VarE "x") (VarE "y")))
    read "λx.y z" --> Ok (AbsE "x" (AppE (VarE "y") (VarE "z")))
    read "(λx.y) z" --> Ok (AppE (AbsE "x" (VarE "y")) (VarE "z"))

    -- Variable definition (syntax sugar)
    read "x=y; z" --> Ok (AppE (AbsE "x" (VarE "z")) (VarE "y"))

-}
read : String -> Result Error Expr
read txt =
    parse txt expressionParser |> Result.mapError SyntaxError


{-| Writes a Lambda type into a text representation.

    import Lambda exposing (Type(..), writeType)

    -- Builtin types
    writeType IntT --> "@Int"
    writeType NumT --> "@Num"

    -- Named types
    writeType (T "T") --> "T"

    -- Abstraction types
    writeType (AbsT (T "A") (T "B")) --> "A->B"
    writeType (AbsT (T "A") (AbsT (T "B") (T "C"))) --> "A->B->C"
    writeType (AbsT (AbsT (T "A") (T "B")) (T "C")) --> "(A->B)->C"

-}
writeType : Type -> String
writeType typ =
    case typ of
        T name ->
            name

        IntT ->
            "@Int"

        NumT ->
            "@Num"

        AbsT ((AbsT _ _) as t1) t2 ->
            "(" ++ writeType t1 ++ ")->" ++ writeType t2

        AbsT t1 t2 ->
            writeType t1 ++ "->" ++ writeType t2


{-| Writes a Lambda expression into a text representation.

    import Lambda exposing (Expr(..), Type(..), write)

    -- Values
    write (Int 42) --> "42"
    write (Num 3.14) --> "3.14"

    -- Variables
    write (Var "x" (T "a")) --> "x"

    -- Abstraction
    write (Abs ("x", T "a") (Var "y" (T "b"))) --> "λx:a.y"
    write (Abs ("x", T "a") (Abs ("y", T "b") (Var "z" (T "c")))) --> "λx:a.λy:b.z"

    -- Application
    write (App (Var "f" (T "a")) (Var "x" (T "b")) (T "c")) --> "f x"
    write (App (App (Var "f" (T "a")) (Var "x" (T "b")) (T "c")) (Var "y" (T "d")) (T "e")) --> "f x y"
    write (App (Var "f" (T "a")) (App (Var "x" (T "b")) (Var "y" (T "c")) (T "d")) (T "d")) --> "f (x y)"
    write (Abs ("x", T "a") (App (Var "y" (T "b")) (Var "z" (T "c")) (T "d"))) --> "λx:a.y z"

    -- Variable definition (syntax sugar)
    write (App (Abs ("x", T "a") (Var "z" (T "c"))) (Var "y" (T "b")) (T "c")) --> "x:a=y; z"

-}
write : TypedExpr -> String
write expr =
    case expr of
        Int value ->
            String.fromInt value

        Num value ->
            String.fromFloat value

        Var name _ ->
            name

        Abs ( name, inT ) out ->
            "λ" ++ name ++ ":" ++ writeType inT ++ "." ++ write out

        App abs ((App _ _ _) as arg) _ ->
            write abs ++ " (" ++ write arg ++ ")"

        App (Abs ( name, inT ) out) value _ ->
            name ++ ":" ++ writeType inT ++ "=" ++ write value ++ "; " ++ write out

        App abs arg _ ->
            write abs ++ " " ++ write arg


{-| Evaluates an expression and returns either an `(Expr, Type)` pair, or an `Error`.

    import Lambda exposing (Error(..), Expr(..), TypedExpr(..), evaluate)

    -- Builtin values
    evaluate (IntE 42) --> Ok (Int 42)
    evaluate (NumE 3.14) --> Ok (Num 3.14)

    -- Variable
    evaluate (VarE "x") --> Err (VariableNotFound "x")

    -- Abstraction
    evaluate (AbsE "x" (VarE "x")) --> Ok (Abs ("x", T "a") (Var "x" (T "a")))

    -- Application
    evaluate (AbsE "f" (AppE (VarE "f") (IntE 42))) --> Ok (Abs ("f", AbsT IntT (T "a")) (App (Var "f" (AbsT IntT (T "a"))) (Int 42) (T "a")))
    evaluate (AppE (AbsE "x" (VarE "x")) (IntE 42)) --> Ok (Int 42)
    evaluate (AppE (AbsE "f" (AppE (VarE "f") (NumE 3.14))) (AbsE "x" (IntE 42))) --> Ok (Int 42)

-}
evaluate : Expr -> Result Error TypedExpr
evaluate expr =
    let
        ( typedExpr, ctx ) =
            toTypedExpr expr newContext
    in
    eval typedExpr ctx
        |> Result.map (\( e, c ) -> finalizeTypes e c)
        |> Result.map (\x -> canonicalize x Dict.empty |> Tuple.first)



-- Local definitions


type alias Context =
    { variables : Dict String TypedExpr
    , types : DisjointSet Type
    , freeTypes : Set String
    }


newContext : Context
newContext =
    { variables = Dict.empty
    , types = DisjointSet.add [ IntT, NumT ] DisjointSet.empty
    , freeTypes = Set.empty
    }


toTypedExpr : Expr -> Context -> ( TypedExpr, Context )
toTypedExpr expr ctx =
    case expr of
        IntE value ->
            ( Int value, ctx )

        NumE value ->
            ( Num value, ctx )

        VarE name ->
            withNewType ctx
                |> Tuple.mapFirst (Var name)

        AbsE name outE ->
            let
                ( inT, ctx1 ) =
                    withNewType ctx

                ( out, ctx2 ) =
                    toTypedExpr outE ctx1
            in
            ( Abs ( name, inT ) out, ctx2 )

        AppE absE argE ->
            let
                ( abs, ctx1 ) =
                    toTypedExpr absE ctx

                ( arg, ctx2 ) =
                    toTypedExpr argE ctx1

                ( typ, ctx3 ) =
                    withNewType ctx2
            in
            ( App abs arg typ, ctx3 )


typeOf : TypedExpr -> Context -> Type
typeOf expr ctx =
    case expr of
        Int _ ->
            IntT

        Num _ ->
            NumT

        Var _ typ ->
            finalType typ ctx

        Abs ( _, inT ) out ->
            AbsT (finalType inT ctx) (typeOf out ctx)

        App _ _ typ ->
            finalType typ ctx


eval : TypedExpr -> Context -> Result Error ( TypedExpr, Context )
eval expr ctx =
    case expr of
        Int _ ->
            Ok ( expr, ctx )

        Num _ ->
            Ok ( expr, ctx )

        Var name _ ->
            Dict.get name ctx.variables
                |> Result.fromMaybe (VariableNotFound name)
                |> Result.andThen
                    (\value ->
                        if value /= expr then
                            eval value ctx

                        else
                            Ok ( value, ctx )
                    )

        Abs ( name, inT ) out0 ->
            andThen2 (\_ out c -> Ok ( Abs ( name, inT ) out, c ))
                (\c -> Ok (withNewVariable name inT c))
                (eval out0)
                ctx

        App abs0 arg0 typ ->
            andThen2
                (\abs arg ctx1 ->
                    andThen
                        (\_ ctx2 ->
                            case abs of
                                Abs ( name, _ ) out ->
                                    andThen (\_ -> eval out)
                                        (withVariable name arg)
                                        ctx2

                                _ ->
                                    Ok ( App abs arg typ, ctx2 )
                        )
                        (unify (typeOf abs ctx1) (AbsT (typeOf arg ctx1) typ))
                        ctx1
                )
                (eval abs0)
                (eval arg0)
                ctx


unify : Type -> Type -> Context -> Result Error ( Type, Context )
unify type1 type2 ctx =
    let
        t1 =
            DisjointSet.find type1 ctx.types |> Maybe.withDefault type1

        t2 =
            DisjointSet.find type2 ctx.types |> Maybe.withDefault type2

        bind : String -> Type -> Context -> Context
        bind name typ c =
            { c
                | types = DisjointSet.union typ (T name) c.types
                , freeTypes = Set.remove name c.freeTypes
            }
    in
    case ( t1, t2 ) of
        ( AbsT inT1 outT1, AbsT inT2 outT2 ) ->
            Result.andThen
                (\( inT, inC ) ->
                    Result.map
                        (\( outT, outC ) -> ( AbsT inT outT, outC ))
                        (unify outT1 outT2 inC)
                )
                (unify inT1 inT2 ctx)

        ( T name1, _ ) ->
            if Set.member name1 ctx.freeTypes then
                Ok ( t2, bind name1 t2 ctx )

            else if t1 == t2 then
                Ok ( t1, ctx )

            else
                Err (TypeMismatch t1 t2)

        ( _, T _ ) ->
            unify t2 t1 ctx

        ( _, _ ) ->
            if t1 == t2 then
                Ok ( t1, ctx )

            else
                Err (TypeMismatch t1 t2)


canonicalize : TypedExpr -> Dict String Type -> ( TypedExpr, Dict String Type )
canonicalize expr types =
    case expr of
        Int _ ->
            ( expr, types )

        Num _ ->
            ( expr, types )

        Var name typ ->
            canonicalizeType typ types
                |> Tuple.mapFirst (Var name)

        Abs ( name, inT ) out ->
            let
                ( newInT, types1 ) =
                    canonicalizeType inT types

                ( newOut, types2 ) =
                    canonicalize out types1
            in
            ( Abs ( name, newInT ) newOut, types2 )

        App abs arg typ ->
            let
                ( newAbs, types1 ) =
                    canonicalize abs types

                ( newArg, types2 ) =
                    canonicalize arg types1

                ( newType, types3 ) =
                    canonicalizeType typ types2
            in
            ( App newAbs newArg newType, types3 )


canonicalizeType : Type -> Dict String Type -> ( Type, Dict String Type )
canonicalizeType typ types =
    case typ of
        T name ->
            case Dict.get name types of
                Just newType ->
                    ( newType, types )

                Nothing ->
                    let
                        newType =
                            T (newLowercaseName 1 (Dict.keys types))
                    in
                    ( newType, Dict.insert name newType types )

        IntT ->
            ( typ, types )

        NumT ->
            ( typ, types )

        AbsT t1 t2 ->
            let
                ( newType1, types1 ) =
                    canonicalizeType t1 types

                ( newType2, types2 ) =
                    canonicalizeType t2 types1
            in
            ( AbsT newType1 newType2, types2 )


finalType : Type -> Context -> Type
finalType typ c =
    case typ of
        AbsT t1 t2 ->
            AbsT (finalType t1 c) (finalType t2 c)

        _ ->
            DisjointSet.find typ c.types
                |> Maybe.withDefault typ


finalizeTypes : TypedExpr -> Context -> TypedExpr
finalizeTypes expr ctx =
    case expr of
        Int _ ->
            expr

        Num _ ->
            expr

        Var name typ ->
            Var name (finalType typ ctx)

        Abs ( name, inT ) out ->
            Abs ( name, finalType inT ctx ) (finalizeTypes out ctx)

        App abs arg typ ->
            App (finalizeTypes abs ctx) (finalizeTypes arg ctx) (finalType typ ctx)


withNewType : Context -> ( Type, Context )
withNewType ctx =
    let
        existingNames =
            DisjointSet.items ctx.types
                |> List.filterMap
                    (\typ ->
                        case typ of
                            T name ->
                                Just name

                            _ ->
                                Nothing
                    )

        typeName =
            newLowercaseName 1 existingNames
    in
    ( T typeName
    , { ctx
        | types = DisjointSet.union (T typeName) (T typeName) ctx.types
        , freeTypes = Set.insert typeName ctx.freeTypes
      }
    )


withNewVariable : String -> Type -> Context -> ( TypedExpr, Context )
withNewVariable name typ ctx =
    ( Var name typ
    , { ctx | variables = Dict.insert name (Var name typ) ctx.variables }
    )


withVariable : String -> TypedExpr -> Context -> Result Error ( TypedExpr, Context )
withVariable name value ctx =
    andThen
        (\x c ->
            Ok ( x, { c | variables = Dict.insert name x c.variables } )
        )
        (eval value)
        ctx


andThen :
    (a -> state -> Result error ( b, state ))
    -> (state -> Result error ( a, state ))
    -> state
    -> Result error ( b, state )
andThen f r1 state =
    Result.andThen (\( x, s ) -> f x s) (r1 state)


andThen2 :
    (a -> b -> state -> Result error ( c, state ))
    -> (state -> Result error ( a, state ))
    -> (state -> Result error ( b, state ))
    -> state
    -> Result error ( c, state )
andThen2 f r1 r2 state =
    andThen (\x1 -> andThen (f x1) r2) r1 state



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
        [ [ fromRight (text "->") AbsT ]
        , [ inbetween (char '(') (char ')') identity
          , term (map T identifier)
          , term (map (\_ -> IntT) (text "@Int"))
          , term (map (\_ -> NumT) (text "@Num"))
          ]
        ]


expressionParser : Parser Expr
expressionParser =
    let
        application : Operator Expr
        application =
            Parser.Expression.InfixFromLeft
                (\expr ->
                    succeed (\right left -> AppE left right)
                        |> drop spaces
                        |> take expr
                )

        abstraction : Operator Expr
        abstraction =
            Parser.Expression.Prefix
                (\expr ->
                    succeed AbsE
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
                    succeed (\name value outE -> AppE (AbsE name outE) value)
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
          , term (map IntE int)
          , term (map NumE number)
          , term (map VarE identifier)
          ]
        ]



-- Utility functions


newLowercaseName : Int -> List String -> String
newLowercaseName seed existing =
    let
        name =
            (case seed |> toBase (Char.toCode 'z' - Char.toCode 'a' + 2) of
                x :: xs ->
                    x - 1 :: xs

                [] ->
                    [ 0 ]
            )
                |> List.map (\x -> x + Char.toCode 'a')
                |> List.map Char.fromCode
                |> String.fromList
    in
    if List.any ((==) name) existing then
        newLowercaseName (seed + 1) existing

    else
        name


toBase : Int -> Int -> List Int
toBase base num =
    if num == 0 then
        []

    else
        ((num // base) |> toBase base) ++ [ num |> modBy base ]

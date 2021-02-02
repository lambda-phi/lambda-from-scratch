module Lambda exposing (Error(..), Expr(..), Type(..), evaluate, typeOf)

{-| [Hindley-Milner lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus#Alternative_syntaxes)
-}

import Dict exposing (Dict)
import DisjointSet exposing (DisjointSet)
import Lambda.Util exposing (newLowercaseName)
import Parser


type Type
    = IntType
    | NumType
    | Type String -- a
    | AbsType Type Type -- a -> b


type Expr
    = Int Int
    | Num Float
    | Var String -- x
    | Abs String Expr -- λx.x
    | App Expr Expr -- f x


type Error
    = SyntaxError Parser.Error
    | VariableNotFound String
    | InvalidApply Expr Type
    | TypeMismatch Type Type


{-|

    import Lambda
    import Lambda.IO

    check : String -> Result Error String
    check expr =
        Lambda.IO.read expr
            |> Result.andThen typeOf
            |> Result.map Lambda.IO.writeType

    check "42" --> Ok "@Int"
    check "3.14" --> Ok "@Num"
    check "x" --> Err (VariableNotFound "x")
    check "λx.42" --> Ok "a->@Int"
    check "λx.x" --> Ok "a->a"
    check "(λx.3.14) 42" --> Ok "@Num"
    check "(λx.x) 42" --> Ok "@Int"
    check "(λx.x) (λx.42)" --> Ok "a->@Int"

-}
typeOf : Expr -> Result Error Type
typeOf expression =
    let
        typeOf_ : Expr -> Context -> Result Error ( Type, Context )
        typeOf_ expr ctx =
            case expr of
                Int _ ->
                    Ok ( IntType, ctx )

                Num _ ->
                    Ok ( NumType, ctx )

                Var name ->
                    Dict.get name ctx.variables
                        |> Result.fromMaybe (VariableNotFound name)
                        |> Result.map (\t -> ( t, ctx ))

                Abs name outE ->
                    let
                        inputT =
                            newType ctx
                    in
                    { ctx | variables = Dict.insert name inputT ctx.variables }
                        |> typeOf_ outE
                        |> Result.map (Tuple.mapFirst (AbsType inputT))

                App absE argE ->
                    resultAndThen2
                        (\( absT, c ) ( argT, _ ) ->
                            case absT of
                                AbsType inputT outputT ->
                                    if isFreeType inputT c then
                                        Ok (unify argT inputT c)
                                            |> Result.map
                                                (\newCtx ->
                                                    ( finalType outputT newCtx, newCtx )
                                                )

                                    else if inputT == argT then
                                        Ok ( finalType outputT c, c )

                                    else
                                        Err (TypeMismatch inputT argT)

                                _ ->
                                    Err (InvalidApply absE absT)
                        )
                        (typeOf_ absE ctx)
                        (typeOf_ argE ctx)
    in
    typeOf_ expression newContext |> Result.map Tuple.first


{-|

    import Lambda

    evaluate (Int 42) --> Ok (Int 42, IntType)
    evaluate (Num 3.14) --> Ok (Num 3.14, NumType)
    evaluate (Var "x") --> Err (VariableNotFound "x")
    evaluate (Abs "x" (Var "x")) --> Ok ((Abs "x" (Var "x")), AbsType (Type "a") (Type "a"))
    evaluate (App (Abs "x" (Int 42)) (Num 3.14)) --> Ok (Int 42, IntType)

-}
evaluate : Expr -> Result Error ( Expr, Type )
evaluate expression =
    let
        eval : Dict String Expr -> Expr -> Result Error ( Expr, Type )
        eval vars expr =
            case expr of
                Var x ->
                    Dict.get x vars
                        |> Result.fromMaybe (VariableNotFound x)
                        |> Result.andThen (eval vars)

                App (Abs x outE) argE ->
                    typeOf expr
                        |> Result.andThen (\_ -> eval (Dict.insert x argE vars) outE)

                _ ->
                    typeOf expr
                        |> Result.map (Tuple.pair expr)
    in
    eval Dict.empty expression


unify : Type -> Type -> Context -> Context
unify t1 t2 ctx =
    { ctx | types = DisjointSet.union t1 t2 ctx.types }


finalType : Type -> Context -> Type
finalType typ ctx =
    case typ of
        AbsType t1 t2 ->
            AbsType (finalType t1 ctx) (finalType t2 ctx)

        _ ->
            DisjointSet.find typ ctx.types |> Maybe.withDefault typ



-- CONTEXT


type alias Context =
    { variables : Dict String Type
    , types : DisjointSet Type
    }


newContext : Context
newContext =
    -- TODO(DisjointSet): add an `add` function
    { variables = Dict.empty
    , types =
        DisjointSet.empty
            |> DisjointSet.union IntType IntType
            |> DisjointSet.union NumType NumType
    }


newType : Context -> Type
newType ctx =
    let
        existingNames =
            -- TODO: add DisjointSet.items to return a list of all items.
            DisjointSet.toList ctx.types
                |> List.map Tuple.first
                |> List.filterMap
                    (\typ ->
                        case typ of
                            Type name ->
                                Just name

                            _ ->
                                Nothing
                    )
    in
    Type (newLowercaseName 1 existingNames)


isFreeType : Type -> Context -> Bool
isFreeType typ ctx =
    -- TODO(DisjointSet): add a `has` method
    case DisjointSet.find typ ctx.types of
        Just _ ->
            False

        Nothing ->
            True


resultAndThen2 : (a -> b -> Result error c) -> Result error a -> Result error b -> Result error c
resultAndThen2 f result1 result2 =
    Result.andThen (\x1 -> Result.andThen (f x1) result2) result1

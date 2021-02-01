module Lambda exposing (Error(..), Expr(..), Type(..), TypedExpr(..), evaluate, inferType, typeOf, withType, withVariable, write)

{-| [Hindley-Milner lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus#Alternative_syntaxes)
-}

import Dict exposing (Dict)
import DisjointSet exposing (DisjointSet)
import Lambda.Util exposing (newLowercaseName)


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


type TypedExpr
    = TInt Int
    | TNum Float
    | TVar String Type -- x:A
    | TAbs ( String, Type ) TypedExpr -- λx:A.x
    | TApp TypedExpr TypedExpr -- (f:A) (x:B)


type Error
    = VariableNotFound String
    | InvalidApply Expr Type
    | TypeMismatch Type Type



-- READ / WRITE


{-|

    import Lambda

    write (Int 42) --> "42"
    write (Num 3.14) --> "3.14"
    write (Var "x") --> "x"
    write (Abs "x" (Var "y")) --> "λx.y"
    write (App (Var "f") (Var "x")) --> "f x"
    write (App (App (Var "f") (Var "x")) (Var "y")) --> "f x y"
    write (Abs "x" (App (Var "y") (Var "z"))) --> "λx.y z"
    write (App (Var "f") (Abs "x" (Var "y"))) --> "f (λx.y)"
    write (App (Abs "x" (Var "y")) (Var "z")) --> "(λx.y) z"
    write (App (Abs "x" (Var "y")) (Abs "a" (Var "b"))) --> "(λx.y) (λa.b)"

-}
write : Expr -> String
write expr =
    case expr of
        Int value ->
            String.fromInt value

        Num value ->
            String.fromFloat value

        Var x ->
            x

        Abs x outE ->
            "λ" ++ x ++ "." ++ write outE

        App ((Abs _ _) as absE) ((Abs _ _) as argE) ->
            "(" ++ write absE ++ ") (" ++ write argE ++ ")"

        App ((Abs _ _) as absE) argE ->
            "(" ++ write absE ++ ") " ++ write argE

        App absE ((Abs _ _) as argE) ->
            write absE ++ " (" ++ write argE ++ ")"

        App absE argE ->
            write absE ++ " " ++ write argE



-- EXPRESSIONS AND TYPES


{-|

    import Lambda

    typeOf (Int 42) --> Ok IntType
    typeOf (Num 3.14) --> Ok NumType
    typeOf (Var "x") --> Err (VariableNotFound "x")
    typeOf (Abs "x" (Int 42)) --> Ok (AbsType (Type "a") IntType)
    typeOf (Abs "x" (Var "x")) --> Ok (AbsType (Type "a") (Type "a"))
    typeOf (Abs "x" (Var "y")) --> Err (VariableNotFound "y")
    typeOf (App (Abs "x" (Int 42)) (Num 3.14)) --> Ok IntType
    typeOf (App (Abs "x" (Num 3.14)) (Int 42)) --> Ok NumType
    typeOf (App (Abs "x" (Var "x")) (Int 42)) --> Ok IntType
    typeOf (App (Abs "x" (Var "x")) (Num 3.14)) --> Ok NumType
    typeOf (App (Abs "x" (Var "x")) (Abs "x" (Int 42))) --> Ok (AbsType (Type "a") IntType)

-}
typeOf : Expr -> Result Error Type
typeOf expr =
    inferTypes (\( _, t ) _ -> Ok t) expr newContext


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


{-|

    import Lambda
    import Lambda.IO

    inferType : String -> Result Error String
    inferType expr =
        Lambda.IO.read expr
            |> Result.andThen inferType
            |> Result.andThen Lambda.IO.writeTyped

    inferType "42" -- Ok "42"
    inferType "3.14" -- Ok "3.14"
    inferType "x" -- Err (VariableNotFound "x")
    inferType "λx.y" -- Err (VariableNotFound "y")
    inferType "λx.x" -- Ok "λx:a.x:a"
    inferType "λx.42" -- Ok "λx:a.42"
    inferType "(λx.x) 42" -- Ok "((λx:Int.x:Int):Int->Int) 42"
    inferType "(λx.x) 3.14" -- Ok "((λx:Num.x:Num):Num->Num) 3.14"

-}
inferType : Expr -> Result Error TypedExpr
inferType expression =
    let
        infer : Expr -> Context -> Result Error TypedExpr
        infer expr ctx =
            case expr of
                Int value ->
                    Ok (TInt value)

                Num value ->
                    Ok (TNum value)

                Var x ->
                    Dict.get x ctx.variables
                        |> Result.fromMaybe (VariableNotFound x)
                        |> Result.map (TVar x)

                Abs x outE ->
                    let
                        xType =
                            newType ctx
                    in
                    ctx
                        |> withVariable x xType
                        |> infer outE
                        |> Result.map (TAbs ( x, xType ))

                App absE argE ->
                    -- inferTypes2
                    --     (\( _, absT ) ( _, argT ) c ->
                    --         case absT of
                    --             AbsType inT outT ->
                    --                 if isFreeType inT c then
                    --                     let
                    --                         newCtx =
                    --                             unify argT inT c
                    --                     in
                    --                     f ( expr, finalType outT newCtx ) newCtx
                    --                 else if inT == argT then
                    --                     f ( expr, finalType outT c ) c
                    --                 else
                    --                     Err (TypeMismatch inT argT)
                    --             _ ->
                    --                 Err (InvalidApply absE absT)
                    --     )
                    --     absE
                    --     argE
                    --     ctx
                    Result.map2
                        (\tAbs tArg ->
                            Debug.todo "app"
                        )
                        (infer absE ctx)
                        (infer argE ctx)
    in
    infer expression newContext


inferTypes : (( Expr, Type ) -> Context -> Result Error a) -> Expr -> Context -> Result Error a
inferTypes f expr ctx =
    case expr of
        Int _ ->
            f ( expr, IntType ) ctx

        Num _ ->
            f ( expr, NumType ) ctx

        Var x ->
            Dict.get x ctx.variables
                |> Result.fromMaybe (VariableNotFound x)
                |> Result.andThen (\t -> f ( expr, finalType t ctx ) ctx)

        Abs x outE ->
            let
                xT =
                    newType ctx
            in
            ctx
                |> withVariable x xT
                |> inferTypes (\( _, outT ) -> f ( expr, AbsType xT (finalType outT ctx) )) outE

        App absE argE ->
            inferTypes2
                (\( _, absT ) ( _, argT ) c ->
                    case absT of
                        AbsType inT outT ->
                            if isFreeType inT c then
                                let
                                    newCtx =
                                        unify argT inT c
                                in
                                f ( expr, finalType outT newCtx ) newCtx

                            else if inT == argT then
                                f ( expr, finalType outT c ) c

                            else
                                Err (TypeMismatch inT argT)

                        _ ->
                            Err (InvalidApply absE absT)
                )
                absE
                argE
                ctx


inferTypes2 : (( Expr, Type ) -> ( Expr, Type ) -> Context -> Result Error a) -> Expr -> Expr -> Context -> Result Error a
inferTypes2 f expr1 expr2 ctx =
    inferTypes (\( e1, t1 ) -> inferTypes (f ( e1, t1 )) expr2) expr1 ctx


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
    { variables = Dict.empty
    , types = DisjointSet.empty
    }
        |> withType IntType
        |> withType NumType


withVariable : String -> Type -> Context -> Context
withVariable name typ ctx =
    { ctx | variables = Dict.insert name typ ctx.variables }


withType : Type -> Context -> Context
withType typ ctx =
    -- TODO(DisjointSet): add an `add` function
    { ctx | types = DisjointSet.union typ typ ctx.types }


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

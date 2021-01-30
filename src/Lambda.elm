module Lambda exposing (Context, Error(..), Expression(..), Type(..), evaluate, isFreeType, newContext, newType, toBase, typeOf, withType, withVariable, write)

import Char
import Dict exposing (Dict)
import DisjointSet exposing (DisjointSet)


{-| [Hindley-Milner lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus#Alternative_syntaxes)
-}
type Type
    = Type String -- a
    | TypeAbs Type Type -- a -> b


type Expression
    = Int Int
    | Num Float
    | Var String -- x
    | Abs String Expression -- λx.x
    | App Expression Expression -- f x


type Error
    = VariableNotFound String
    | InvalidApply Expression Type
    | TypeMismatch Type Type


type alias Context =
    { variables : Dict String Type
    , types : DisjointSet Type
    }


newContext : Context
newContext =
    { variables = Dict.empty
    , types = DisjointSet.empty
    }
        |> withType "Integer"
        |> withType "Number"


withVariable : String -> Type -> Context -> Context
withVariable name typ ctx =
    { ctx | variables = Dict.insert name typ ctx.variables }


withType : String -> Context -> Context
withType name ctx =
    -- TODO(DisjointSet): add an `add` function
    { ctx | types = DisjointSet.union (Type name) (Type name) ctx.types }


{-|

    99 |> toBase 2 --> [1,1,0,0,0,1,1]

    99 |> toBase 3 --> [1,0,2,0,0]

    99 |> toBase 4 --> [1,2,0,3]

    99 |> toBase 5 --> [3,4,4]

    99 |> toBase 6 --> [2,4,3]

    99 |> toBase 7 --> [2,0,1]

    99 |> toBase 8 --> [1,4,3]

    99 |> toBase 9 --> [1,2,0]

-}
toBase : Int -> Int -> List Int
toBase base num =
    if num == 0 then
        []

    else
        ((num // base) |> toBase base) ++ [ num |> modBy base ]


{-|

    import Dict

    newType 1 [] --> Type "a"
    newType 26 [] --> Type "z"
    newType 27 [] --> Type "aa"
    newType 28 [] --> Type "ab"

    newType 1 [ Type "a" ] --> Type "b"
    newType 1 [ Type "a", Type "b" ] --> Type "c"

-}
newType : Int -> List Type -> Type
newType seed existing =
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
    if List.any ((==) (Type name)) existing then
        newType (seed + 1) existing

    else
        Type name


{-|

    import Lambda

    typeOf (Int 42) --> Ok (Type "Integer")
    typeOf (Num 3.14) --> Ok (Type "Number")
    typeOf (Var "x") --> Err (VariableNotFound "x")
    typeOf (Abs "x" (Int 42)) --> Ok (TypeAbs (Type "a") (Type "Integer"))
    typeOf (Abs "x" (Var "x")) --> Ok (TypeAbs (Type "a") (Type "a"))
    typeOf (Abs "x" (Var "y")) --> Err (VariableNotFound "y")
    typeOf (App (Abs "x" (Int 42)) (Num 3.14)) --> Ok (Type "Integer")
    typeOf (App (Abs "x" (Num 3.14)) (Int 42)) --> Ok (Type "Number")
    typeOf (App (Abs "x" (Var "x")) (Int 42)) --> Ok (Type "Integer")
    typeOf (App (Abs "x" (Var "x")) (Num 3.14)) --> Ok (Type "Number")
    typeOf (App (Abs "x" (Var "x")) (Abs "x" (Int 42))) --> Ok (TypeAbs (Type "a") (Type "Integer"))

-}
typeOf : Expression -> Result Error Type
typeOf expr =
    inferTypes (\( _, t ) _ -> Ok t) expr newContext


{-|

    import Lambda

    evaluate (Int 42) --> Ok (Int 42, Type "Integer")
    evaluate (Num 3.14) --> Ok (Num 3.14, Type "Number")
    evaluate (Var "x") --> Err (VariableNotFound "x")
    evaluate (Abs "x" (Var "x")) --> Ok ((Abs "x" (Var "x")), TypeAbs (Type "a") (Type "a"))
    evaluate (App (Abs "x" (Int 42)) (Num 3.14)) --> Ok (Int 42, Type "Integer")

-}
evaluate : Expression -> Result Error ( Expression, Type )
evaluate expression =
    let
        eval : Dict String Expression -> Expression -> Result Error ( Expression, Type )
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
write : Expression -> String
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


inferTypes : (( Expression, Type ) -> Context -> Result Error a) -> Expression -> Context -> Result Error a
inferTypes f expr ctx =
    case expr of
        Int _ ->
            f ( expr, Type "Integer" ) ctx

        Num _ ->
            f ( expr, Type "Number" ) ctx

        Var x ->
            Dict.get x ctx.variables
                |> Result.fromMaybe (VariableNotFound x)
                |> Result.andThen (\t -> f ( expr, finalType t ctx ) ctx)

        Abs x outE ->
            let
                xT =
                    newType 1 (DisjointSet.toList ctx.types |> List.map Tuple.first)
            in
            ctx
                |> withVariable x xT
                |> inferTypes (\( _, outT ) -> f ( expr, TypeAbs xT (finalType outT ctx) )) outE

        App absE argE ->
            inferTypes2
                (\( _, absT ) ( _, argT ) c ->
                    case absT of
                        TypeAbs inT outT ->
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


inferTypes2 : (( Expression, Type ) -> ( Expression, Type ) -> Context -> Result Error a) -> Expression -> Expression -> Context -> Result Error a
inferTypes2 f expr1 expr2 ctx =
    inferTypes (\( e1, t1 ) -> inferTypes (f ( e1, t1 )) expr2) expr1 ctx


unify : Type -> Type -> Context -> Context
unify t1 t2 ctx =
    { ctx | types = DisjointSet.union t1 t2 ctx.types }


finalType : Type -> Context -> Type
finalType typ ctx =
    case typ of
        Type _ ->
            DisjointSet.find typ ctx.types |> Maybe.withDefault typ

        TypeAbs t1 t2 ->
            TypeAbs (finalType t1 ctx) (finalType t2 ctx)


{-|

    isFreeType (Type "Integer") newContext --> False

    isFreeType (Type "a") newContext --> True

-}
isFreeType : Type -> Context -> Bool
isFreeType typ ctx =
    -- TODO(DisjointSet): add a `has` method
    case DisjointSet.find typ ctx.types of
        Just _ ->
            False

        Nothing ->
            True

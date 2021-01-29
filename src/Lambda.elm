module Lambda exposing (Context, Error(..), Expression(..), Type(..), isFreeType, map, newContext, newType, toBase, withType, withVariable)

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

    import Dict

    ctx : Context
    ctx =
        newContext
            |> withVariable "x" (Type "Integer")
            |> withVariable "f" (TypeAbs (Type "Integer") (Type "Number"))
            |> withVariable "g" (TypeAbs (Type "a") (Type "a"))

    typeOf : Expression -> Result Error Type
    typeOf expr =
        map (\x t s -> (t, s)) expr Nothing ctx
            |> Result.map Tuple.first

    typeOf (Int 42) --> Ok (Type "Integer")
    typeOf (Num 3.14) --> Ok (Type "Number")

    typeOf (Var "x") --> Ok (Type "Integer")
    typeOf (Var "y") --> Err (VariableNotFound "y")

    typeOf (Abs "x" (Int 42)) --> Ok (TypeAbs (Type "a") (Type "Integer"))
    typeOf (Abs "x" (Var "x")) --> Ok (TypeAbs (Type "a") (Type "a"))
    typeOf (Abs "x" (Var "y")) --> Err (VariableNotFound "y")

    typeOf (App (Var "x") (Int 42)) --> Err (InvalidApply (Var "x") (Type "Integer"))
    typeOf (App (Var "f") (Int 42)) --> Ok (Type "Number")
    typeOf (App (Var "f") (Num 3.14)) --> Err (TypeMismatch (Type "Integer") (Type "Number"))
    typeOf (App (Var "f") (Abs "x" (Int 42))) --> Err (TypeMismatch (Type "Integer") (TypeAbs (Type "a") (Type "Integer")))
    typeOf (App (Var "g") (Int 42)) --> Ok (Type "Integer")
    typeOf (App (Var "g") (Num 3.14)) --> Ok (Type "Number")
    typeOf (App (Var "g") (Abs "x" (Int 42))) --> Ok (TypeAbs (Type "a") (Type "Integer"))

    typeOf (App (Abs "x" (Int 42)) (Num 3.14)) --> Ok (Type "Integer")
    typeOf (App (Abs "x" (Num 3.14)) (Int 42)) --> Ok (Type "Number")
    typeOf (App (Abs "x" (Var "x")) (Int 42)) --> Ok (Type "Integer")
    typeOf (App (Abs "x" (Var "x")) (Num 3.14)) --> Ok (Type "Number")
    typeOf (App (Abs "x" (Var "x")) (Abs "x" (Int 42))) --> Ok (TypeAbs (Type "a") (Type "Integer"))

-}
map : (Expression -> Type -> state -> ( a, state )) -> Expression -> state -> Context -> Result Error ( a, state )
map f expr state ctx =
    inferTypes (\x t _ -> Ok (f x t state)) expr ctx


inferTypes : (Expression -> Type -> Context -> Result Error a) -> Expression -> Context -> Result Error a
inferTypes f expr ctx =
    case expr of
        Int _ ->
            f expr (Type "Integer") ctx

        Num _ ->
            f expr (Type "Number") ctx

        Var x ->
            Dict.get x ctx.variables
                |> Result.fromMaybe (VariableNotFound x)
                |> Result.andThen (\t -> f expr (finalType t ctx) ctx)

        Abs x outE ->
            let
                xT =
                    newType 1 (DisjointSet.toList ctx.types |> List.map Tuple.first)
            in
            ctx
                |> withVariable x xT
                |> inferTypes (\_ outT -> f expr (TypeAbs xT (finalType outT ctx))) outE

        App absE argE ->
            andThen2
                (\( _, absT ) ( _, argT ) c ->
                    case absT of
                        TypeAbs inT outT ->
                            if isFreeType inT c then
                                let
                                    newCtx =
                                        unify argT inT c
                                in
                                f expr (finalType outT newCtx) newCtx

                            else if inT == argT then
                                f expr (finalType outT c) c

                            else
                                Err (TypeMismatch inT argT)

                        _ ->
                            Err (InvalidApply absE absT)
                )
                absE
                argE
                ctx


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


andThen2 : (( Expression, Type ) -> ( Expression, Type ) -> Context -> Result Error a) -> Expression -> Expression -> Context -> Result Error a
andThen2 f expr1 expr2 ctx =
    inferTypes
        (\e1 t1 -> inferTypes (\e2 t2 -> f ( e1, t1 ) ( e2, t2 )) expr2)
        expr1
        ctx


{-|

    isFreeType (Type "Integer") newContext --> False

    isFreeType (Type "a") newContext --> True

-}
isFreeType : Type -> Context -> Bool
isFreeType typ ctx =
    case DisjointSet.find typ ctx.types of
        Just _ ->
            False

        Nothing ->
            True

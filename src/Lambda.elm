module Lambda exposing
    ( Type(..), Expr(..), Error(..)
    , evaluate
    )

{-| A simple Lambda calculus implementation with type inference.

@docs Type, Expr, Error

@docs evaluate

-}

import Dict exposing (Dict)
import DisjointSet exposing (DisjointSet)
import Lambda.Util exposing (newLowercaseName)
import Parser
import Set exposing (Set)


{-| A Lambda calculus type.
-}
type Type
    = IntType -- @Int
    | NumType -- @Num
    | Type String -- a
    | AbsType Type Type -- a -> b


{-| A Lambda calculus expression.
-}
type Expr
    = Int Int -- 42
    | Num Float -- 3.14
    | Var String -- x
    | Abs String Expr -- λx.x
    | App Expr Expr -- f x


{-| An error from parsing or evaluation.
-}
type Error
    = SyntaxError Parser.Error
    | VariableNotFound String
    | TypeMismatch Type Type


{-| Evaluates an expression and returns either an `(Expr, Type)` pair, or an `Error`.

    import Lambda

    -- Builtin values
    evaluate (Int 42) --> Ok (Int 42, IntType)
    evaluate (Num 3.14) --> Ok (Num 3.14, NumType)

    -- Variable: x
    evaluate (Var "x") --> Err (VariableNotFound "x")

    -- Abstraction: λx.x
    evaluate (Abs "x" (Var "x")) --> Ok (Abs "x" (Var "x"), AbsType (Type "a") (Type "a"))

    -- Application: (λx.x) 42
    evaluate (App (Abs "x" (Var "x")) (Int 42)) --> Ok (Int 42, IntType)

-}
evaluate : Expr -> Result Error ( Expr, Type )
evaluate expr =
    eval expr newContext
        |> Result.map Tuple.first
        |> Result.map (\( e, t ) -> ( e, canonicalize t Dict.empty |> Tuple.first ))


eval : Expr -> Context -> Result Error ( ( Expr, Type ), Context )
eval expr ctx =
    case expr of
        Int _ ->
            Ok ( ( expr, IntType ), ctx )

        Num _ ->
            Ok ( ( expr, NumType ), ctx )

        Var name ->
            Dict.get name ctx.variables
                |> Result.fromMaybe (VariableNotFound name)
                |> Result.andThen
                    (\( value, typ ) ->
                        if value /= expr then
                            eval value ctx

                        else
                            Ok ( ( value, typ ), ctx )
                    )

        Abs name outE ->
            map2
                (\( _, inT ) ( out, outT ) c ->
                    ( ( Abs name out, AbsType inT outT ), c )
                )
                (Ok (withNewVariable name ctx))
                (eval outE)

        App absE argE ->
            andThen2
                (\( abs, absT0 ) ( arg, argT ) appC0 ->
                    let
                        ( outT0, appC ) =
                            withNewType appC0
                    in
                    Result.andThen
                        (\( absT, c ) ->
                            case ( abs, absT ) of
                                ( Abs name outE, AbsType _ _ ) ->
                                    andThen (\_ -> eval outE)
                                        (withVariable name arg c)

                                ( _, AbsType _ outT ) ->
                                    Ok ( ( App abs arg, outT ), c )

                                _ ->
                                    -- Impossible state: `unify` should catch this
                                    Err (TypeMismatch absT (AbsType argT outT0))
                        )
                        (unify absT0 (AbsType argT outT0) appC)
                )
                (eval absE ctx)
                (eval argE)


canonicalize : Type -> Dict String Type -> ( Type, Dict String Type )
canonicalize typ types =
    case typ of
        IntType ->
            ( typ, types )

        NumType ->
            ( typ, types )

        Type name ->
            case Dict.get name types of
                Just newType ->
                    ( newType, types )

                Nothing ->
                    let
                        newType =
                            Type (newLowercaseName 1 (Dict.keys types))
                    in
                    ( newType, Dict.insert name newType types )

        AbsType t1 t2 ->
            let
                ( newType1, types1 ) =
                    canonicalize t1 types

                ( newType2, types2 ) =
                    canonicalize t2 types1
            in
            ( AbsType newType1 newType2, types2 )


andThen : (( Expr, Type ) -> Context -> Result Error ( ( Expr, Type ), Context )) -> Result Error ( ( Expr, Type ), Context ) -> Result Error ( ( Expr, Type ), Context )
andThen f result =
    Result.andThen
        (\( ( e, t ), ctx ) ->
            f ( e, finalType t ctx ) ctx
        )
        result


andThen2 : (( Expr, Type ) -> ( Expr, Type ) -> Context -> Result Error ( ( Expr, Type ), Context )) -> Result Error ( ( Expr, Type ), Context ) -> (Context -> Result Error ( ( Expr, Type ), Context )) -> Result Error ( ( Expr, Type ), Context )
andThen2 f result1 toResult2 =
    andThen
        (\( e1, t1 ) ctx1 ->
            andThen
                (\( e2, t2 ) ctx2 ->
                    f ( e1, finalType t1 ctx2 ) ( e2, finalType t2 ctx2 ) ctx2
                )
                (toResult2 ctx1)
        )
        result1


map : (( Expr, Type ) -> Context -> ( ( Expr, Type ), Context )) -> Result Error ( ( Expr, Type ), Context ) -> Result Error ( ( Expr, Type ), Context )
map f result =
    andThen (\et ctx -> Ok (f et ctx)) result


map2 : (( Expr, Type ) -> ( Expr, Type ) -> Context -> ( ( Expr, Type ), Context )) -> Result Error ( ( Expr, Type ), Context ) -> (Context -> Result Error ( ( Expr, Type ), Context )) -> Result Error ( ( Expr, Type ), Context )
map2 f result1 toResult2 =
    andThen2 (\et1 et2 ctx -> Ok (f et1 et2 ctx)) result1 toResult2



-- CONTEXT


type alias Context =
    { variables : Dict String ( Expr, Type )
    , types : DisjointSet Type
    , freeTypes : Set String
    }


newContext : Context
newContext =
    { variables = Dict.empty
    , types = DisjointSet.add [ IntType, NumType ] DisjointSet.empty
    , freeTypes = Set.empty
    }


withNewType : Context -> ( Type, Context )
withNewType ctx =
    let
        existingNames =
            DisjointSet.items ctx.types
                |> List.filterMap
                    (\typ ->
                        case typ of
                            Type name ->
                                Just name

                            _ ->
                                Nothing
                    )

        typeName =
            newLowercaseName 1 existingNames
    in
    ( Type typeName
    , { ctx
        | types = DisjointSet.union (Type typeName) (Type typeName) ctx.types
        , freeTypes = Set.insert typeName ctx.freeTypes
      }
    )


withNewVariable : String -> Context -> ( ( Expr, Type ), Context )
withNewVariable name ctx =
    let
        ( typ, c ) =
            withNewType ctx
    in
    ( ( Var name, typ )
    , { c | variables = Dict.insert name ( Var name, typ ) c.variables }
    )


withVariable : String -> Expr -> Context -> Result Error ( ( Expr, Type ), Context )
withVariable name value ctx =
    map
        (\( x, t ) c ->
            ( ( x, t ), { c | variables = Dict.insert name ( x, t ) c.variables } )
        )
        (eval value ctx)


finalType : Type -> Context -> Type
finalType typ ctx =
    case typ of
        AbsType t1 t2 ->
            AbsType (finalType t1 ctx) (finalType t2 ctx)

        _ ->
            DisjointSet.find typ ctx.types |> Maybe.withDefault typ


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
                | types = DisjointSet.union typ (Type name) c.types
                , freeTypes = Set.remove name c.freeTypes
            }
    in
    case ( t1, t2 ) of
        ( AbsType inT1 outT1, AbsType inT2 outT2 ) ->
            Result.andThen
                (\( inT, inC ) ->
                    Result.map
                        (\( outT, outC ) -> ( AbsType inT outT, outC ))
                        (unify outT1 outT2 inC)
                )
                (unify inT1 inT2 ctx)

        ( Type name1, _ ) ->
            if Set.member name1 ctx.freeTypes then
                Ok ( t2, bind name1 t2 ctx )

            else if t1 == t2 then
                Ok ( t1, ctx )

            else
                Err (TypeMismatch t1 t2)

        ( _, Type _ ) ->
            unify t2 t1 ctx

        ( _, _ ) ->
            if t1 == t2 then
                Ok ( t1, ctx )

            else
                Err (TypeMismatch t1 t2)

module Lambda exposing (Error(..), Expr(..), Type(..), evaluate)

{-| [Hindley-Milner lambda calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus#Alternative_syntaxes)
-}

import Dict exposing (Dict)
import DisjointSet exposing (DisjointSet)
import Lambda.Util exposing (newLowercaseName)
import Parser
import Set exposing (Set)


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
    | TypeMismatch Type Type


{-|

    import Lambda
    import Lambda.IO

    eval : String -> Result Error (String, String)
    eval txt =
        Lambda.IO.read txt
            |> Result.andThen evaluate
            |> Result.map (Tuple.mapBoth Lambda.IO.write Lambda.IO.writeType)

    -- Values
    eval "42" --> Ok ("42", "@Int")
    eval "3.14" --> Ok ("3.14", "@Num")

    -- Variables
    eval "x" --> Err (VariableNotFound "x")

    -- Abstractions
    eval "λx.42" --> Ok ("λx.42", "a->@Int")
    eval "λx.x" --> Ok ("λx.x", "a->a")
    eval "λx.y" --> Err (VariableNotFound "y")

    -- Applications
    eval "1 2" --> Err (TypeMismatch IntType (AbsType IntType (Type "a")))
    eval "λf.f 42" --> Ok ("λf.f 42", "(@Int->b)->b")
    eval "(λx.x) 42" --> Ok ("42", "@Int")
    eval "x=42; x" --> Ok ("42", "@Int")
    eval "f=λx.42; f" --> Ok ("λx.42", "e->@Int")
    eval "f=λx.42; f 3.14" --> Ok ("42", "@Int")

-}
evaluate : Expr -> Result Error ( Expr, Type )
evaluate expression =
    eval expression newContext
        |> Result.map Tuple.first


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
    -- TODO(DisjointSet): add an `add` function
    { variables = Dict.empty
    , types =
        DisjointSet.empty
            |> DisjointSet.union IntType IntType
            |> DisjointSet.union NumType NumType
    , freeTypes = Set.empty
    }


withNewType : Context -> ( Type, Context )
withNewType ctx =
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

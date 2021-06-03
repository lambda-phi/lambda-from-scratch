module Lambda exposing
    ( Context
    , Error(..)
    , Expr(..)
    , Type
    , add
    , andThen
    , andThen2
    , andThen3
    , asCall
    , asForAll
    , asFunc
    , asFuncType
    , call
    , define
    , eval
    , exp
    , forAll
    , function
    , functionType
    , get
    , letVar
    , map
    , map2
    , map3
    , mul
    , newName
    , read
    , resultAndThen2
    , run
    , tupleMap
    , unify
    , withError
    , withResult
    , withValue
    , write
    )

{-| A simple Lambda calculus implementation with type inference.

TODO: validate that this Elm issue works: <https://github.com/elm/compiler/issues/2186>

[Calculus of constructions](https://en.wikipedia.org/wiki/Calculus_of_constructions)

-}

import Dict exposing (Dict)
import Parser exposing (Parser, andThen, drop, lazy, oneOf, succeed, take)
import Parser.Char exposing (char, digit, letter)
import Parser.Common exposing (int, number, spaces, text)
import Parser.Expression exposing (fromLeft, fromRight, inbetween, term)
import Parser.Sequence exposing (concat, exactly, oneOrMore, zeroOrMore)


type alias Type =
    Expr


{-| A Lambda calculus Expression.

TODO: figure out how to have a "Formatter" type to write, like "Parser" to read
TODO: rename "Parser" to "Reader" and "Formatter" to "Writer" (?)

-}
type Expr
    = Int Int --         42         Integer value
    | Num Float --       3.14       Number value
    | Var String --      x          Variable
    | Fun Type Type --   T1 -> T2   Function type
    | App Expr Expr --   e1 e2      Application (function call)
    | For String Type -- ∀x. T      Type abstraction (for all types)
    | Lam String Expr -- λx. e      Lambda abstraction (function definition)
    | TE Expr Type --    e : T      Typed expression


{-| An error from parsing or evaluation.
-}
type Error
    = SyntaxError Parser.Error
    | VariableNotFound String
    | TypeMismatch Expr Expr -- got, expected


type alias Env =
    { names : Dict String ( Expr, Type )
    , nameSeed : Int
    }


type alias Context a =
    Env -> Result Error ( a, Env )


run : Context a -> Result Error a
run ctx =
    let
        env =
            { names =
                Dict.fromList
                    -- TODO: Find a way to customize this? Maybe part of the AST?
                    [ ( "Any", ( Var "Any", Var "Any" ) ) --    Any types (free type)
                    , ( "Type", ( Var "Type", Var "Type" ) ) -- Type of types (Kind)
                    , ( "Int", ( Var "Int", Var "Type" ) ) --   Int type
                    , ( "Num", ( Var "Num", Var "Type" ) ) --   Num type
                    ]
            , nameSeed = 1
            }
    in
    Result.map Tuple.first (ctx env)


{-|

    withValue 5 |> run --> Ok 5

-}
withValue : a -> Context a
withValue x =
    \env -> Ok ( x, env )


{-|

    withError (VariableNotFound "x") |> run --> Err (VariableNotFound "x")

-}
withError : Error -> Context a
withError err =
    \_ -> Err err


{-|

    import Lambda

    withResult (Ok 5) |> run                       --> Ok 5
    withResult (Err (VariableNotFound "x")) |> run --> Err (VariableNotFound "x")

-}
withResult : Result Error a -> Context a
withResult result =
    \env -> Result.map (\x -> ( x, env )) result



-- Definitions


{-|

    import Lambda exposing (Expr(..), define, run)

    -- We evaluate the Expression to get its type.
    define "x" (Var "y") |> run --> Err (VariableNotFound "y")
    define "x" (Int 42) |> run  --> Ok ( Int 42, Var "Int" )

    -- Since we already know the type of a typed expression (TE),
    -- we can evaluate it lazily. This simplifies recursive definitions.
    define "x" (TE (Var "y") (Var "a")) |> run   --> Err (VariableNotFound "a")
    define "x" (TE (Var "y") (Var "Int")) |> run --> Ok ( Var "y", Var "Int" )

-}
define : String -> Expr -> Context ( Expr, Type )
define name value =
    case value of
        TE e typ ->
            -- We already know the type, so we can skip evaluating the value.
            -- The value is lazily evaluated when the variable is accessed.
            andThen
                (\( t, _ ) env ->
                    Ok ( ( e, t ), { env | names = Dict.insert name ( e, t ) env.names } )
                )
                (eval typ)

        _ ->
            -- Evaluate the value to get its type, the define it.
            andThen (\( e, t ) -> define name (TE e t))
                (eval value)


{-|

    import Lambda exposing (Expr(..), get, run)

    get "x" |> run --> Err (VariableNotFound "x")
    define "x" (Int 42) |> andThen (\_ -> get "x") |> run --> Ok ( Int 42, Var "Int" )

    andThen3
        (\_ _ _ -> get "x")
        (define "x" (TE (Var "y") (Var "Int")))
        (define "y" (TE (Var "z") (Var "Int")))
        (define "z" (Int 42))
        |> run
    --> Ok ( Int 42, Var "Int" )

-}
get : String -> Context ( Expr, Type )
get name =
    \env ->
        case Dict.get name env.names of
            Just ( value, typ ) ->
                if value == Var name then
                    Ok ( ( value, typ ), env )

                else
                    andThen
                        (\( v, t ) -> define name (TE v t))
                        (eval (TE value typ))
                        env

            Nothing ->
                Err (VariableNotFound name)


{-|

    import Lambda exposing (Expr(..), newName, run)

    -- It starts with 'a', and advances the `nameSeed`.
    run newName --> Ok "a"

    -- If that name already exists, it tries with the next one.
    define "a" (Int 42) |> andThen (\_ -> newName) |> run --> Ok "b"

-}
newName : Context String
newName =
    \env ->
        let
            name =
                (case toBase (Char.toCode 'z' - Char.toCode 'a' + 2) env.nameSeed of
                    x :: xs ->
                        x - 1 :: xs

                    [] ->
                        [ 0 ]
                )
                    |> List.map (\x -> x + Char.toCode 'a')
                    |> List.map Char.fromCode
                    |> String.fromList
        in
        if Dict.member name env.names then
            newName { env | nameSeed = env.nameSeed + 1 }

        else
            Ok ( name, { env | nameSeed = env.nameSeed + 1 } )



-- Evaluation


{-|

    import Lambda exposing (Expr(..), define, eval, run)

    -- Values
    eval (Int 42) |> run   --> Ok ( Int 42, Var "Int" )
    eval (Num 3.14) |> run --> Ok ( Num 3.14, Var "Num" )

    -- Variables
    eval (Var "x") |> run --> Err (VariableNotFound "x")
    define "x" (Int 42)
        |> andThen (\_ -> eval (Var "x"))
        |> run
    --> Ok ( Int 42, Var "Int" )

    -- Built-in types
    eval (Var "Any") |> run  --> Ok ( Var "Any", Var "Any" )
    eval (Var "Type") |> run --> Ok ( Var "Type", Var "Type" )
    eval (Var "Int") |> run  --> Ok ( Var "Int", Var "Type" )
    eval (Var "Num") |> run  --> Ok ( Var "Num", Var "Type" )

    -- Function type
    eval (Fun (Var "Int") (Var "Num")) |> run --> Ok ( Fun (Var "Int") (Var "Num"), Fun (Var "Type") (Var "Type") )

    -- Application
    define "f" (TE (Var "f") (Fun (Var "Int") (Var "Num")))
        |> andThen (\_ -> eval (Var "f"))
        |> run
    --> Ok ( Var "f", Fun (Var "Int") (Var "Num") )

    define "f" (TE (Var "f") (Fun (Var "Int") (Var "Num")))
        |> andThen (\_ -> eval (App (Var "f") (Int 42)))
        |> run
    --> Ok ( App (Var "f") (Int 42), Var "Num" )

    -- Type abstraction
    eval (For "a" (Var "a")) |> run   --> Ok ( For "a" (Var "a"), Var "Type" )
    eval (For "a" (Var "Int")) |> run --> Ok ( For "a" (Var "Int"), Var "Type" )
    eval (For "a" (Fun (Var "a") (Var "a"))) |> run --> Ok ( For "a" (Fun (Var "a") (Var "a")), Var "Type" )

    -- Lambda abstraction
    eval (Lam "x" (Int 42)) |> run  --> Ok ( Lam "x" (Int 42), For "a" (Fun (Var "a") (Var "Int")) )
    eval (Lam "x" (Var "x")) |> run --> Ok ( Lam "x" (Var "x"), For "a" (Fun (Var "a") (Var "a")) )
    eval (Lam "x" (Lam "y" (Var "x"))) |> run --> Ok ( Lam "x" (Lam "y" (Var "x")), For "a" (Fun (Var "a") (For "b" (Fun (Var "b") (Var "a")))) )

    -- Typed expression
    eval (TE (Int 42) (Var "Int")) |> run --> Ok ( Int 42, Var "Int" )
    eval (TE (Int 42) (Var "x")) |> run   --> Err (VariableNotFound "x")
    eval (TE (Int 42) (Var "Num")) |> run --> Err (TypeMismatch (Var "Num") (Var "Int"))
    eval (TE (Int 42) (For "a" (Var "a"))) |> run --> Ok ( Int 42, Var "Int" )

    -- β-reduction
    eval (App (Lam "x" (Var "x")) (Int 42)) |> run --> Ok ( Int 42, Var "Int" )

    -- Name shadowing

-}
eval : Expr -> Context ( Expr, Type )
eval expr =
    case expr of
        Int _ ->
            withValue ( expr, Var "Int" )

        Num _ ->
            withValue ( expr, Var "Num" )

        Var x ->
            get x

        Fun t1 t2 ->
            map2
                (\( type1, kind1 ) ( type2, kind2 ) ->
                    ( Fun type1 type2, Fun kind1 kind2 )
                )
                (eval t1)
                (eval t2)

        App e1 e2 ->
            andThen3
                (\( outType, _ ) ( func, funcType ) ( arg, argType ) ->
                    andThen
                        (\_ ->
                            case func of
                                Lam x e ->
                                    define x arg |> andThen (\_ -> eval e)

                                _ ->
                                    map (Tuple.pair (App func arg))
                                        (eval outType |> map Tuple.first)
                        )
                        (unify (Fun argType outType) funcType)
                )
                (newName |> andThen (\x -> define x (Var "Any")))
                (eval e1)
                (eval e2)

        For x t ->
            andThen2 (\_ value -> withValue ( For x value, Var "Type" ))
                (define x (TE (Var x) (Var "Any")))
                (eval t |> map Tuple.first)

        Lam x e ->
            andThen
                (\xt ->
                    andThen3
                        (\_ _ ( y, yt ) ->
                            map Tuple.first
                                (eval (For xt (Fun (Var xt) yt)))
                                |> map (Tuple.pair (Lam x y))
                        )
                        (eval (For xt (Var xt)))
                        (define x (TE (Var x) (Var xt)))
                        (eval e)
                )
                newName

        TE value typ ->
            andThen
                (\( e, et ) ->
                    map (\t -> ( e, t )) (unify typ et)
                )
                (eval value)


{-|

    import Lambda exposing (Expr (..), unify, run)

    -- Simple types must be equal to unify successfully.
    unify (Var "Int") (Var "Int") |> run --> Ok (Var "Int")
    unify (Var "Int") (Var "Num") |> run -- Err (TypeMismatch (Var "Int") (Var "Num"))

    -- And since we have dependent types, any valid expression is a valid type.
    unify (Int 42) (Int 42) |> run --> Ok ( Int 42 )
    unify (Int 42) (Int 43) |> run --> Err (TypeMismatch (Int 42) (Int 43))

    -- Generic types can be bound to other types.
    unify (For "a" (Var "a")) (Var "Int") |> run --> Ok (Var "Int")
    unify (Var "Int") (For "a" (Var "a")) |> run --> Ok (Var "Int")

    -- Function types must match both input and output types.
    unify (Var "Int") (Fun (Var "Int") (Var "Num")) |> run --> Err (TypeMismatch (Var "Int") (Fun (Var "Int") (Var "Num")))
    unify (Fun (Var "Int") (Var "Num")) (Var "Int") |> run --> Err (TypeMismatch (Fun (Var "Int") (Var "Num")) (Var "Int"))
    unify (Fun (Var "Int") (Var "Num")) (Fun (Var "Int") (Var "Num")) |> run --> Ok (Fun (Var "Int") (Var "Num"))
    unify (Fun (Var "Int") (Var "Num")) (Fun (Var "Int") (Var "Int")) |> run --> Err (TypeMismatch (Var "Num") (Var "Int"))
    unify (Fun (Var "Int") (Var "Num")) (Fun (Var "Num") (Var "Num")) |> run --> Err (TypeMismatch (Var "Int") (Var "Num"))

    -- Generic function types are also possible.
    unify (For "a" (Fun (Var "a") (Var "a"))) (Fun (Var "Int") (Var "Int")) |> run --> Ok (Fun (Var "Int") (Var "Int"))
    unify (For "a" (Fun (Var "a") (Var "a"))) (Fun (Var "Int") (Var "Num")) |> run --> Err (TypeMismatch (Var "Int") (Var "Num"))

    -- Type variable name clashes are also possible.
    unify (For "a" (Fun (Var "a") (Var "Int"))) (For "a" (Fun (Var "Int") (Var "a"))) |> run --> Ok (Fun (Var "Int") (Var "Int"))
    unify (For "a" (Fun (Var "a") (Var "Int"))) (For "a" (Fun (Var "Num") (Var "a"))) |> run --> Err (TypeMismatch (Var "Int") (Var "Num"))

-}
unify : Type -> Type -> Context Type
unify type1 type2 =
    andThen2
        (\( typ1, knd1 ) ( typ2, knd2 ) ->
            case ( ( typ1, knd1 ), ( typ2, knd2 ) ) of
                ( ( Fun a1 b1, _ ), ( Fun a2 b2, _ ) ) ->
                    map2 Fun (unify a1 a2) (unify b1 b2)

                ( ( For _ t1, _ ), ( t2, _ ) ) ->
                    unify t1 t2

                ( ( t1, _ ), ( For _ t2, _ ) ) ->
                    unify t1 t2

                ( ( Var x, Var "Any" ), ( t2, _ ) ) ->
                    define x t2 |> map Tuple.first

                ( ( t1, _ ), ( Var x, Var "Any" ) ) ->
                    define x t1 |> map Tuple.first

                _ ->
                    if ( typ1, knd1 ) == ( typ2, knd2 ) then
                        withValue typ1

                    else
                        withError (TypeMismatch typ1 typ2)
        )
        (eval type1)
        (eval type2)



-- Chaining


{-|

    map (\x -> x + 22)
        (withValue 20)
        |> run
    --> Ok 42

-}
map : (a -> b) -> Context a -> Context b
map f ctx =
    andThen (\a -> withValue (f a)) ctx


{-|

    map2 (+)
        (withValue 20)
        (withValue 22)
        |> run
    --> Ok 42

-}
map2 : (a -> b -> c) -> Context a -> Context b -> Context c
map2 f ctxA ctxB =
    andThen2 (\a b -> withValue (f a b)) ctxA ctxB


{-|

    map3 (\x y z -> x + y + z)
        (withValue 20)
        (withValue 12)
        (withValue 10)
        |> run
    --> Ok 42

-}
map3 : (a -> b -> c -> d) -> Context a -> Context b -> Context c -> Context d
map3 f ctxA ctxB ctxC =
    andThen3 (\a b c -> withValue (f a b c)) ctxA ctxB ctxC


{-|

    andThen (\x -> withValue (x + 22))
        (withValue 20)
        |> run
    --> Ok 42

-}
andThen : (a -> Context b) -> Context a -> Context b
andThen f ctx =
    \env -> Result.andThen (tupleMap f) (ctx env)


{-|

    andThen2 (\x y -> withValue (x + y))
        (withValue 20)
        (withValue 22)
        |> run
    --> Ok 42

-}
andThen2 : (a -> b -> Context c) -> Context a -> Context b -> Context c
andThen2 f ctxA ctxB =
    andThen (\a -> andThen (f a) ctxB) ctxA


{-|

    andThen3 (\x y z -> withValue (x + y + z))
        (withValue 20)
        (withValue 12)
        (withValue 10)
        |> run
    --> Ok 42

-}
andThen3 : (a -> b -> c -> Context d) -> Context a -> Context b -> Context c -> Context d
andThen3 f ctxA ctxB ctxC =
    andThen (\a -> andThen2 (f a) ctxB ctxC) ctxA



-- Syntax sugar (TODO: move to Lambda.Common)


{-|

    import Lambda

    Ok (function [] (Int 42))                -- parse "42"
    Ok (function [ "x" ] (Int 42))           -- parse "λx. 42"
    Ok (function [ "x", "y", "z" ] (Int 42)) -- parse "λx y z. 42"

-}
function : List String -> Expr -> Expr
function inputs output =
    List.foldr Lam output inputs


{-|

    import Lambda

    parse "42" |> Result.map asFunc         -- Ok ( [], Int 42 )
    parse "λx. 42" |> Result.map asFunc     -- Ok ( [ "x" ], Int 42 )
    parse "λx y z. 42" |> Result.map asFunc -- Ok ( [ "x", "y", "z" ], Int 42 )

-}
asFunc : Expr -> ( List String, Expr )
asFunc expr =
    let
        asFunc_ : List String -> Expr -> ( List String, Expr )
        asFunc_ xs y0 =
            case y0 of
                Lam x y ->
                    Tuple.mapFirst ((::) x) (asFunc_ xs y)

                _ ->
                    ( xs, y0 )
    in
    asFunc_ [] expr


{-|

    import Lambda

    Ok (forAll [] (Var "Int"))              -- parse "Int"
    Ok (forAll [ "a" ] (Var "a"))           -- parse "∀a. a"
    Ok (forAll [ "a", "b", "c" ] (Var "a")) -- parse "∀a b c. a"

-}
forAll : List String -> Expr -> Expr
forAll inputs output =
    List.foldr For output inputs


{-|

    import Lambda

    parse "42" |> Result.map asForAll         -- Ok ( [], Int 42 )
    parse "∀x. 42" |> Result.map asForAll     -- Ok ( [ "x" ], Int 42 )
    parse "∀x y z. 42" |> Result.map asForAll -- Ok ( [ "x", "y", "z" ], Int 42 )

-}
asForAll : Expr -> ( List String, Expr )
asForAll expr =
    let
        asForAll_ : List String -> Expr -> ( List String, Expr )
        asForAll_ xs y0 =
            case y0 of
                For x y ->
                    Tuple.mapFirst ((::) x) (asForAll_ xs y)

                _ ->
                    ( xs, y0 )
    in
    asForAll_ [] expr


{-|

    import Lambda

    Ok (functionType [] (Var "a"))                            -- parse "a"
    Ok (functionType [ Var "a" ] (Var "b"))                   -- parse "a -> b"
    Ok (functionType [ Var "a", Var "b", Var "c" ] (Var "d")) -- parse "a -> b -> c -> d"

-}
functionType : List Type -> Type -> Type
functionType inputs output =
    List.foldr Fun output inputs


{-|

    import Lambda

    parse "a" |> Result.map asFuncType                -- Ok ( [], Var "a" )
    parse "a -> b" |> Result.map asFuncType           -- Ok ( [ Var "a" ], Var "b" )
    parse "a -> b -> c -> d" |> Result.map asFuncType -- Ok ( [ Var "a", Var "b", Var "c" ], Var "d" )

-}
asFuncType : Type -> ( List Type, Type )
asFuncType typ =
    let
        asFuncType_ : List Type -> Type -> ( List Type, Type )
        asFuncType_ xs y0 =
            case y0 of
                Fun x y ->
                    Tuple.mapFirst ((::) x) (asFuncType_ xs y)

                _ ->
                    ( xs, y0 )
    in
    asFuncType_ [] typ


{-|

    import Lambda

    Ok (call (Var "f") [])                            -- parse "f"
    Ok (call (Var "f") [ Var "x" ])                   -- parse "f x"
    Ok (call (Var "f") [ Var "x", Var "y", Var "z" ]) -- parse "f x y z"

-}
call : Expr -> List Expr -> Expr
call f xs =
    List.foldl (\x y -> App y x) f xs


{-|

    import Lambda

    parse "f" |> Result.map asCall       -- Ok ( Var "f", [] )
    parse "f x" |> Result.map asCall     -- Ok ( Var "f", [ Var "x" ] )
    parse "f x y z" |> Result.map asCall -- Ok ( Var "f", [ Var "x", Var "y", Var "z" ] )

-}
asCall : Expr -> ( Expr, List Expr )
asCall expr =
    let
        asCall_ : Expr -> List Expr -> ( Expr, List Expr )
        asCall_ e xs =
            case e of
                App f x ->
                    asCall_ f (x :: xs)

                _ ->
                    ( e, xs )
    in
    asCall_ expr []


exp : Expr -> Expr -> Expr
exp x y =
    call (Var "^") [ x, y ]


mul : Expr -> Expr -> Expr
mul x y =
    call (Var "*") [ x, y ]


add : Expr -> Expr -> Expr
add x y =
    call (Var "+") [ x, y ]


{-|

    import Lambda

    Ok (letVar "x" (Var "y") (Var "z"))                -- parse "(λx. z) y"
    Ok (letVar "x" (Var "y") (Var "z"))                -- parse "x := y; z"
    Ok (letVar "x" (TE (Var "y") (Var "a")) (Var "z")) -- parse "x : a = y; z"

-}
letVar : String -> Expr -> Expr -> Expr
letVar name value expr =
    App (Lam name expr) value



-- Utility functions


tupleMap : (a -> b -> c) -> ( a, b ) -> c
tupleMap f ( x, y ) =
    f x y


resultAndThen2 : (a -> b -> Result e c) -> Result e a -> Result e b -> Result e c
resultAndThen2 f result1 result2 =
    Result.andThen (\x -> Result.andThen (f x) result2) result1


toBase : Int -> Int -> List Int
toBase base num =
    if num == 0 then
        []

    else
        ((num // base) |> toBase base) ++ [ num |> modBy base ]



-- Reading and writing (TODO: move to Lambda.Phi)


{-| Reads a Lambda Expression.

    import Lambda exposing (Expr(..), parse)

    -- Values
    read "42"   -- withValue (Int 42)
    read "3.14" -- withValue (Num 3.14)

    -- Variables
    read "x" -- withValue (Var "x")

    -- Function type
    read "a -> b" -- withValue (Fun (Var "a") (Var "b"))

    -- Application
    read "f x" -- withValue (App (Var "f") (Var "x"))

    -- Type abstraction
    read "∀a. b"   -- withValue (For "a" (Var "b"))
    read "∀a b. c" -- withValue (For "a" (For "b" (Var "c")))

    -- Lambda abstraction
    read "λx. y"   -- withValue (Lam "x" (Var "y"))
    read "λx y. z" -- withValue (Lam "x" (Lam "y" (Var "z")))

    -- Typed Expression
    read "x : a" -- withValue (TE (Var "x") (Var "a"))

    -- Variable definitions
    read "x := y; z"    -- withValue (App (Lam "x" (Var "z")) (Var "y"))
    read "x : a = y; z" -- withValue (App (Lam "x" (Var "z")) (TE (Var "y") (Var "a")))

-}
read : String -> Context Expr
read text =
    Parser.parse text exprP
        |> Result.mapError SyntaxError
        |> withResult


{-| Writes a Lambda Expression.

    import Lambda exposing (Expr(..), write)

    -- Values
    write (Int 42)   -- "42"
    write (Num 3.14) -- "3.14"

    -- Variables
    write (Var "x") -- "x"

    -- Function type
    write (Fun (Var "a") (Var "b")) -- "a -> b"

    -- Application
    write (App (Var "f") (Var "x")) -- "f x"

    -- Type abstraction
    write (For "a" (Var "b")) -- "∀a. b"

    -- Lambda abstraction
    write (Lam "x" (Var "y")) -- "λx. y"

    -- Typed Expression
    write (TE (Var "x") (Var "a")) -- "x : a"

    -- Variable definitions
    write (App (Lam "x" (Var "z")) (Var "y"))                -- "x := y; z"
    write (App (Lam "x" (Var "z")) (TE (Var "y") (Var "a"))) -- "x : a = y; z"

-}
write : Expr -> String
write expr =
    let
        precedence : Expr -> Int
        precedence e =
            case e of
                TE _ _ ->
                    0

                Fun _ _ ->
                    1

                Lam _ _ ->
                    1

                For _ _ ->
                    1

                App (App (Var "+") _) _ ->
                    3

                App (App (Var "*") _) _ ->
                    4

                App (App (Var "^") _) _ ->
                    5

                App _ _ ->
                    6

                Int _ ->
                    7

                Num _ ->
                    7

                Var _ ->
                    7

        unop1 : Expr -> String
        unop1 e =
            if precedence e < precedence expr then
                "(" ++ write e ++ ")"

            else
                write e

        unop2 : Expr -> String
        unop2 e =
            if precedence e <= precedence expr then
                "(" ++ write e ++ ")"

            else
                write e
    in
    case expr of
        Int k ->
            String.fromInt k

        Num k ->
            String.fromFloat k

        Var x ->
            x

        Fun t1 t2 ->
            unop2 t1 ++ " -> " ++ unop1 t2

        App (Lam x e) (TE v t) ->
            x ++ " : " ++ write t ++ " = " ++ write v ++ "; " ++ write e

        App (Lam x e) v ->
            x ++ " := " ++ write v ++ "; " ++ write e

        App (App (Var "^") e1) e2 ->
            unop2 e1 ++ " ^ " ++ unop1 e2

        App (App (Var "*") e1) e2 ->
            unop1 e1 ++ " * " ++ unop2 e2

        App (App (Var "+") e1) e2 ->
            unop1 e1 ++ " + " ++ unop2 e2

        App e1 e2 ->
            unop1 e1 ++ " " ++ unop2 e2

        For _ _ ->
            tupleMap
                (\xs y -> "∀" ++ String.join " " xs ++ ". " ++ write y)
                (asForAll expr)

        Lam _ _ ->
            tupleMap
                (\xs y -> "λ" ++ String.join " " xs ++ ". " ++ write y)
                (asFunc expr)

        TE e t ->
            unop1 e ++ " : " ++ unop2 t



-- Parsers


identifierP : Parser String
identifierP =
    succeed String.fromList
        |> take
            (concat
                [ exactly 1 letter
                , zeroOrMore (oneOf [ letter, digit, char '_' ])
                ]
            )
        |> drop spaces


exprP : Parser Expr
exprP =
    let
        funcP : Parser Expr
        funcP =
            succeed function
                |> drop (char 'λ')
                |> drop spaces
                |> take
                    (oneOrMore
                        (succeed identity
                            |> take identifierP
                            |> drop spaces
                        )
                    )
                |> drop (text ".")
                |> drop spaces
                |> take (lazy (\_ -> exprP))

        forAllP : Parser Expr
        forAllP =
            succeed forAll
                |> drop (char '∀')
                |> drop spaces
                |> take
                    (oneOrMore
                        (succeed identity
                            |> take identifierP
                            |> drop spaces
                        )
                    )
                |> drop (text ".")
                |> drop spaces
                |> take (lazy (\_ -> exprP))

        letVarP : Parser Expr
        letVarP =
            succeed letVar
                |> take identifierP
                |> drop spaces
                |> take
                    (oneOf
                        [ succeed identity
                            |> drop (text ":=")
                            |> drop spaces
                            |> take (lazy (\_ -> exprP))
                        , succeed (\t e -> TE e t)
                            |> drop (char ':')
                            |> drop spaces
                            |> take (lazy (\_ -> exprP))
                            |> drop spaces
                            |> drop (char '=')
                            |> drop spaces
                            |> take (lazy (\_ -> exprP))
                        ]
                    )
                |> drop spaces
                |> drop (char ';')
                |> drop spaces
                |> take (lazy (\_ -> exprP))
    in
    Parser.Expression.expression
        [ [ fromLeft App spaces ]
        , [ fromRight exp (char '^') ]
        , [ fromLeft mul (char '*') ]
        , [ fromLeft add (char '+') ]
        , [ fromRight Fun (text "->") ]
        , [ fromLeft TE (char ':') ]

        -- , [ fromLeft Or (text "|") ]
        , [ inbetween identity (char '(') (char ')')
          , term identity funcP
          , term identity forAllP
          , term identity letVarP
          , term Int int
          , term Num number
          , term Var identifierP
          ]
        ]

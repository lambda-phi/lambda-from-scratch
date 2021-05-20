module Lambda exposing
    ( Env
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
    , func
    , funcType
    , letVar
    , map
    , mul
    , newEnv
    , read
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
import DisjointSet exposing (DisjointSet)
import Parser exposing (Parser, andThen, drop, lazy, oneOf, parse, succeed, take)
import Parser.Char exposing (char, digit, letter)
import Parser.Common exposing (int, number, spaces, text)
import Parser.Expression exposing (fromLeft, fromRight, inbetween, prefix, term)
import Parser.Sequence exposing (concat, exactly, oneOrMore, zeroOrMore)
import Set exposing (Set)


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
    | Fun Expr Expr --   T1 -> T2   Function type
    | App Expr Expr --   e1 e2      Application (function call)
    | For String Expr -- ∀x. T      Type abstraction (for all types)
    | Lam String Expr -- λx. e      Lambda abstraction (function definition)
    | TE Expr Expr --    e : T      Typed expression


{-| An error from parsing or evaluation.
-}
type Error
    = SyntaxError Parser.Error
    | VariableNotFound String
    | TypeMismatch Expr Expr -- got, expected



-- TODO: consider changing `Env a = Result Error {...}`


type alias Env a =
    { names : Dict String ( Expr, Expr ) -- { name : (value, type) }
    , types : DisjointSet Expr -- equivalent types / substitutions
    , nameSeed : Int
    , result : Result Error a
    }


newEnv : Env ()
newEnv =
    { names =
        Dict.fromList
            -- TODO: Find a way to customize this? Maybe part of the AST?
            [ ( "@", ( Var "@", Var "@" ) ) --          Free types
            , ( "Type", ( Var "Type", Var "Type" ) ) -- Type of types (Kind)
            , ( "Int", ( Var "Int", Var "Type" ) ) --   Int type
            , ( "Num", ( Var "Num", Var "Type" ) ) --   Num type
            ]
    , types = DisjointSet.empty
    , nameSeed = 1
    , result = Ok ()
    }


{-| Reads a Lambda Expression.

    import Lambda exposing (Expr(..), read)

    -- Values
    read "42"   --> Ok (Int 42)
    read "3.14" --> Ok (Num 3.14)

    -- Variables
    read "x" --> Ok (Var "x")

    -- Function type
    read "a -> b" --> Ok (Fun (Var "a") (Var "b"))

    -- Application
    read "f x" --> Ok (App (Var "f") (Var "x"))

    -- Type abstraction
    read "∀a. b"   --> Ok (For "a" (Var "b"))
    read "∀a b. c" --> Ok (For "a" (For "b" (Var "c")))

    -- Lambda abstraction
    read "λx. y"   --> Ok (Lam "x" (Var "y"))
    read "λx y. z" --> Ok (Lam "x" (Lam "y" (Var "z")))

    -- Typed Expression
    read "x : a" --> Ok (TE (Var "x") (Var "a"))

    -- Variable definitions
    read "x := y; z"    --> Ok (App (Lam "x" (Var "z")) (Var "y"))
    read "x : a = y; z" --> Ok (App (Lam "x" (Var "z")) (TE (Var "y") (Var "a")))

    -- Operator precedence
    read "x y z"    --> Ok (App (App (Var "x") (Var "y")) (Var "z"))
    read "x y ^ z"  --> Ok (exp (App (Var "x") (Var "y")) (Var "z"))
    read "x y * z"  --> Ok (mul (App (Var "x") (Var "y")) (Var "z"))
    read "x y + z"  --> Ok (add (App (Var "x") (Var "y")) (Var "z"))
    read "x y -> z" --> Ok (Fun (App (Var "x") (Var "y")) (Var "z"))
    read "x y : z"  --> Ok (TE (App (Var "x") (Var "y")) (Var "z"))
    read "x λy. z"  --> Ok (App (Var "x") (Lam "y" (Var "z")))

    read "x ^ y z"    --> Ok (exp (Var "x") (App (Var "y") (Var "z")))
    read "x ^ y ^ z"  --> Ok (exp (Var "x") (exp (Var "y") (Var "z")))
    read "x ^ y * z"  --> Ok (mul (exp (Var "x") (Var "y")) (Var "z"))
    read "x ^ y + z"  --> Ok (add (exp (Var "x") (Var "y")) (Var "z"))
    read "x ^ y -> z" --> Ok (Fun (exp (Var "x") (Var "y")) (Var "z"))
    read "x ^ y : z"  --> Ok (TE (exp (Var "x") (Var "y")) (Var "z"))
    read "x ^ λy. z"  --> Ok (exp (Var "x") (Lam "y" (Var "z")))

    read "x * y z"    --> Ok (mul (Var "x") (App (Var "y") (Var "z")))
    read "x * y ^ z"  --> Ok (mul (Var "x") (exp (Var "y") (Var "z")))
    read "x * y * z"  --> Ok (mul (mul (Var "x") (Var "y")) (Var "z"))
    read "x * y + z"  --> Ok (add (mul (Var "x") (Var "y")) (Var "z"))
    read "x * y : z"  --> Ok (TE (mul (Var "x") (Var "y")) (Var "z"))
    read "x * y -> z" --> Ok (Fun (mul (Var "x") (Var "y")) (Var "z"))
    read "x * λy. z"  --> Ok (mul (Var "x") (Lam "y" (Var "z")))

    read "x + y z"    --> Ok (add (Var "x") (App (Var "y") (Var "z")))
    read "x + y ^ z"  --> Ok (add (Var "x") (exp (Var "y") (Var "z")))
    read "x + y * z"  --> Ok (add (Var "x") (mul (Var "y") (Var "z")))
    read "x + y + z"  --> Ok (add (add (Var "x") (Var "y")) (Var "z"))
    read "x + y -> z" --> Ok (Fun (add (Var "x") (Var "y")) (Var "z"))
    read "x + y : z"  --> Ok (TE (add (Var "x") (Var "y")) (Var "z"))
    read "x + λy. z"  --> Ok (add (Var "x") (Lam "y" (Var "z")))

    read "x -> y z"    --> Ok (Fun (Var "x") (App (Var "y") (Var "z")))
    read "x -> y ^ z"  --> Ok (Fun (Var "x") (exp (Var "y") (Var "z")))
    read "x -> y * z"  --> Ok (Fun (Var "x") (mul (Var "y") (Var "z")))
    read "x -> y + z"  --> Ok (Fun (Var "x") (add (Var "y") (Var "z")))
    read "x -> y : z"  --> Ok (TE (Fun (Var "x") (Var "y")) (Var "z"))
    read "x -> y -> z" --> Ok (Fun (Var "x") (Fun (Var "y") (Var "z")))
    read "x -> λy. z"  --> Ok (Fun (Var "x") (Lam "y" (Var "z")))

    read "x : y z"    --> Ok (TE (Var "x") (App (Var "y") (Var "z")))
    read "x : y ^ z"  --> Ok (TE (Var "x") (exp (Var "y") (Var "z")))
    read "x : y * z"  --> Ok (TE (Var "x") (mul (Var "y") (Var "z")))
    read "x : y + z"  --> Ok (TE (Var "x") (add (Var "y") (Var "z")))
    read "x : y -> z" --> Ok (TE (Var "x") (Fun (Var "y") (Var "z")))
    read "x : y : z"  --> Ok (TE (TE (Var "x") (Var "y")) (Var "z"))
    read "x : λy. z"  --> Ok (TE (Var "x") (Lam "y" (Var "z")))

    read "λx. y z"    --> Ok (Lam "x" (App (Var "y") (Var "z")))
    read "λx. y ^ z"  --> Ok (Lam "x" (exp (Var "y") (Var "z")))
    read "λx. y * z"  --> Ok (Lam "x" (mul (Var "y") (Var "z")))
    read "λx. y + z"  --> Ok (Lam "x" (add (Var "y") (Var "z")))
    read "λx. y -> z" --> Ok (Lam "x" (Fun (Var "y") (Var "z")))
    read "λx. y : z"  --> Ok (Lam "x" (TE (Var "y") (Var "z")))
    read "λx. λy. z"  --> Ok (Lam "x" (Lam "y" (Var "z")))

    read "x + (y + z)" --> Ok (add (Var "x") (add (Var "y") (Var "z")))
    read "(x ^ y) ^ z" --> Ok (exp (exp (Var "x") (Var "y")) (Var "z"))

-}
read : String -> Result Error Expr
read txt =
    parse txt exprP
        |> Result.mapError SyntaxError


{-| Writes a Lambda Expression.

    import Lambda exposing (Expr(..), write)

    -- Values
    write (Int 42)   --> "42"
    write (Num 3.14) --> "3.14"

    -- Variables
    write (Var "x") --> "x"

    -- Function type
    write (Fun (Var "a") (Var "b")) --> "a -> b"

    -- Application
    write (App (Var "f") (Var "x")) --> "f x"

    -- Type abstraction
    write (For "a" (Var "b")) --> "∀a. b"

    -- Lambda abstraction
    write (Lam "x" (Var "y")) --> "λx. y"

    -- Typed Expression
    write (TE (Var "x") (Var "a")) --> "x : a"

    -- Variable definitions
    write (App (Lam "x" (Var "z")) (Var "y"))                --> "x := y; z"
    write (App (Lam "x" (Var "z")) (TE (Var "y") (Var "a"))) --> "x : a = y; z"

    -- Operator precedence
    write (App (App (Var "x") (Var "y")) (Var "z")) --> "x y z"
    write (exp (App (Var "x") (Var "y")) (Var "z")) --> "x y ^ z"
    write (mul (App (Var "x") (Var "y")) (Var "z")) --> "x y * z"
    write (add (App (Var "x") (Var "y")) (Var "z")) --> "x y + z"
    write (Fun (App (Var "x") (Var "y")) (Var "z")) --> "x y -> z"
    write (TE (App (Var "x") (Var "y")) (Var "z"))  --> "x y : z"
    write (App (Var "x") (Lam "y" (Var "z")))       --> "x (λy. z)"

    write (exp (Var "x") (App (Var "y") (Var "z"))) --> "x ^ y z"
    write (exp (Var "x") (exp (Var "y") (Var "z"))) --> "x ^ y ^ z"
    write (mul (exp (Var "x") (Var "y")) (Var "z")) --> "x ^ y * z"
    write (add (exp (Var "x") (Var "y")) (Var "z")) --> "x ^ y + z"
    write (Fun (exp (Var "x") (Var "y")) (Var "z")) --> "x ^ y -> z"
    write (TE (exp (Var "x") (Var "y")) (Var "z"))  --> "x ^ y : z"
    write (exp (Var "x") (Lam "y" (Var "z")))       --> "x ^ (λy. z)"

    write (mul (Var "x") (App (Var "y") (Var "z"))) --> "x * y z"
    write (mul (Var "x") (exp (Var "y") (Var "z"))) --> "x * y ^ z"
    write (mul (mul (Var "x") (Var "y")) (Var "z")) --> "x * y * z"
    write (add (mul (Var "x") (Var "y")) (Var "z")) --> "x * y + z"
    write (Fun (mul (Var "x") (Var "y")) (Var "z")) --> "x * y -> z"
    write (TE (mul (Var "x") (Var "y")) (Var "z"))  --> "x * y : z"
    write (mul (Var "x") (Lam "y" (Var "z")))       --> "x * (λy. z)"

    write (add (Var "x") (App (Var "y") (Var "z"))) --> "x + y z"
    write (add (Var "x") (exp (Var "y") (Var "z"))) --> "x + y ^ z"
    write (add (Var "x") (mul (Var "y") (Var "z"))) --> "x + y * z"
    write (add (add (Var "x") (Var "y")) (Var "z")) --> "x + y + z"
    write (Fun (add (Var "x") (Var "y")) (Var "z")) --> "x + y -> z"
    write (TE (add (Var "x") (Var "y")) (Var "z"))  --> "x + y : z"
    write (add (Var "x") (Lam "y" (Var "z")))       --> "x + (λy. z)"

    write (Fun (Var "x") (App (Var "y") (Var "z"))) --> "x -> y z"
    write (Fun (Var "x") (exp (Var "y") (Var "z"))) --> "x -> y ^ z"
    write (Fun (Var "x") (mul (Var "y") (Var "z"))) --> "x -> y * z"
    write (Fun (Var "x") (add (Var "y") (Var "z"))) --> "x -> y + z"
    write (Fun (Var "x") (Fun (Var "y") (Var "z"))) --> "x -> y -> z"
    write (TE (Fun (Var "x") (Var "y")) (Var "z"))  --> "x -> y : z"
    write (Fun (Var "x") (Lam "y" (Var "z")))       --> "x -> λy. z"

    write (TE (Var "x") (App (Var "y") (Var "z"))) --> "x : y z"
    write (TE (Var "x") (exp (Var "y") (Var "z"))) --> "x : y ^ z"
    write (TE (Var "x") (mul (Var "y") (Var "z"))) --> "x : y * z"
    write (TE (Var "x") (add (Var "y") (Var "z"))) --> "x : y + z"
    write (TE (Var "x") (Fun (Var "y") (Var "z"))) --> "x : y -> z"
    write (TE (TE (Var "x") (Var "y")) (Var "z"))  --> "x : y : z"
    write (TE (Var "x") (Lam "y" (Var "z")))       --> "x : λy. z"

    write (Lam "x" (App (Var "y") (Var "z"))) --> "λx. y z"
    write (Lam "x" (exp (Var "y") (Var "z"))) --> "λx. y ^ z"
    write (Lam "x" (mul (Var "y") (Var "z"))) --> "λx. y * z"
    write (Lam "x" (add (Var "y") (Var "z"))) --> "λx. y + z"
    write (Lam "x" (TE (Var "y") (Var "z")))  --> "λx. y : z"
    write (Lam "x" (Fun (Var "y") (Var "z"))) --> "λx. y -> z"
    write (Lam "x" (Lam "y" (Var "z")))       --> "λx y. z"

    write (add (Var "x") (add (Var "y") (Var "z"))) --> "x + (y + z)"
    write (exp (exp (Var "x") (Var "y")) (Var "z")) --> "(x ^ y) ^ z"

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



-- Syntax sugar


{-|

    import Lambda

    Ok (func [] (Int 42))                --> read "42"
    Ok (func [ "x" ] (Int 42))           --> read "λx. 42"
    Ok (func [ "x", "y", "z" ] (Int 42)) --> read "λx y z. 42"

-}
func : List String -> Expr -> Expr
func inputs output =
    List.foldr Lam output inputs


{-|

    import Lambda

    read "42" |> Result.map asFunc         --> Ok ( [], Int 42 )
    read "λx. 42" |> Result.map asFunc     --> Ok ( [ "x" ], Int 42 )
    read "λx y z. 42" |> Result.map asFunc --> Ok ( [ "x", "y", "z" ], Int 42 )

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

    Ok (forAll [] (Var "Int"))              --> read "Int"
    Ok (forAll [ "a" ] (Var "a"))           --> read "∀a. a"
    Ok (forAll [ "a", "b", "c" ] (Var "a")) --> read "∀a b c. a"

-}
forAll : List String -> Expr -> Expr
forAll inputs output =
    List.foldr For output inputs


{-|

    import Lambda

    read "42" |> Result.map asForAll         --> Ok ( [], Int 42 )
    read "∀x. 42" |> Result.map asForAll     --> Ok ( [ "x" ], Int 42 )
    read "∀x y z. 42" |> Result.map asForAll --> Ok ( [ "x", "y", "z" ], Int 42 )

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

    Ok (funcType [] (Var "a"))                            --> read "a"
    Ok (funcType [ Var "a" ] (Var "b"))                   --> read "a -> b"
    Ok (funcType [ Var "a", Var "b", Var "c" ] (Var "d")) --> read "a -> b -> c -> d"

-}
funcType : List Type -> Type -> Type
funcType inputs output =
    List.foldr Fun output inputs


{-|

    import Lambda

    read "a" |> Result.map asFuncType                --> Ok ( [], Var "a" )
    read "a -> b" |> Result.map asFuncType           --> Ok ( [ Var "a" ], Var "b" )
    read "a -> b -> c -> d" |> Result.map asFuncType --> Ok ( [ Var "a", Var "b", Var "c" ], Var "d" )

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

    Ok (call (Var "f") [])                            --> read "f"
    Ok (call (Var "f") [ Var "x" ])                   --> read "f x"
    Ok (call (Var "f") [ Var "x", Var "y", Var "z" ]) --> read "f x y z"

-}
call : Expr -> List Expr -> Expr
call f xs =
    List.foldl (\x y -> App y x) f xs


{-|

    import Lambda

    read "f" |> Result.map asCall       --> Ok ( Var "f", [] )
    read "f x" |> Result.map asCall     --> Ok ( Var "f", [ Var "x" ] )
    read "f x y z" |> Result.map asCall --> Ok ( Var "f", [ Var "x", Var "y", Var "z" ] )

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

    Ok (letVar "x" (Var "y") (Var "z"))                --> read "(λx. z) y"
    Ok (letVar "x" (Var "y") (Var "z"))                --> read "x := y; z"
    Ok (letVar "x" (TE (Var "y") (Var "a")) (Var "z")) --> read "x : a = y; z"

-}
letVar : String -> Expr -> Expr -> Expr
letVar name value expr =
    App (Lam name expr) value



-- Type inference


{-|

    import Dict

    -- We evaluate the Expression to get its type.
    define "x" (Int 42) newEnv |> .names
    --> Dict.insert "x" (Int 42, Var "Int") newEnv.names

    -- Errors are part of the result, and the name remains undefined.
    define "x" (Var "y") newEnv |> .result --> Err (VariableNotFound "y")
    define "x" (Var "y") newEnv |> .names  --> newEnv.names

    -- Since we already know the type of a typed expression (TE),
    -- we can evaluate it lazily. This simplifies recursive definitions.
    define "x" (TE (Var "y") (Var "Int")) newEnv |> .names
    --> Dict.insert "x" (Var "y", Var "Int") newEnv.names

    -- But the type is still evaluated.
    define "x" (TE (Var "y") (Var "a")) newEnv |> .result --> Err (VariableNotFound "a")

-}
define : String -> Expr -> Env a -> Env a
define name value env =
    case value of
        TE expr typ ->
            andThen
                (\t _ -> { env | names = Dict.insert name ( expr, t ) env.names })
                (map Tuple.first (eval typ env))

        _ ->
            map (tupleMap TE) (eval value env)
                |> andThen (define_ name)
                |> andThen (\_ -> withResult env.result)


define_ : String -> Expr -> Env a -> Env a
define_ =
    -- Workaround for: https://github.com/elm/compiler/issues/2186
    define


{-|

    import Dict

    env : Env ()
    env =
        newEnv
            |> define "x" (Int 42)
            |> define "f" (TE (Var "f") (Fun (Var "Int") (Var "Num")))

    evalExpr : String -> Env (String, String)
    evalExpr expr =
        withResult (read expr) env
            |> andThen eval
            |> map (Tuple.mapBoth write write)

    -- Values
    evalExpr "42"   --> withValue ("42", "Int") env
    evalExpr "3.14" --> withValue ("3.14", "Num") env

    -- Variables
    evalExpr "x" --> withValue ("42", "Int") env
    evalExpr "y" --> withError (VariableNotFound "y") env

    -- Function type
    evalExpr "Int"        --> withValue ("Int", "Type") env
    evalExpr "Int -> Num" --> withValue ("Int -> Num", "Type -> Type") env

    -- Application
    evalExpr "f"    --> withValue ("f", "Int -> Num") env
    evalExpr "f 42" --> withValue ("f 42", "Num") env

    -- Type abstraction
    evalExpr "∀a. a"      --> withValue ("∀a. a", "Type") { env | names = Dict.insert "a" (Var "a", Var "@") env.names }
    evalExpr "∀a. Int"    --> withValue ("Int", "Type") env
    evalExpr "∀a. a -> a" --> withValue ("∀a. a -> a", "Type") { env | names = Dict.insert "a" (Var "a", Var "@") env.names }
    evalExpr "∀a. a -> ∀b. b -> ∀c. c" --> withValue ("∀a b c. a -> b -> c", "Type") { env | names = env.names |> Dict.insert "a" (Var "a", Var "@") |> Dict.insert "b" (Var "b", Var "@") |> Dict.insert "c" (Var "c", Var "@") }

    -- Lambda abstraction
    evalExpr "λx. x"     --> withValue ("λx. x", "∀a. a -> a") env
    evalExpr "λx. 42"    --> withValue ("λx. 42", "∀a. a -> Int") env
    evalExpr "λx y z. x" --> withValue ("λx y z. x", "∀a b c. a -> b -> c -> a") env

    -- Typed expression
    evalExpr "x : Int"   --> withValue ("42", "Int") env
    evalExpr "y : z"     --> withError (VariableNotFound "z") env
    evalExpr "y : Int"   --> withError (VariableNotFound "y") env
    evalExpr "x : Num"   --> withError (TypeMismatch (Var "Num") (Var "Int")) env
    evalExpr "x : ∀a. a" --> withValue ("42", "Int") env

    -- β-reduction

-}
eval : Expr -> Env a -> Env ( Expr, Type )
eval expr env =
    case expr of
        Int _ ->
            withValue ( expr, Var "Int" ) env

        Num _ ->
            withValue ( expr, Var "Num" ) env

        Var x ->
            case Dict.get x env.names of
                Just ( value, typ ) ->
                    if value == Var x then
                        withValue ( value, typ ) env

                    else
                        -- Lazy evaluation through typed Expressions
                        -- TODO: update the name definition to the result
                        eval (TE value typ) env

                Nothing ->
                    withError (VariableNotFound x) env

        Fun t1 t2 ->
            map2
                (\( ta, ka ) ( tb, kb ) ->
                    case asForAll tb of
                        ( typeVars, ttb ) ->
                            ( forAll typeVars (Fun ta ttb), Fun ka kb )
                )
                (eval_ t1 env)
                (eval_ t2)

        App e1 e2 ->
            andThen3
                (\outT ( v1, t1 ) ( v2, t2 ) env_ ->
                    map (\_ -> ( v1, v2, outT ))
                        (unify (Fun t2 outT) t1 env_)
                )
                (newTypeVar env)
                (eval_ e1)
                (eval_ e2)
                |> andThen
                    (\( v1, v2, outT ) env_ ->
                        case v1 of
                            -- β-reduction: computation happens here! 🎉
                            -- (λx. y) z  -- we now know x's value.
                            Lam x y ->
                                withValue v2 env_
                                    |> andThen (define x)
                                    |> eval_ y

                            _ ->
                                withValue
                                    ( App v1 v2
                                    , DisjointSet.find outT env_.types
                                        |> Maybe.withDefault outT
                                    )
                                    env_
                    )
                |> andThen (\result _ -> withValue result env)

        For x y ->
            env
                |> define x (TE (Var x) (Var "@"))
                |> eval_ y
                |> andThen
                    (\( value, typ ) env_ ->
                        if isVarInExpr x value then
                            withValue ( For x value, Var "Type" ) env_

                        else
                            withValue ( value, typ ) env
                    )

        -- TODO: clean up For's environment
        -- |> andThen (\result _ -> withValue result env)
        Lam x e ->
            let
                -- TODO: use newTypeVar
                xT =
                    Dict.keys env.names
                        |> Set.fromList
                        |> newVarName env.nameSeed
                        |> Tuple.first
            in
            env
                |> eval_ (For xT (Var xT))
                |> define x (TE (Var x) (Var xT))
                |> eval_ e
                |> andThen
                    (\( y, yT ) env_ ->
                        -- TODO: clean up this, basically we evaluate the Lambda's type
                        map (\( yyT, _ ) -> ( Lam x y, yyT ))
                            (eval_ (For xT (Fun (Var xT) yT)) env_)
                    )
                |> andThen (\result _ -> withValue result env)

        TE value typ ->
            andThen2
                (\( tt, _ ) ( e, et ) env_ ->
                    map (\t -> ( e, t ))
                        (unify tt et env_)
                )
                (eval_ typ env)
                (eval_ value)
                |> andThen (\result _ -> withValue result env)


eval_ : Expr -> Env a -> Env ( Expr, Type )
eval_ =
    -- Workaround for: https://github.com/elm/compiler/issues/2186
    eval


isVarInExpr : String -> Expr -> Bool
isVarInExpr name expr =
    -- TODO: replace with freeTypesOf
    case expr of
        Int _ ->
            False

        Num _ ->
            False

        Var x ->
            x == name

        App e1 e2 ->
            isVarInExpr name e1 || isVarInExpr name e2

        Fun t1 t2 ->
            isVarInExpr name t1 || isVarInExpr name t2

        Lam x y ->
            x /= name && isVarInExpr name y

        For x y ->
            x /= name && isVarInExpr name y

        TE e t ->
            isVarInExpr name e || isVarInExpr name t


{-|

    import Dict
    import DisjointSet

    -- Simple types must be equal to unify.
    unify (Var "Int") (Var "Int") newEnv --> withValue (Var "Int") newEnv
    unify (Var "Int") (Var "Num") newEnv --> withError (TypeMismatch (Var "Int") (Var "Num")) newEnv

    -- And since we have dependent types, any valid expression is a valid type.
    unify (Int 42) (Int 42) newEnv --> withValue (Int 42) newEnv
    unify (Int 42) (Int 43) newEnv --> withError (TypeMismatch (Int 42) (Int 43)) newEnv

    -- Generic types can be bound to other types.
    unify (For "a" (Var "a")) (Var "Int") newEnv --> withValue (Var "Int") { newEnv | names = Dict.insert "a" (Var "a", Var "@") newEnv.names, types = DisjointSet.union (Var "Int") (Var "a") newEnv.types }
    unify (Var "Int") (For "a" (Var "a")) newEnv --> withValue (Var "Int") { newEnv | names = Dict.insert "a" (Var "a", Var "@") newEnv.names, types = DisjointSet.union (Var "Int") (Var "a") newEnv.types }

    -- Function types must match both input and output types.
    unify (Var "Int") (Fun (Var "Int") (Var "Num")) newEnv                   --> withError (TypeMismatch (Var "Int") (Fun (Var "Int") (Var "Num"))) newEnv
    unify (Fun (Var "Int") (Var "Num")) (Var "Int") newEnv                   --> withError (TypeMismatch (Fun (Var "Int") (Var "Num")) (Var "Int")) newEnv
    unify (Fun (Var "Int") (Var "Num")) (Fun (Var "Int") (Var "Num")) newEnv --> withValue (Fun (Var "Int") (Var "Num")) newEnv
    unify (Fun (Var "Int") (Var "Num")) (Fun (Var "Int") (Var "Int")) newEnv --> withError (TypeMismatch (Var "Num") (Var "Int")) newEnv
    unify (Fun (Var "Int") (Var "Num")) (Fun (Var "Num") (Var "Num")) newEnv --> withError (TypeMismatch (Var "Int") (Var "Num")) newEnv

    -- Generic function types are also possible.
    unify (For "a" (Fun (Var "a") (Var "a"))) (Fun (Var "Int") (Var "Int")) newEnv --> withValue (Fun (Var "Int") (Var "Int"))  { newEnv | names = Dict.insert "a" (Var "a", Var "@") newEnv.names, types = DisjointSet.union (Var "Int") (Var "a") newEnv.types }
    unify (For "a" (Fun (Var "a") (Var "a"))) (Fun (Var "Int") (Var "Num")) newEnv -- withValue (Fun (Var "Int") (Var "Int"))  { newEnv | names = Dict.insert "a" (Var "a", Var "@") newEnv.names, types = DisjointSet.union (Var "Int") (Var "a") newEnv.types }

-}
unify : Type -> Type -> Env a -> Env Type
unify type1 type2 env =
    -- TODO [outdated]: DisjointSet.find type1 and type2 before everything, maybe at (eval Var)
    -- TODO [outdated]: The DisjointSet should point to (Var "@") on free variables
    -- TODO: We don't need DisjointSet, we just keep types as names,
    --       and make sure to update all the types pointing to other types.
    --  Example:
    --      unify (∀a b. a -> b) (Int -> Int)
    --          let a : @ = a
    --          let b : @ = b
    --          unify (a : @) (Int : Type)
    --              bind a Int -- let a : Type = Int
    --          unify (b : @) (Int : Type)
    --              bind b Int -- let b : Type = Int
    --
    --  Example:
    --      unify (∀a. a -> a) (Int -> Num)
    --          let a : @ = a
    --          unify (a : @) (Int : Type)
    --              bind a Int -- let a : Type = Int
    --          unify (Int : Type) (Num : Type) -- TypeMismatch
    --
    --  Example:
    --      ∀a. a -> ∀a. a ==> ∀a b. a -> b
    --      unify (∀a. Int -> a) (∀a. a -> Num) -- rename a to b in rhs
    --      unify (∀a. Int -> a) (∀b. b -> Num)
    --          let a : @ = a
    --          let b : @ = b
    --          unify (b : @) (Int : Type)
    --              bind b Int -- let b : Type = Int
    --          unify (a : @) (Num : Type)
    --              bind a Num -- let a : Type = Num
    --
    --  Example: if a == b and b == Int then a == Int
    -- TODO:
    --  Builtin types: Any, Type, Int, Num
    --  * read : String -> Result Error Expr
    --  * write : Expr -> String
    --  * eval : Expr -> Env -> Result Error (Expr, Type)
    --  * unify : Type -> Type -> Env -> Result Error Type
    --  * define : String -> Expr -> Env -> Result Error Env
    --  * declare : String -> Type -> Env -> Result Error Env
    --  * createVar : Type -> Env -> String
    --  * createName : Env -> (String, Env)
    andThen2
        (\( typ1, knd1 ) ( typ2, knd2 ) env_ ->
            let
                bind : String -> Type -> Env Type
                bind x typ =
                    withValue typ
                        { env_
                            | types = DisjointSet.union typ (Var x) env_.types
                        }
            in
            case ( ( typ1, knd1 ), ( typ2, knd2 ) ) of
                ( ( Fun a1 b1, _ ), ( Fun a2 b2, _ ) ) ->
                    map2 Fun
                        (unify_ a1 a2 env_)
                        (unify_ b1 b2)

                ( ( For _ t1, _ ), ( t2, _ ) ) ->
                    -- define x (TE (Var x) (Var "@")) env_
                    unify_ t1 t2 env_

                ( ( t1, _ ), ( For _ t2, _ ) ) ->
                    -- define x (TE (Var x) (Var "@")) env_
                    unify_ t1 t2 env_

                ( ( Var x, Var "@" ), ( t2, _ ) ) ->
                    bind x t2

                ( ( t1, _ ), ( Var x, Var "@" ) ) ->
                    bind x t1

                ( ( t1, _ ), ( t2, _ ) ) ->
                    if t1 == t2 then
                        withValue t1 env_

                    else
                        withError (TypeMismatch t1 t2) env_
        )
        (eval type1 env)
        (eval type2)


unify_ : Type -> Type -> Env a -> Env Type
unify_ =
    -- Workaround for: https://github.com/elm/compiler/issues/2186
    unify


map : (a -> b) -> Env a -> Env b
map f env =
    andThen (\x -> withValue (f x)) env


map2 : (a -> b -> c) -> Env a -> (Env a -> Env b) -> Env c
map2 f env envAB =
    andThen2 (\x1 x2 -> withValue (f x1 x2)) env envAB


andThen : (a -> Env a -> Env b) -> Env a -> Env b
andThen f env =
    case env.result of
        Ok x ->
            f x env

        Err err ->
            withError err env


andThen2 :
    (a -> b -> Env b -> Env c)
    -> Env a
    -> (Env a -> Env b)
    -> Env c
andThen2 f env envAB =
    andThen (\x envA -> andThen (f x) (envAB envA)) env


andThen3 :
    (a -> b -> c -> Env c -> Env d)
    -> Env a
    -> (Env a -> Env b)
    -> (Env b -> Env c)
    -> Env d
andThen3 f env envAB envBC =
    andThen (\x envA -> andThen2 (f x) (envAB envA) envBC) env


withResult : Result Error b -> Env a -> Env b
withResult result env =
    { names = env.names
    , types = env.types
    , nameSeed = env.nameSeed
    , result = result
    }


withValue : b -> Env a -> Env b
withValue value env =
    withResult (Ok value) env


withError : Error -> Env a -> Env b
withError err env =
    withResult (Err err) env



-- Utility functions


tupleMap : (a -> b -> c) -> ( a, b ) -> c
tupleMap f ( x, y ) =
    f x y



-- Local helper functions


newTypeVar : Env a -> Env Type
newTypeVar env =
    Dict.keys env.names
        |> Set.fromList
        |> newVarName env.nameSeed
        |> tupleMap
            (\x seed ->
                { env | nameSeed = seed }
                    |> define x (TE (Var x) (Var "@"))
                    |> withValue (Var x)
            )


newVarName : Int -> Set String -> ( String, Int )
newVarName seed existing =
    let
        name =
            (case toBase (Char.toCode 'z' - Char.toCode 'a' + 2) seed of
                x :: xs ->
                    x - 1 :: xs

                [] ->
                    [ 0 ]
            )
                |> List.map (\x -> x + Char.toCode 'a')
                |> List.map Char.fromCode
                |> String.fromList
    in
    if Set.member name existing then
        newVarName (seed + 1) existing

    else
        ( name, seed + 1 )


newVarNames : Int -> Int -> Set String -> List String
newVarNames n firstSeed existing =
    List.foldl
        (\_ ( xs, ( seed, latestExisting ) ) ->
            (\( x, newSeed ) ->
                ( xs ++ [ x ], ( newSeed, Set.insert x latestExisting ) )
            )
                (newVarName seed latestExisting)
        )
        ( [], ( firstSeed, existing ) )
        (List.repeat n Nothing)
        |> Tuple.first


toBase : Int -> Int -> List Int
toBase base num =
    if num == 0 then
        []

    else
        ((num // base) |> toBase base) ++ [ num |> modBy base ]



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
            succeed func
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

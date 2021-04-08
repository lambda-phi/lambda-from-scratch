module Lambda exposing
    ( Env
    , Error(..)
    , Expr(..)
    , andThen
    , andThen2
    , andThen3
    , andThen4
    , asFunc
    , asFuncType
    , call
    , define
    , eval
    , func
    , funcType
    , letType
    , letVar
    , map
    , newEnv
    , newTypeVar
    , read
    , resolveType
    , unify
    , withError
    , withResult
    , withValue
    , write
    )

{-| A simple Lambda calculus implementation with type inference.

TODO: validate that this Elm issue works: <https://github.com/elm/compiler/issues/2186>

-}

import Dict exposing (Dict)
import DisjointSet exposing (DisjointSet)
import Parser exposing (Parser, andThen, drop, lazy, oneOf, parse, succeed, take, textOf)
import Parser.Char exposing (char, digit, letter)
import Parser.Common exposing (int, number, spaces, text)
import Parser.Expression exposing (fromLeft, fromRight, inbetween, term)
import Parser.Sequence exposing (concat, exactly, oneOrMore, zeroOrMore)
import Set exposing (Set)


{-| A Lambda calculus expression.

TODO: figure out how to have a "Formatter" type to write, like "Parser" to read
TODO: rename "Parser" to "Reader" and "Formatter" to "Writer" (?)

-}
type Expr
    = Int Int -- 42
    | Num Float -- 3.14
    | Var String -- x
    | Abs String Expr -- λx -> y
      -- Binary operators
    | App Expr Expr -- f x
    | Pow Expr Expr -- x ^ y
    | Mul Expr Expr -- x * y
    | Add Expr Expr -- x + y
    | TE Expr Type -- x : a
    | Or Expr Expr -- x | y
    | TAbs Type Type -- a -> b
      -- Error reporting
    | Pos String ( Int, Int ) ( Int, Int ) Expr -- filename start end expr


type alias Type =
    Expr


{-| An error from parsing or evaluation.
-}
type Error
    = SyntaxError Parser.Error
    | VariableNotFound Expr
    | NotAType Expr
    | TypeMismatch Type Type -- got, expected


type alias Env a =
    { names : Dict String ( Expr, Type )
    , equivalentTypes : DisjointSet Expr
    , typeNameSeed : Int
    , result : Result Error a
    }


newEnv : Env ()
newEnv =
    { names =
        Dict.fromList
            [ ( "Type", ( Var "Type", Var "a" ) )
            , ( "Int", ( Var "Type", Var "a" ) )
            , ( "Num", ( Var "Type", Var "a" ) )
            ]
    , equivalentTypes = DisjointSet.empty
    , typeNameSeed = 1
    , result = Ok ()
    }


{-| Reads a Lambda expression.

    import Lambda exposing (Expr(..), read)

    -- Values
    read "42"   --> Ok (Int 42)
    read "3.14" --> Ok (Num 3.14)

    -- Variables
    read "x" --> Ok (Var "x")

    -- Abstraction
    read "λx -> y"   --> Ok (Abs "x" (Var "y"))
    read "λx y -> z" --> Ok (Abs "x" (Abs "y" (Var "z")))

    -- Application
    read "f x" --> Ok (App (Var "f") (Var "x"))

    -- TODO: ^ * +

    -- Typed expression
    read "x : a" --> Ok (TE (Var "x") (Var "a"))

    -- TODO: |

    -- Type abstraction
    read "a -> b" --> Ok (TAbs (Var "a") (Var "b"))

    -- Variable definitions
    read "x := y; z"    --> Ok (letVar "x" (Var "y") (Var "z"))
    read "x : a = y; z" --> Ok (letVar "x" (TE (Var "y") (Var "a")) (Var "z"))

    -- Operator precedence
    read "x y z"      --> Ok (App (App (Var "x") (Var "y")) (Var "z"))
    read "x y ^ z"    --> Ok (Pow (App (Var "x") (Var "y")) (Var "z"))
    read "x y * z"    --> Ok (Mul (App (Var "x") (Var "y")) (Var "z"))
    read "x y + z"    --> Ok (Add (App (Var "x") (Var "y")) (Var "z"))
    read "x y : z"    --> Ok (TE (App (Var "x") (Var "y")) (Var "z"))
    read "x y -> z"   --> Ok (TAbs (App (Var "x") (Var "y")) (Var "z"))
    read "x λy -> z"  --> Ok (App (Var "x") (Abs "y" (Var "z")))

    read "x ^ y z"     --> Ok (Pow (Var "x") (App (Var "y") (Var "z")))
    read "x ^ y ^ z"   --> Ok (Pow (Var "x") (Pow (Var "y") (Var "z")))
    read "x ^ y * z"   --> Ok (Mul (Pow (Var "x") (Var "y")) (Var "z"))
    read "x ^ y + z"   --> Ok (Add (Pow (Var "x") (Var "y")) (Var "z"))
    read "x ^ y : z"   --> Ok (TE (Pow (Var "x") (Var "y")) (Var "z"))
    read "x ^ y -> z"  --> Ok (TAbs (Pow (Var "x") (Var "y")) (Var "z"))
    read "x ^ λy -> z" --> Ok (Pow (Var "x") (Abs "y" (Var "z")))

    read "x * y z"     --> Ok (Mul (Var "x") (App (Var "y") (Var "z")))
    read "x * y ^ z"   --> Ok (Mul (Var "x") (Pow (Var "y") (Var "z")))
    read "x * y * z"   --> Ok (Mul (Mul (Var "x") (Var "y")) (Var "z"))
    read "x * y + z"   --> Ok (Add (Mul (Var "x") (Var "y")) (Var "z"))
    read "x * y : z"   --> Ok (TE (Mul (Var "x") (Var "y")) (Var "z"))
    read "x * y -> z"  --> Ok (TAbs (Mul (Var "x") (Var "y")) (Var "z"))
    read "x * λy -> z" --> Ok (Mul (Var "x") (Abs "y" (Var "z")))

    read "x + y z"     --> Ok (Add (Var "x") (App (Var "y") (Var "z")))
    read "x + y ^ z"   --> Ok (Add (Var "x") (Pow (Var "y") (Var "z")))
    read "x + y * z"   --> Ok (Add (Var "x") (Mul (Var "y") (Var "z")))
    read "x + y + z"   --> Ok (Add (Add (Var "x") (Var "y")) (Var "z"))
    read "x + y : z"   --> Ok (TE (Add (Var "x") (Var "y")) (Var "z"))
    read "x + y -> z"  --> Ok (TAbs (Add (Var "x") (Var "y")) (Var "z"))
    read "x + λy -> z" --> Ok (Add (Var "x") (Abs "y" (Var "z")))

    read "x : y z"     --> Ok (TE (Var "x") (App (Var "y") (Var "z")))
    read "x : y ^ z"   --> Ok (TE (Var "x") (Pow (Var "y") (Var "z")))
    read "x : y * z"   --> Ok (TE (Var "x") (Mul (Var "y") (Var "z")))
    read "x : y + z"   --> Ok (TE (Var "x") (Add (Var "y") (Var "z")))
    read "x : y : z"   --> Ok (TE (TE (Var "x") (Var "y")) (Var "z"))
    read "x : y -> z"  --> Ok (TAbs (TE (Var "x") (Var "y")) (Var "z"))
    read "x : λy -> z" --> Ok (TE (Var "x") (Abs "y" (Var "z")))

    read "x -> y z"     --> Ok (TAbs (Var "x") (App (Var "y") (Var "z")))
    read "x -> y ^ z"   --> Ok (TAbs (Var "x") (Pow (Var "y") (Var "z")))
    read "x -> y * z"   --> Ok (TAbs (Var "x") (Mul (Var "y") (Var "z")))
    read "x -> y + z"   --> Ok (TAbs (Var "x") (Add (Var "y") (Var "z")))
    read "x -> y : z"   --> Ok (TAbs (Var "x") (TE (Var "y") (Var "z")))
    read "x -> y -> z"  --> Ok (TAbs (Var "x") (TAbs (Var "y") (Var "z")))
    read "x -> λy -> z" --> Ok (TAbs (Var "x") (Abs "y" (Var "z")))

    read "λx -> y z"     --> Ok (Abs "x" (App (Var "y") (Var "z")))
    read "λx -> y ^ z"   --> Ok (Abs "x" (Pow (Var "y") (Var "z")))
    read "λx -> y * z"   --> Ok (Abs "x" (Mul (Var "y") (Var "z")))
    read "λx -> y + z"   --> Ok (Abs "x" (Add (Var "y") (Var "z")))
    read "λx -> y : z"   --> Ok (Abs "x" (TE (Var "y") (Var "z")))
    read "λx -> y -> z"  --> Ok (Abs "x" (TAbs (Var "y") (Var "z")))
    read "λx -> λy -> z" --> Ok (Abs "x" (Abs "y" (Var "z")))

    read "x + (y + z)" --> Ok (Add (Var "x") (Add (Var "y") (Var "z")))
    read "(x ^ y) ^ z" --> Ok (Pow (Pow (Var "x") (Var "y")) (Var "z"))

    -- Type definitions
    read "@Bool = True | False; x"
    -- Ok  ( letType "Bool" []
    --         [ ( "True", Var "Bool" )
    --         , ( "False", Var "Bool" )
    --         ]
    --         (Var "x")
    --     )

-}
read : String -> Result Error Expr
read txt =
    parse txt exprP
        |> Result.mapError SyntaxError


{-| Writes a Lambda expression.

    import Lambda exposing (Expr(..), write)

    -- Values
    write (Int 42)   --> "42"
    write (Num 3.14) --> "3.14"

    -- Variables
    write (Var "x") --> "x"

    -- Abstraction
    write (Abs "x" (Var "y")) --> "λx -> y"

    -- Application
    write (App (Var "f") (Var "x")) --> "f x"

    -- TODO: ^ * +

    -- Typed expression
    write (TE (Var "x") (Var "a")) --> "x : a"

    -- TODO: |

    -- Type abstraction
    write (TAbs (Var "a") (Var "b")) --> "a -> b"

    -- Variable definitions
    write (App (Abs "x" (Var "z")) (Var "y"))                --> "x := y; z"
    write (App (Abs "x" (Var "z")) (TE (Var "y") (Var "a"))) --> "x : a = y; z"

    -- Operator precedence
    write (App (App (Var "x") (Var "y")) (Var "z"))  --> "x y z"
    write (Pow (App (Var "x") (Var "y")) (Var "z"))  --> "x y ^ z"
    write (Mul (App (Var "x") (Var "y")) (Var "z"))  --> "x y * z"
    write (Add (App (Var "x") (Var "y")) (Var "z"))  --> "x y + z"
    write (TE (App (Var "x") (Var "y")) (Var "z"))   --> "x y : z"
    write (TAbs (App (Var "x") (Var "y")) (Var "z")) --> "x y -> z"
    write (App (Var "x") (Abs "y" (Var "z")))        --> "x (λy -> z)"

    write (Pow (Var "x") (App (Var "y") (Var "z")))  --> "x ^ y z"
    write (Pow (Var "x") (Pow (Var "y") (Var "z")))  --> "x ^ y ^ z"
    write (Mul (Pow (Var "x") (Var "y")) (Var "z"))  --> "x ^ y * z"
    write (Add (Pow (Var "x") (Var "y")) (Var "z"))  --> "x ^ y + z"
    write (TE (Pow (Var "x") (Var "y")) (Var "z"))   --> "x ^ y : z"
    write (TAbs (Pow (Var "x") (Var "y")) (Var "z")) --> "x ^ y -> z"
    write (Pow (Var "x") (Abs "y" (Var "z")))        --> "x ^ (λy -> z)"

    write (Mul (Var "x") (App (Var "y") (Var "z")))  --> "x * y z"
    write (Mul (Var "x") (Pow (Var "y") (Var "z")))  --> "x * y ^ z"
    write (Mul (Mul (Var "x") (Var "y")) (Var "z"))  --> "x * y * z"
    write (Add (Mul (Var "x") (Var "y")) (Var "z"))  --> "x * y + z"
    write (TE (Mul (Var "x") (Var "y")) (Var "z"))   --> "x * y : z"
    write (TAbs (Mul (Var "x") (Var "y")) (Var "z")) --> "x * y -> z"
    write (Mul (Var "x") (Abs "y" (Var "z")))        --> "x * (λy -> z)"

    write (Add (Var "x") (App (Var "y") (Var "z")))  --> "x + y z"
    write (Add (Var "x") (Pow (Var "y") (Var "z")))  --> "x + y ^ z"
    write (Add (Var "x") (Mul (Var "y") (Var "z")))  --> "x + y * z"
    write (Add (Add (Var "x") (Var "y")) (Var "z"))  --> "x + y + z"
    write (TE (Add (Var "x") (Var "y")) (Var "z"))   --> "x + y : z"
    write (TAbs (Add (Var "x") (Var "y")) (Var "z")) --> "x + y -> z"
    write (Add (Var "x") (Abs "y" (Var "z")))        --> "x + (λy -> z)"

    write (TE (Var "x") (App (Var "y") (Var "z")))  --> "x : y z"
    write (TE (Var "x") (Pow (Var "y") (Var "z")))  --> "x : y ^ z"
    write (TE (Var "x") (Mul (Var "y") (Var "z")))  --> "x : y * z"
    write (TE (Var "x") (Add (Var "y") (Var "z")))  --> "x : y + z"
    write (TE (TE (Var "x") (Var "y")) (Var "z"))   --> "x : y : z"
    write (TAbs (TE (Var "x") (Var "y")) (Var "z")) --> "x : y -> z"
    write (TE (Var "x") (Abs "y" (Var "z")))        --> "x : (λy -> z)"

    write (TAbs (Var "x") (App (Var "y") (Var "z")))  --> "x -> y z"
    write (TAbs (Var "x") (Pow (Var "y") (Var "z")))  --> "x -> y ^ z"
    write (TAbs (Var "x") (Mul (Var "y") (Var "z")))  --> "x -> y * z"
    write (TAbs (Var "x") (Add (Var "y") (Var "z")))  --> "x -> y + z"
    write (TAbs (Var "x") (TE (Var "y") (Var "z")))   --> "x -> y : z"
    write (TAbs (Var "x") (TAbs (Var "y") (Var "z"))) --> "x -> y -> z"
    write (TAbs (Var "x") (Abs "y" (Var "z")))        --> "x -> (λy -> z)"

    write (Abs "x" (App (Var "y") (Var "z")))  --> "λx -> y z"
    write (Abs "x" (Pow (Var "y") (Var "z")))  --> "λx -> y ^ z"
    write (Abs "x" (Mul (Var "y") (Var "z")))  --> "λx -> y * z"
    write (Abs "x" (Add (Var "y") (Var "z")))  --> "λx -> y + z"
    write (Abs "x" (TE (Var "y") (Var "z")))   --> "λx -> y : z"
    write (Abs "x" (TAbs (Var "y") (Var "z"))) --> "λx -> y -> z"
    write (Abs "x" (Abs "y" (Var "z")))        --> "λx y -> z"

    write (Add (Var "x") (Add (Var "y") (Var "z"))) --> "x + (y + z)"
    write (Pow (Pow (Var "x") (Var "y")) (Var "z")) --> "(x ^ y) ^ z"

-}
write : Expr -> String
write expr =
    let
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

        binopFromLeft : Expr -> String -> Expr -> String
        binopFromLeft e1 op e2 =
            unop1 e1 ++ op ++ unop2 e2

        binopFromRight : Expr -> String -> Expr -> String
        binopFromRight e1 op e2 =
            unop2 e1 ++ op ++ unop1 e2
    in
    case expr of
        Int k ->
            String.fromInt k

        Num k ->
            String.fromFloat k

        Var x ->
            x

        Abs _ _ ->
            tupleMap (\xs y -> "λ" ++ String.join " " xs ++ " -> " ++ unop1 y)
                (asFunc expr)

        App (Abs x e) (TE v t) ->
            x ++ " : " ++ write t ++ " = " ++ write v ++ "; " ++ write e

        App (Abs x e) v ->
            x ++ " := " ++ write v ++ "; " ++ write e

        App e1 e2 ->
            binopFromLeft e1 " " e2

        Pow e1 e2 ->
            binopFromRight e1 " ^ " e2

        Mul e1 e2 ->
            binopFromLeft e1 " * " e2

        Add e1 e2 ->
            binopFromLeft e1 " + " e2

        TE e t ->
            binopFromLeft e " : " t

        Or e1 e2 ->
            binopFromLeft e1 " | " e2

        TAbs t1 t2 ->
            binopFromRight t1 " -> " t2

        Pos _ _ _ e ->
            write e



-- Syntax sugar


{-|

    import Lambda

    Ok (func [] (Int 42))                --> read "42"
    Ok (func [ "x" ] (Int 42))           --> read "λx -> 42"
    Ok (func [ "x", "y", "z" ] (Int 42)) --> read "λx y z -> 42"

-}
func : List String -> Expr -> Expr
func inputs output =
    List.foldr Abs output inputs


{-|

    import Lambda

    read "42" |> Result.map asFunc           --> Ok ( [], Int 42 )
    read "λx -> 42" |> Result.map asFunc     --> Ok ( [ "x" ], Int 42 )
    read "λx y z -> 42" |> Result.map asFunc --> Ok ( [ "x", "y", "z" ], Int 42 )

-}
asFunc : Expr -> ( List String, Expr )
asFunc expr =
    let
        asFunc_ : List String -> Expr -> ( List String, Expr )
        asFunc_ xs y0 =
            case y0 of
                Abs x y ->
                    asFunc_ (xs ++ [ x ]) y

                _ ->
                    ( xs, y0 )
    in
    asFunc_ [] expr


{-|

    import Lambda

    Ok (funcType [] (Var "a"))                            --> read "a"
    Ok (funcType [ Var "a" ] (Var "b"))                   --> read "a -> b"
    Ok (funcType [ Var "a", Var "b", Var "c" ] (Var "d")) --> read "a -> b -> c -> d"

-}
funcType : List Type -> Type -> Type
funcType inputs output =
    List.foldr TAbs output inputs


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
                TAbs x y ->
                    asFuncType_ (xs ++ [ x ]) y

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

    Ok (letVar "x" (Var "y") (Var "z"))                --> read "(λx -> z) y"
    Ok (letVar "x" (Var "y") (Var "z"))                --> read "x := y; z"
    Ok (letVar "x" (TE (Var "y") (Var "a")) (Var "z")) --> read "x : a = y; z"

-}
letVar : String -> Expr -> Expr -> Expr
letVar name value expr =
    App (Abs name expr) value


{-| Defines a new tagged union type.

    -- @T = A; x
    Ok  (letType "T" []
            ( "A", Var "T" )
            []
            (Var "x")
        )
    --> [ "T : A = Type"
    --> , "A : T = λa -> a"
    --> , "x"
    --> ] |> String.join ";" |> read

    -- @T a b = A; x
    Ok  (letType "T" [ "a", "b" ]
            ( "A", App (App (Var "T") (Var "a")) (Var "b") )
            []
            (Var "x")
        )
    --> [ "T : A = λa b -> Type"
    --> , "A : T a b = λc -> c"
    --> , "x"
    --> ] |> String.join ";" |> read

    -- @T a = A a a; x
    Ok  (letType "T" [ "a" ]
            ( "A", TAbs (Var "a") (TAbs (Var "a") (App (Var "T") (Var "a"))) )
            []
            (Var "x")
        )
    --> [ "T : A = λa -> Type"
    --> , "A : a -> a -> T a = λc d -> λb -> b c d"
    --> , "x"
    --> ] |> String.join ";" |> read

    -- @T = A | B | C; x
    Ok  (letType "T" []
            ( "A", Var "T" )
            [ ( "B", Var "T" )
            , ( "C", Var "T" )
            ]
            (Var "x")
        )
    --> [ "T : A | B | C = Type"
    --> , "A : T = λa b c -> a"
    --> , "B : T = λa b c -> b"
    --> , "C : T = λa b c -> c"
    --> , "x"
    --> ] |> String.join ";" |> read

-}
letType : String -> List String -> ( String, Type ) -> List ( String, Type ) -> Expr -> Expr
letType name args ctorHead ctorsTail expr =
    let
        -- Example:
        --  type Maybe a = Just a | Nothing
        --      Maybe : Just | Nothing = λa -> Type
        --      Just : a -> Maybe a = λd -> λb c -> b c
        --      Nothing : Maybe a = λb c -> c
        ctors : List ( String, Type )
        ctors =
            -- [ ( "Just", Abs "a" (App (Var "Maybe") (Var "a")) ) -- Just : a -> Maybe a
            -- , ( "Nothing", App (Var "Maybe") (Var "a") )        -- Nothing : Maybe a
            -- ]
            ctorHead :: ctorsTail

        ctorInputTypes : List (List Type)
        ctorInputTypes =
            -- [ [ Var "a" ] -- Just [ Var "a" ]
            -- , []          -- Nothing []
            -- ]
            List.map Tuple.second ctors
                |> List.map asFuncType
                |> List.map Tuple.first

        typ : Type
        typ =
            -- Just | Nothing
            List.foldl (\y x -> Or x y)
                (Var (Tuple.first ctorHead))
                (List.map Var (List.map Tuple.first ctorsTail))

        choices : List String
        choices =
            -- [ "b", "c" ]
            newVarNames (List.length ctors) 1 (Set.fromList args)
    in
    letVar name
        (TE (func args (Var "Type")) typ)
        (List.foldr
            (\( ( ctorName, ctorType ), ( choice, inputTypes ) ) ->
                let
                    ctorInputs =
                        newVarNames
                            (List.length inputTypes)
                            (List.length choices + 1)
                            (Set.fromList (args ++ choices))

                    ctorDef =
                        func (ctorInputs ++ choices)
                            (call (Var choice) (List.map Var ctorInputs))
                in
                letVar ctorName (TE ctorDef ctorType)
            )
            expr
            (zip ctors (zip choices ctorInputTypes))
        )


{-| Define a new variable in an `Env`.

    import Dict

    -- We evaluate the expression to get its type.
    define "x" (Int 42) newEnv
    --> { newEnv | names = Dict.insert "x" (Int 42, Var "Int") newEnv.names }

    define "x" (Var "y") newEnv
    --> newEnv |> withError (VariableNotFound (Var "y"))

    -- Note that we already know the type of a typed expression,
    -- so we can defer its actual evaluation until it's used.
    -- This allows to define recursive functions and other expressions
    -- that depend on variables that haven't been defined yet.
    define "x" (TE (Var "y") (Var "a")) newEnv
    --> { newEnv | names = Dict.insert "x" (Var "y", Var "a") newEnv.names }

-}
define : String -> Expr -> Env a -> Env a
define name value env =
    case value of
        TE e t ->
            { env | names = Dict.insert name ( e, t ) env.names }

        _ ->
            map (tupleMap TE) (eval value env)
                |> andThen (define_ name)
                |> andThen (\_ -> withResult env.result)


define_ : String -> Expr -> Env a -> Env a
define_ =
    -- Workaround for: https://github.com/elm/compiler/issues/2186
    define


{-| Define a new type in an `Env`.
-}
defineType : String -> Env a -> Env a
defineType name env =
    Debug.todo "defineType"


{-| Creates a new type variable with a unique name.

    import Dict
    import DisjointSet exposing (add, empty)

    -- Starts with "a".
    newTypeVar newEnv |> .result --> Ok (Var "a")

    -- Goes alphabetically if a type is already defined.
    newTypeVar { newEnv | equivalentTypes = add [ Var "a" ] empty } |> .result --> Ok (Var "b")

    -- Type definitions are also checked.
    newTypeVar { newEnv | names = Dict.singleton "a" (Var "Type", Var "Type") } |> .result --> Ok (Var "b")

    -- Names can have multiple letters when a-z are all used up.
    newTypeVar { newEnv | typeNameSeed = 1234 } |> .result --> Ok (Var "ast")

-}
newTypeVar : Env a -> Env Type
newTypeVar env =
    let
        maybeTypeName typ =
            case typ of
                Var t ->
                    Just t

                _ ->
                    Nothing

        existingNames =
            List.filterMap maybeTypeName (DisjointSet.items env.equivalentTypes)
                ++ Dict.keys env.names
                |> Set.fromList
    in
    newVarName env.typeNameSeed existingNames
        |> tupleMap
            (\t newTypeNameSeed ->
                withValue (Var t)
                    { env
                        | typeNameSeed = newTypeNameSeed
                        , equivalentTypes = DisjointSet.add [ Var t ] env.equivalentTypes
                    }
            )


{-| Evaluates an expression.

    import Lambda

    evalFrom : String -> Result Error (String, String)
    evalFrom txt =
        newEnv
            |> define "x" (Int 42)
            |> define "y" (TE (Var "y") (Var "Num"))
            |> define "f" (TE (Var "f") (TAbs (Var "Int") (Var "Num")))
            |> define "g" (TE (Var "g") (TAbs (Var "a") (Var "a")))
            |> withResult (read txt)
            |> andThen eval
            |> .result
            |> Result.map (Tuple.mapBoth write write)

    -- Values
    evalFrom "42"   --> Ok ( "42", "Int" )
    evalFrom "3.14" --> Ok ( "3.14", "Num" )

    -- Variables
    evalFrom "x" --> Ok ( "42", "Int" )
    evalFrom "y" --> Ok ( "y", "Num" )
    evalFrom "z" --> Err (VariableNotFound (Var "z"))
    evalFrom "f" --> Ok ( "f", "Int -> Num" )
    evalFrom "g" --> Ok ( "g", "a -> a" )
    evalFrom "h" --> Err (VariableNotFound (Var "h"))

    -- Abstraction
    evalFrom "λx -> x" --> Ok ( "λx -> x", "a -> a" )
    evalFrom "λx -> y" --> Ok ( "λx -> y", "a -> Num" )
    evalFrom "λy -> x" --> Ok ( "λy -> 42", "a -> Int" )
    evalFrom "λx -> z" --> Err (VariableNotFound (Var "z"))

    -- Application
    evalFrom "f x" --> Ok ( "f 42", "Num" )
    evalFrom "f y" --> Err (TypeMismatch (Var "Num") (Var "Int"))
    evalFrom "g x" --> Ok ( "g 42", "Int" )
    evalFrom "g y" --> Ok ( "g y", "Num" )
    evalFrom "h z" --> Err (VariableNotFound (Var "h"))
    evalFrom "f z" --> Err (VariableNotFound (Var "z"))
    evalFrom "(λx -> x) 42"    --> Ok ( "42", "Int" )
    evalFrom "(λx -> 3.14) 42" --> Ok ( "3.14", "Num" )

    -- TODO: ^ * +

    -- Typed expression
    evalFrom "(x : a)"       --> Ok ( "x", "a" )
    evalFrom "(z : a)"       --> Ok ( "z", "a" ) -- defer evaluation
    evalFrom "λx -> 42"      --> Ok ( "λx -> 42", "a -> Int" )
    evalFrom "λx -> (y : a)" --> Ok ( "λx -> y", "b -> a" )
    evalFrom "(x : 42)"      --> Err (NotAType (Int 42))

    -- TODO: |

    -- Type abstraction
    evalFrom "a -> b" -- Ok ( "a -> b", "Type" )

    -- Variable definitions
    evalFrom "x := 42; x"    -- Ok ( "42", "Int" )
    evalFrom "x : a = y; x"  -- Ok ( "(y : a)", "a" )
    evalFrom "x : 42 = y; x" -- Err (NotAType (Int 42))

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
                        eval value env

                Nothing ->
                    withError (VariableNotFound (Var x)) env

        Abs x (TE y yType) ->
            andThen2
                (\yT xT env_ ->
                    define x (TE (Var x) xT) env_
                        |> withValue ( Abs x y, TAbs xT yT )
                )
                (resolveType yType env)
                newTypeVar

        Abs x y ->
            andThen
                (\xT env_ ->
                    define x (TE (Var x) xT) env_
                        |> eval_ y
                        |> map (Tuple.mapBoth (Abs x) (TAbs xT))
                )
                (newTypeVar env)

        App e1 e2 ->
            andThen3
                (\outT ( v1, t1 ) ( v2, t2 ) env_ ->
                    unify (TAbs t2 outT) t1 env_
                        |> map (\_ -> ( v1, v2, outT ))
                )
                (newTypeVar env)
                (eval_ e1)
                (eval_ e2)
                |> andThen
                    (\( v1, v2, outT ) env_ ->
                        case v1 of
                            Abs x y ->
                                withValue v2 env_
                                    |> andThen (define x)
                                    |> eval_ y

                            _ ->
                                withValue
                                    ( App v1 v2
                                    , DisjointSet.find outT env_.equivalentTypes
                                        |> Maybe.withDefault outT
                                    )
                                    env_
                    )

        Pow e1 e2 ->
            Debug.todo "eval Pow"

        Mul e1 e2 ->
            Debug.todo "eval Mul"

        Add e1 e2 ->
            Debug.todo "eval Add"

        TE e t ->
            map (Tuple.pair e) (resolveType t env)

        Or e1 e2 ->
            Debug.todo "eval Or"

        TAbs t1 t2 ->
            Debug.todo "eval TAbs"

        Pos source start end e ->
            Debug.todo "eval Pos"


eval_ : Expr -> Env a -> Env ( Expr, Type )
eval_ =
    -- Workaround for: https://github.com/elm/compiler/issues/2186
    eval


{-| Resolves and validates a type expression.

    import Dict
    import DisjointSet exposing (empty, union)

    envWithA : Env ()
    envWithA =
        { newEnv
            | names = Dict.fromList
                [ ( "x", ( Int 42, Var "Int" ) )
                , ( "Type", ( Var "Type", Var "a" ) )
                , ( "Int", ( Var "Type", Var "a" ) )
                , ( "T", ( Abs "a" (Var "Type"), TAbs (Var "a") (Var "b") ) )
                ]
            ,equivalentTypes = union (Var "a") (Var "b") empty
        }

    -- Defined names must be types.
    resolveType (Var "x") envWithA |> .result     --> Err (NotAType (Var "x"))
    resolveType (Var "Int") envWithA |> .result   --> Ok (Var "Int")
    resolveType (Var "T") envWithA |> .result     --> Err (NotAType (Var "T"))
    resolveType (App (Var "T") (Var "Int")) envWithA |> .result --> Ok (App (Var "T") (Var "Int"))

    -- Undefined names are type variables.
    resolveType (Var "a") newEnv |> .result   --> Ok (Var "a")
    resolveType (Var "a") envWithA |> .result --> Ok (Var "a")
    resolveType (Var "b") envWithA |> .result --> Ok (Var "a")

    -- Type abstractions are also allowed.
    resolveType (TAbs (Var "b") (Var "Int")) envWithA |> .result
    --> Ok (TAbs (Var "a") (Var "Int"))

    -- Other expressions are not types.
    resolveType (Int 42) newEnv |> .result --> Err (NotAType (Int 42))

-}
resolveType : Type -> Env a -> Env Type
resolveType typ env =
    case typ of
        Var x ->
            case ( DisjointSet.find (Var x) env.equivalentTypes, Dict.get x env.names ) of
                ( Just t, _ ) ->
                    withValue t env

                ( _, Just ( Var "Type", _ ) ) ->
                    withValue (Var x) env

                ( Nothing, Nothing ) ->
                    withValue (Var x)
                        { env | equivalentTypes = DisjointSet.add [ Var x ] env.equivalentTypes }

                _ ->
                    withError (NotAType typ) env

        TAbs type1 type2 ->
            map2 TAbs
                (resolveType_ type1 env)
                (resolveType_ type2)

        App _ _ ->
            eval typ env
                |> map Tuple.first
                |> andThen resolveType_
                |> withValue typ

        _ ->
            withError (NotAType typ) env


resolveType_ : Type -> Env a -> Env Type
resolveType_ =
    -- Workaround for: https://github.com/elm/compiler/issues/2186
    resolveType


{-| Tries to unify two types or get a `TypeMismatch` error.

    import DisjointSet exposing (add, empty, union)

    int : Expr
    int = Var "Int"

    num : Expr
    num = Var "Num"

    a : Expr
    a = Var "a"

    b : Expr
    b = Var "b"

    -- Bound types
    unify int int newEnv --> newEnv |> withValue int
    unify int num newEnv --> newEnv |> withError (TypeMismatch int num)

    -- Free types
    unify a a newEnv --> withValue a { newEnv | equivalentTypes = empty }
    unify a b newEnv --> withValue a { newEnv | equivalentTypes = union a b empty }

    -- Bound types with free types
    unify int a newEnv --> withValue int { newEnv | equivalentTypes = union int a empty }
    unify a int newEnv --> withValue int { newEnv | equivalentTypes = union int a empty }

    -- Abstraction types
    unify (TAbs int num) (TAbs int num) newEnv --> withValue (TAbs int num) newEnv
    unify (TAbs int num) a newEnv --> withValue (TAbs int num) { newEnv | equivalentTypes = union (TAbs int num) a empty }
    unify a (TAbs int num) newEnv --> withValue (TAbs int num) { newEnv | equivalentTypes = union (TAbs int num) a empty }
    unify (TAbs int num) (TAbs a b) newEnv --> withValue (TAbs int num) { newEnv | equivalentTypes = add [ a, b ] empty |> union int a |> union num b }
    unify (TAbs a b) (TAbs int num) newEnv --> withValue (TAbs int num) { newEnv | equivalentTypes = add [ a, b ] empty |> union int a |> union num b }

    -- Not a type
    unify (Int 1) (Int 2) newEnv --> withError (NotAType (Int 1)) newEnv
    unify a (Int 2) newEnv --> withError (NotAType (Int 2)) { newEnv | equivalentTypes = add [ a ] empty }

-}
unify : Type -> Type -> Env a -> Env Type
unify type1 type2 env =
    let
        isFreeType : String -> Bool
        isFreeType typeName =
            not (Dict.member typeName env.names)

        bind : String -> Expr -> Env a -> Env Type
        bind x typ env_ =
            { env_ | equivalentTypes = DisjointSet.union typ (Var x) env_.equivalentTypes }
                |> withValue typ
    in
    andThen2
        (\t1 t2 env_ ->
            case ( t1, t2 ) of
                ( TAbs a1 b1, TAbs a2 b2 ) ->
                    andThen2
                        (\ab1 ab2 -> withValue (TAbs ab1 ab2))
                        (unify_ a1 a2 env_)
                        (unify_ b1 b2)

                ( Var x, Var y ) ->
                    if x == y then
                        -- (t, t) ->
                        withValue t1 env

                    else if isFreeType y then
                        -- (_, Var y) if isFreeType y ->
                        bind y t1 env

                    else if isFreeType x then
                        -- (Var _, _) ->
                        unify t2 t1 env

                    else
                        withError (TypeMismatch t1 t2) env

                ( _, Var y ) ->
                    if isFreeType y then
                        bind y t1 env

                    else
                        withError (TypeMismatch t1 t2) env

                ( Var _, _ ) ->
                    unify t2 t1 env

                _ ->
                    if t1 == t2 then
                        withValue t1 env

                    else
                        -- _ ->
                        withError (TypeMismatch t1 t2) env
        )
        (resolveType_ type1 env)
        (resolveType_ type2)


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


andThen4 :
    (a -> b -> c -> d -> Env d -> Env e)
    -> Env a
    -> (Env a -> Env b)
    -> (Env b -> Env c)
    -> (Env c -> Env d)
    -> Env e
andThen4 f env envAB envBC envCD =
    andThen (\x envA -> andThen3 (f x) (envAB envA) envBC envCD) env


withResult : Result Error b -> Env a -> Env b
withResult result env =
    { names = env.names
    , equivalentTypes = env.equivalentTypes
    , typeNameSeed = env.typeNameSeed
    , result = result
    }


withValue : b -> Env a -> Env b
withValue value env =
    withResult (Ok value) env


withError : Error -> Env a -> Env b
withError err env =
    withResult (Err err) env



-- Utility functions


zip : List a -> List b -> List ( a, b )
zip list1 list2 =
    case ( list1, list2 ) of
        ( x :: xs, y :: ys ) ->
            ( x, y ) :: zip xs ys

        _ ->
            []


tupleMap : (a -> b -> c) -> ( a, b ) -> c
tupleMap f ( x, y ) =
    f x y



-- Local helper functions


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
    concat
        [ exactly 1 letter
        , zeroOrMore (oneOf [ letter, digit, char '_' ])
        ]
        |> textOf


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
                |> drop (text "->")
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

        -- letTypeP : Parser Expr
        -- letTypeP =
        --     succeed letType
        --         |> drop (char '@')
        --         |> drop spaces
        --         |> take identifierP
        --         |> drop spaces
        --         |> drop (char ':')
        --         |> drop spaces
        --         |> take (lazy (\_ -> exprP))
        --         |> drop spaces
        --         |> drop (char '=')
        --         |> drop spaces
        --         |> take
        --             (oneOrMore
        --                 (succeed Tuple.pair
        --                     |> take identifierP
        --                     |> drop spaces
        --                     |> drop (char ':')
        --                     |> drop spaces
        --                     |> take (lazy (\_ -> exprP))
        --                     |> drop spaces
        --                 )
        --             )
    in
    Parser.Expression.expression
        [ [ fromLeft App spaces ]
        , [ fromRight Pow (char '^') ]
        , [ fromLeft Mul (char '*') ]
        , [ fromLeft Add (char '+') ]
        , [ fromLeft TE (char ':') ]
        , [ fromLeft Or (char '|') ]
        , [ fromRight TAbs (text "->") ]
        , [ inbetween identity (char '(') (char ')')
          , term identity funcP
          , term identity letVarP

          --   , term identity letTypeP
          , term Int int
          , term Num number
          , term Var identifierP
          ]
        ]


precedence : Expr -> Int
precedence expr =
    -- TODO: find a way to have only one definition for operator precedence
    --       both for readers/parsers and writers/formatters.
    case expr of
        Int _ ->
            -- TODO: adjust these 100s
            100

        Num _ ->
            100

        Var _ ->
            100

        App _ _ ->
            7

        Pow _ _ ->
            6

        Mul _ _ ->
            5

        Add _ _ ->
            4

        TE _ _ ->
            3

        Or _ _ ->
            2

        TAbs _ _ ->
            1

        Abs _ _ ->
            0

        Pos _ _ _ e ->
            precedence e

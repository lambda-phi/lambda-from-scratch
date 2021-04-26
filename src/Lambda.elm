module Lambda exposing
    ( Env
    , Error(..)
    , Expr(..)
    , Type
    , add
    , andThen
    , andThen2
    , andThen3
    , andThen4
    , asCall
    , asFunc
    , asFuncType
    , call
    , define
    , defineType
    , eval
    , evalType
    , exp
    , func
    , funcType
    , letVar
    , map
    , mul
    , newEnv
    , newTypeVar
    , read
    , unify
    , withError
    , withResult
    , withValue
    , write
    )

{-| A simple Lambda calculus implementation with type inference.

TODO: validate that this Elm issue works: <https://github.com/elm/compiler/issues/2186>

Practical Implementation of a Dependently Typed Functional Programming Language:
<https://eb.host.cs.st-andrews.ac.uk/writings/thesis.pdf>


## Type Theory expressions

    type t
        = *i               -- type universes
        | x                -- variable
        | c                -- constructor
        | ∀x:t. t          -- function space
        | λx:t. t          -- abstraction
        | t t              -- application
        | let x:t = t in t -- let binding


## Contractions

β−contraction: substitutes a value applied to a λ-binding for the bound variable
in the scope of that binding.
Γ ⊢ (λx:S. t) s ⇒ let x:S = s in t

η-contraction: eliminates redundant λ abstractions
Γ ⊢ λx:S. f x ⇒ f

δ-contraction: replaces a let bound variable by its value
Γ{x:S = s} ⊢ x ⇒ s


## Type checking

Notation (modified): <https://en.wikipedia.org/wiki/Type_rule>

    Type
        if   Γ ⊢ valid
        then Γ ⊢ *n : *n+1

    Var
        if   Γ{x:S} ⊢ valid
        then Γ{x:S} ⊢ x : S

    Val
        if   Γ{x:S = s} ⊢ valid
        then Γ{x:S = s} ⊢ x : S

    App
        if   Γ ⊢ f : (∀x:S. T)
             Γ ⊢ s : S
        then Γ ⊢ f s : (let x:S = s in T)

    Lam
        if   Γ{x:S} ⊢ e : T
             Γ ⊢ (∀x:S. T) : *n
        then Γ ⊢ (λx:S. e) : (∀x:S. T)

    ForAll
        if   Γ{x:S} ⊢ T : *n
             Γ ⊢ S : *n
        then Γ ⊢ ∀x:S. T : *n

    Let
        if   Γ ⊢ e1 : S
             Γ{x:S = e1} ⊢ e2 : T
             Γ ⊢ S : *n
             Γ{x:S = e1} ⊢ T : *n
        then Γ ⊢ let x:S = e1 in e2 : (let x:S = e1 in T)

-}

import Dict exposing (Dict)
import DisjointSet exposing (DisjointSet)
import Parser exposing (Parser, andThen, drop, lazy, oneOf, parse, succeed, take)
import Parser.Char exposing (char, digit, letter)
import Parser.Common exposing (int, number, spaces, text)
import Parser.Expression exposing (fromLeft, fromRight, inbetween, term)
import Parser.Sequence exposing (concat, exactly, oneOrMore, zeroOrMore)
import Set exposing (Set)


{-| A Lambda calculus Expression.

TODO: figure out how to have a "Formatter" type to write, like "Parser" to read
TODO: rename "Parser" to "Reader" and "Formatter" to "Writer" (?)

-}
type Expr
    = Typ (List String) -- Type    %True False%
    | Int Int -- 42
    | Num Float -- 3.14
    | Var String -- x
    | Lam String Expr -- λx -> y
    | FrA String Type -- ∀a. b
    | App Expr Expr -- f x
    | Fnc Type Type -- a -> b
    | TE Expr Type -- x : a
      -- Error reporting
    | Pos String ( Int, Int ) ( Int, Int ) Expr -- filename start end Expr


{-| Types are just Expressions, and any valid Expression is a valid type.

This is just a name alias to document better when we're expecting any Expression or specifically a type.

-}
type alias Type =
    Expr


{-| An error from parsing or evaluation.
-}
type Error
    = SyntaxError Parser.Error
    | VariableNotFound Expr
    | TypeMismatch Type Type -- got, expected


type alias Env a =
    { names : Dict String ( Expr, Type )
    , types : DisjointSet Expr
    , nameSeed : Int
    , result : Result Error a
    }


newEnv : Env ()
newEnv =
    { names =
        Dict.fromList
            [ ( "Type", ( Var "Type", Var "Type" ) )
            , ( "Int", ( Var "Int", Var "Type" ) )
            , ( "Num", ( Var "Num", Var "Type" ) )
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

    -- Lamtraction
    read "λx -> y"   --> Ok (Lam "x" (Var "y"))
    read "λx y -> z" --> Ok (Lam "x" (Lam "y" (Var "z")))

    -- Application
    read "f x" --> Ok (App (Var "f") (Var "x"))

    -- TODO: ^ * +

    -- Typed Expression
    read "x : a" --> Ok (TE (Var "x") (Var "a"))

    -- TODO: |

    -- Type abstraction
    read "a -> b" --> Ok (Fnc (Var "a") (Var "b"))

    -- Variable definitions
    read "x := y; z"    --> Ok (letVar "x" (Var "y") (Var "z"))
    read "x : a = y; z" --> Ok (letVar "x" (TE (Var "y") (Var "a")) (Var "z"))

    -- Operator precedence
    read "x y z"      --> Ok (App (App (Var "x") (Var "y")) (Var "z"))
    read "x y ^ z"    --> Ok (exp (App (Var "x") (Var "y")) (Var "z"))
    read "x y * z"    --> Ok (mul (App (Var "x") (Var "y")) (Var "z"))
    read "x y + z"    --> Ok (add (App (Var "x") (Var "y")) (Var "z"))
    read "x y -> z"   --> Ok (Fnc (App (Var "x") (Var "y")) (Var "z"))
    read "x y : z"    --> Ok (TE (App (Var "x") (Var "y")) (Var "z"))
    read "x λy -> z"  --> Ok (App (Var "x") (Lam "y" (Var "z")))

    read "x ^ y z"     --> Ok (exp (Var "x") (App (Var "y") (Var "z")))
    read "x ^ y ^ z"   --> Ok (exp (Var "x") (exp (Var "y") (Var "z")))
    read "x ^ y * z"   --> Ok (mul (exp (Var "x") (Var "y")) (Var "z"))
    read "x ^ y + z"   --> Ok (add (exp (Var "x") (Var "y")) (Var "z"))
    read "x ^ y -> z"  --> Ok (Fnc (exp (Var "x") (Var "y")) (Var "z"))
    read "x ^ y : z"   --> Ok (TE (exp (Var "x") (Var "y")) (Var "z"))
    read "x ^ λy -> z" --> Ok (exp (Var "x") (Lam "y" (Var "z")))

    read "x * y z"     --> Ok (mul (Var "x") (App (Var "y") (Var "z")))
    read "x * y ^ z"   --> Ok (mul (Var "x") (exp (Var "y") (Var "z")))
    read "x * y * z"   --> Ok (mul (mul (Var "x") (Var "y")) (Var "z"))
    read "x * y + z"   --> Ok (add (mul (Var "x") (Var "y")) (Var "z"))
    read "x * y : z"   --> Ok (TE (mul (Var "x") (Var "y")) (Var "z"))
    read "x * y -> z"  --> Ok (Fnc (mul (Var "x") (Var "y")) (Var "z"))
    read "x * λy -> z" --> Ok (mul (Var "x") (Lam "y" (Var "z")))

    read "x + y z"     --> Ok (add (Var "x") (App (Var "y") (Var "z")))
    read "x + y ^ z"   --> Ok (add (Var "x") (exp (Var "y") (Var "z")))
    read "x + y * z"   --> Ok (add (Var "x") (mul (Var "y") (Var "z")))
    read "x + y + z"   --> Ok (add (add (Var "x") (Var "y")) (Var "z"))
    read "x + y -> z"  --> Ok (Fnc (add (Var "x") (Var "y")) (Var "z"))
    read "x + y : z"   --> Ok (TE (add (Var "x") (Var "y")) (Var "z"))
    read "x + λy -> z" --> Ok (add (Var "x") (Lam "y" (Var "z")))

    read "x -> y z"     --> Ok (Fnc (Var "x") (App (Var "y") (Var "z")))
    read "x -> y ^ z"   --> Ok (Fnc (Var "x") (exp (Var "y") (Var "z")))
    read "x -> y * z"   --> Ok (Fnc (Var "x") (mul (Var "y") (Var "z")))
    read "x -> y + z"   --> Ok (Fnc (Var "x") (add (Var "y") (Var "z")))
    read "x -> y : z"   --> Ok (TE (Fnc (Var "x") (Var "y")) (Var "z"))
    read "x -> y -> z"  --> Ok (Fnc (Var "x") (Fnc (Var "y") (Var "z")))
    read "x -> λy -> z" --> Ok (Fnc (Var "x") (Lam "y" (Var "z")))

    read "x : y z"     --> Ok (TE (Var "x") (App (Var "y") (Var "z")))
    read "x : y ^ z"   --> Ok (TE (Var "x") (exp (Var "y") (Var "z")))
    read "x : y * z"   --> Ok (TE (Var "x") (mul (Var "y") (Var "z")))
    read "x : y + z"   --> Ok (TE (Var "x") (add (Var "y") (Var "z")))
    read "x : y -> z"  --> Ok (TE (Var "x") (Fnc (Var "y") (Var "z")))
    read "x : y : z"   --> Ok (TE (TE (Var "x") (Var "y")) (Var "z"))
    read "x : λy -> z" --> Ok (TE (Var "x") (Lam "y" (Var "z")))

    read "λx -> y z"     --> Ok (Lam "x" (App (Var "y") (Var "z")))
    read "λx -> y ^ z"   --> Ok (Lam "x" (exp (Var "y") (Var "z")))
    read "λx -> y * z"   --> Ok (Lam "x" (mul (Var "y") (Var "z")))
    read "λx -> y + z"   --> Ok (Lam "x" (add (Var "y") (Var "z")))
    read "λx -> y -> z"  --> Ok (Lam "x" (Fnc (Var "y") (Var "z")))
    read "λx -> y : z"   --> Ok (Lam "x" (TE (Var "y") (Var "z")))
    read "λx -> λy -> z" --> Ok (Lam "x" (Lam "y" (Var "z")))

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

    -- Lamtraction
    write (Lam "x" (Var "y")) --> "λx -> y"

    -- Application
    write (App (Var "f") (Var "x")) --> "f x"

    -- TODO: ^ * +

    -- Typed Expression
    write (TE (Var "x") (Var "a")) --> "x : a"

    -- TODO: |

    -- Type abstraction
    write (Fnc (Var "a") (Var "b")) --> "a -> b"

    -- Variable definitions
    write (App (Lam "x" (Var "z")) (Var "y"))                --> "x := y; z"
    write (App (Lam "x" (Var "z")) (TE (Var "y") (Var "a"))) --> "x : a = y; z"

    -- Operator precedence
    write (App (App (Var "x") (Var "y")) (Var "z"))  --> "x y z"
    write (exp (App (Var "x") (Var "y")) (Var "z"))  --> "x y ^ z"
    write (mul (App (Var "x") (Var "y")) (Var "z"))  --> "x y * z"
    write (add (App (Var "x") (Var "y")) (Var "z"))  --> "x y + z"
    write (Fnc (App (Var "x") (Var "y")) (Var "z")) --> "x y -> z"
    write (TE (App (Var "x") (Var "y")) (Var "z"))   --> "x y : z"
    write (App (Var "x") (Lam "y" (Var "z")))        --> "x (λy -> z)"

    write (exp (Var "x") (App (Var "y") (Var "z")))  --> "x ^ y z"
    write (exp (Var "x") (exp (Var "y") (Var "z")))  --> "x ^ y ^ z"
    write (mul (exp (Var "x") (Var "y")) (Var "z"))  --> "x ^ y * z"
    write (add (exp (Var "x") (Var "y")) (Var "z"))  --> "x ^ y + z"
    write (Fnc (exp (Var "x") (Var "y")) (Var "z")) --> "x ^ y -> z"
    write (TE (exp (Var "x") (Var "y")) (Var "z"))   --> "x ^ y : z"
    write (exp (Var "x") (Lam "y" (Var "z")))        --> "x ^ (λy -> z)"

    write (mul (Var "x") (App (Var "y") (Var "z")))  --> "x * y z"
    write (mul (Var "x") (exp (Var "y") (Var "z")))  --> "x * y ^ z"
    write (mul (mul (Var "x") (Var "y")) (Var "z"))  --> "x * y * z"
    write (add (mul (Var "x") (Var "y")) (Var "z"))  --> "x * y + z"
    write (Fnc (mul (Var "x") (Var "y")) (Var "z")) --> "x * y -> z"
    write (TE (mul (Var "x") (Var "y")) (Var "z"))   --> "x * y : z"
    write (mul (Var "x") (Lam "y" (Var "z")))        --> "x * (λy -> z)"

    write (add (Var "x") (App (Var "y") (Var "z")))  --> "x + y z"
    write (add (Var "x") (exp (Var "y") (Var "z")))  --> "x + y ^ z"
    write (add (Var "x") (mul (Var "y") (Var "z")))  --> "x + y * z"
    write (add (add (Var "x") (Var "y")) (Var "z"))  --> "x + y + z"
    write (Fnc (add (Var "x") (Var "y")) (Var "z")) --> "x + y -> z"
    write (TE (add (Var "x") (Var "y")) (Var "z"))   --> "x + y : z"
    write (add (Var "x") (Lam "y" (Var "z")))        --> "x + (λy -> z)"

    write (Fnc (Var "x") (App (Var "y") (Var "z")))  --> "x -> y z"
    write (Fnc (Var "x") (exp (Var "y") (Var "z")))  --> "x -> y ^ z"
    write (Fnc (Var "x") (mul (Var "y") (Var "z")))  --> "x -> y * z"
    write (Fnc (Var "x") (add (Var "y") (Var "z")))  --> "x -> y + z"
    write (Fnc (Var "x") (Fnc (Var "y") (Var "z"))) --> "x -> y -> z"
    write (TE (Fnc (Var "x") (Var "y")) (Var "z"))   --> "x -> y : z"
    write (Fnc (Var "x") (Lam "y" (Var "z")))        --> "x -> (λy -> z)"

    write (TE (Var "x") (App (Var "y") (Var "z")))  --> "x : y z"
    write (TE (Var "x") (exp (Var "y") (Var "z")))  --> "x : y ^ z"
    write (TE (Var "x") (mul (Var "y") (Var "z")))  --> "x : y * z"
    write (TE (Var "x") (add (Var "y") (Var "z")))  --> "x : y + z"
    write (TE (Var "x") (Fnc (Var "y") (Var "z"))) --> "x : y -> z"
    write (TE (TE (Var "x") (Var "y")) (Var "z"))   --> "x : y : z"
    write (TE (Var "x") (Lam "y" (Var "z")))        --> "x : (λy -> z)"

    write (Lam "x" (App (Var "y") (Var "z")))  --> "λx -> y z"
    write (Lam "x" (exp (Var "y") (Var "z")))  --> "λx -> y ^ z"
    write (Lam "x" (mul (Var "y") (Var "z")))  --> "λx -> y * z"
    write (Lam "x" (add (Var "y") (Var "z")))  --> "λx -> y + z"
    write (Lam "x" (TE (Var "y") (Var "z")))   --> "λx -> y : z"
    write (Lam "x" (Fnc (Var "y") (Var "z"))) --> "λx -> y -> z"
    write (Lam "x" (Lam "y" (Var "z")))        --> "λx y -> z"

    TODO: fix this!
    write (add (Var "x") (add (Var "y") (Var "z"))) -- "x + (y + z)"
    write (exp (exp (Var "x") (Var "y")) (Var "z")) -- "(x ^ y) ^ z"

-}
write : Expr -> String
write expr =
    let
        unop1 : Expr -> Expr -> String
        unop1 op e =
            if precedence e < precedence op then
                "(" ++ write e ++ ")"

            else
                write e

        unop2 : Expr -> Expr -> String
        unop2 op e =
            if precedence e <= precedence op then
                "(" ++ write e ++ ")"

            else
                write e
    in
    case expr of
        Typ [] ->
            "Type"

        Typ ctors ->
            "%" ++ String.join " " ctors ++ "%"

        Int k ->
            String.fromInt k

        Num k ->
            String.fromFloat k

        Var x ->
            x

        Lam _ _ ->
            tupleMap (\xs y -> "λ" ++ String.join " " xs ++ " -> " ++ write y)
                (asFunc expr)

        FrA _ _ ->
            Debug.todo "write FrA"

        App (Lam x e) (TE v t) ->
            x ++ " : " ++ write t ++ " = " ++ write v ++ "; " ++ write e

        App (Lam x e) v ->
            x ++ " := " ++ write v ++ "; " ++ write e

        App ((App (Var "^") e1) as op) e2 ->
            unop2 op e1 ++ " ^ " ++ unop1 op e2

        App ((App (Var "*") e1) as op) e2 ->
            unop1 op e1 ++ " * " ++ unop2 op e2

        App ((App (Var "+") e1) as op) e2 ->
            unop1 op e1 ++ " + " ++ unop2 op e2

        App e1 e2 ->
            unop1 expr e1 ++ " " ++ unop2 expr e2

        TE e t ->
            unop1 expr e ++ " : " ++ unop2 expr t

        Fnc t1 t2 ->
            unop2 expr t1 ++ " -> " ++ unop1 expr t2

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
    List.foldr Lam output inputs


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
                Lam x y ->
                    Tuple.mapFirst ((::) x) (asFunc_ xs y)

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
    List.foldr Fnc output inputs


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
                Fnc x y ->
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

    Ok (letVar "x" (Var "y") (Var "z"))                --> read "(λx -> z) y"
    Ok (letVar "x" (Var "y") (Var "z"))                --> read "x := y; z"
    Ok (letVar "x" (TE (Var "y") (Var "a")) (Var "z")) --> read "x : a = y; z"

-}
letVar : String -> Expr -> Expr -> Expr
letVar name value expr =
    App (Lam name expr) value


asConstructors : Expr -> Type -> List ( String, Type )
asConstructors expr typ =
    case asCall expr of
        -- ( Or x1 x2, [] ) ->
        --     asConstructors x1 typ ++ asConstructors x2 typ
        ( TE x t, [] ) ->
            asConstructors x t

        ( Var x, types ) ->
            [ ( x, funcType types typ ) ]

        _ ->
            [ ( "", typ ) ]


{-| Define a new variable in an `Env`.

    import Dict

    -- We evaluate the Expression to get its type.
    define "x" (Int 42) newEnv |> .names
    --> Dict.insert "x" (Int 42, Var "Int") newEnv.names

    -- Errors are part of the result.
    define "x" (Var "y") newEnv
    --> newEnv |> withError (VariableNotFound (Var "y"))

    -- Note that we already know the type of a typed Expression,
    -- so we can defer its actual evaluation until it's used.
    -- This allows to define recursive functions and other Expressions
    -- that depend on variables that haven't been defined yet.
    define "x" (TE (Var "y") (Var "Int")) newEnv |> .names
    --> Dict.insert "x" (Var "y", Var "Int") newEnv.names

    -- The type can also be a type variable.
    define "x" (TE (Var "y") (Var "a")) newEnv |> .names
    --> Dict.insert "x" (Var "y", TE (Var "a") (Var "Type")) newEnv.names

-}
define : String -> Expr -> Env a -> Env a
define name value env =
    case value of
        TE expr typ ->
            andThen
                (\( t, _ ) _ ->
                    { env | names = Dict.insert name ( expr, t ) env.names }
                )
                (evalType typ env)

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

    writeDefinitions : List String -> Env a -> List String
    writeDefinitions names env =
        List.filterMap
            (\name ->
                Maybe.map
                    (\( value, typ ) ->
                        name ++ " : " ++ write typ ++ " = " ++ write value
                    )
                    (Dict.get name env.names)
            )
            names

    defineType ("T", []) [] newEnv |> writeDefinitions [ "T" ]
    --> [ "T : Type = T" ]

    -- T := A
    newEnv
        |> defineType ( "T", [] )
            [ ( "A", Var "T" ) ]
        |> writeDefinitions [ "T", "A" ]
    --> [ "T : %A% = T"
    --> , "A : T = A"
    --> ]

    -- Bool := True | False
    newEnv
        |> defineType ( "Bool", [] )
            [ ( "True", Var "Bool" )
            , ( "False", Var "Bool" )
            ]
        |> writeDefinitions [ "Bool", "True", "False" ]
    -- [ "Bool : %True False% = Bool"
    -- , "True : Bool = True"
    -- , "False : Bool = False"
    -- ]

    -- Maybe a := Just a | Nothing
    newEnv
        |> defineType ( "Maybe", [ Var "a" ] )
            [ ( "Just", funcType [ Var "a" ] (App (Var "Maybe") (Var "a")) ]
            , ( "Nothing", App (Var "Maybe") (Var "a") )
            ]
        |> writeDefinitions [ "Maybe", "Just", "Nothing" ]
    -- [ "Maybe : Type -> %Just Nothing% = Maybe"
    -- , "Just : ∀a. a -> Maybe a = Just"
    -- , "Nothing : ∀a. Maybe a = Nothing"
    -- ]

    -- Vec Int a := Cons a (Vec n a) : Vec (n + 1) a | Nil 0 a
    newEnv
        |> defineType ( "Vec", [ Var "Int", Var "a" ] )
            [ ( "Cons"
                , [ Var "a", call (Var "Vec") [ Var "n", Var "a" ] ]
                , call (Var "Vec") [ add (Var "n") (Int 1), Var "a" ]
                )
            , ( "Nil", [], call (Var "Vec") [ Int 0, Var "a" ] )
            ]
        |> writeDefinitions [ "Vec", "Cons", "Nil" ]
    -- [ "Vec : Int -> Type -> Type = λa b -> Vec a b"
    -- , "Cons : a -> Vec n a -> Vec (n + 1) a = λd e b c -> b d e"
    -- , "Nil : Vec 0 a = λb c -> c"
    -- ]

-}
defineType : ( String, List Expr ) -> List ( String, Type ) -> Env a -> Env a
defineType ( typeName, typeInputs ) constructors env =
    env
        |> eval (funcType typeInputs (Typ (List.map Tuple.first constructors)))
        |> map Tuple.second
        |> andThen
            (\typeType env_ ->
                List.foldl
                    (\( name, typ ) -> define name (TE (Var name) typ))
                    (define typeName (TE (Var typeName) typeType) env_)
                    constructors
            )
        |> withResult env.result


{-| Creates a new type variable with a unique name.

    import Dict
    import DisjointSet

    -- Starts with "a".
    newTypeVar newEnv |> .result --> Ok (TE (Var "a") (Var "Type"))

    -- Goes alphabetically if a type is already defined.
    newTypeVar { newEnv | types = DisjointSet.add [ Var "a" ] DisjointSet.empty } |> .result --> Ok (TE (Var "b") (Var "Type"))
    newTypeVar { newEnv | names = Dict.singleton "a" (Var "Type", Var "Type") } |> .result --> Ok (TE (Var "b") (Var "Type"))

    -- Names can have multiple letters when a-z are all used up.
    newTypeVar { newEnv | nameSeed = 1234 } |> .result --> Ok (TE (Var "ast") (Var "Type"))

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
            List.filterMap maybeTypeName (DisjointSet.items env.types)
                ++ Dict.keys env.names
                |> Set.fromList
    in
    newVarName env.nameSeed existingNames
        |> tupleMap
            (\t newSeed ->
                withValue (TE (Var t) (Var "Type"))
                    { env | nameSeed = newSeed }
            )


{-| Evaluates an Expression.

    import Lambda

    evalText : String -> Result Error (String, String)
    evalText txt =
        newEnv
            |> withResult (read "42")
            |> andThen (define "x")
            |> withResult (read "y : Num")
            |> andThen (define "y")
            |> withResult (read "f : Int -> Num")
            |> andThen (define "f")
            |> withResult (read "g : a -> a")
            |> andThen (define "g")
            |> withResult (read txt)
            |> andThen eval
            |> .result
            |> Result.map (Tuple.mapBoth write write)

    -- Values
    evalText "42"   --> Ok ( "42", "Int" )
    evalText "3.14" --> Ok ( "3.14", "Num" )

    -- Variables
    evalText "x" --> Ok ( "42", "Int" )
    evalText "y" --> Ok ( "y", "Num" )
    evalText "z" --> Err (VariableNotFound (Var "z"))
    evalText "f" --> Ok ( "f", "Int -> Num" )
    evalText "g" --> Ok ( "g", "(a : Type) -> (a : Type)" )
    evalText "h" --> Err (VariableNotFound (Var "h"))

    -- Lambda abstraction
    evalText "λx -> x" --> Ok ( "λx -> x", "(a : Type) -> (a : Type)" )
    evalText "λx -> y" --> Ok ( "λx -> y", "(a : Type) -> Num" )
    evalText "λy -> x" --> Ok ( "λy -> 42", "(a : Type) -> Int" )
    evalText "λx -> z" --> Err (VariableNotFound (Var "z"))

    -- Application
    evalText "f x" --> Ok ( "f 42", "Num" )
    evalText "f y" --> Err (TypeMismatch (Var "Num") (Var "Int"))
    evalText "g x" --> Ok ( "g 42", "Int" )
    evalText "g y" --> Ok ( "g y", "Num" )
    evalText "h z" --> Err (VariableNotFound (Var "h"))
    evalText "f z" --> Err (VariableNotFound (Var "z"))
    evalText "(λx -> x) 42"    --> Ok ( "42", "Int" )
    evalText "(λx -> 3.14) 42" --> Ok ( "3.14", "Num" )

    -- Function types
    evalText "a -> b"  --> Ok ( "(a : Type) -> (b : Type)", "Type -> Type" )
    evalText "a -> 42" --> Ok ( "(a : Type) -> 42", "Type -> Int" )

    -- Typed Expression (lazy evaluation)
    evalText "x : a" --> Ok ( "x", "a : Type" )
    evalText "z : a" --> Ok ( "z", "a : Type" )

    -- Type unions (TODO: define semantics for this)
    evalText "1 | 2" -- Ok ( "1 | 2", "Num" )
    evalText "42 | 3.14" -- Err (TypeMismatch (Var "Int") (Var "Num"))
    evalText "Int | Num" -- Ok ( "Int | Num", "Type" )
    evalText "A : Type | B : Type" -- Ok ( "A : Type | B : Type", "Type" )

    -- Variable definitions (inferred by lowercase name)
    evalText "x := 42; x"   --> Ok ( "42", "Int" )
    evalText "x : a = y; x" --> Ok ( "y", "Num" )

    -- Named type definitions (inferred by uppercase name)
    -- Undefined names are defined as constructors.
    evalText "Maybe a := Just a | Nothing; Just 42" -- Ok ( "Just 42", "Maybe Int" )
    evalText "Maybe a := Just a | Nothing; Nothing" -- Ok ( "Nothing", "Maybe a" )

    -- Case application
    evalText "Bool := True | False; True 1 2" -- Ok ( "1", "Int" )
    evalText "Bool := True | False; False 1 2" -- Ok ( "2", "Int" )

-}
eval : Expr -> Env a -> Env ( Expr, Type )
eval expr env =
    case expr of
        Typ _ ->
            withValue ( expr, expr ) env

        Int _ ->
            withValue ( expr, Var "Int" ) env

        Num _ ->
            withValue ( expr, Var "Num" ) env

        Var x ->
            case Dict.get x env.names of
                Just ( value, typ ) ->
                    if typ == Var "Type" then
                        withValue ( expr, typ ) env

                    else if value == Var x then
                        withValue ( value, typ ) env

                    else
                        -- Lazy evaluation through typed Expressions
                        -- TODO: unify the result type with the definition type.
                        -- TODO: update the name definition to the result
                        eval value env

                Nothing ->
                    withError (VariableNotFound (Var x)) env

        -- Lam x (TE y yType) ->
        --     andThen2
        --         (\yT xT env_ ->
        --             define x (TE (Var x) xT) env_
        --                 |> withValue ( Lam x y, Fnc xT yT )
        --         )
        --         (evalType yType env)
        --         newTypeVar
        Lam x y ->
            andThen
                (\xT env_ ->
                    define x (TE (Var x) xT) env_
                        |> eval_ y
                        |> map (Tuple.mapBoth (Lam x) (Fnc xT))
                )
                (newTypeVar env)

        FrA a b ->
            Debug.todo "eval FrA"

        -- Type definition
        App ((Lam x y) as e1) ((TE def typ) as e2) ->
            let
                ( inputTypes, outputType ) =
                    asFuncType typ
            in
            if outputType == Var "Type" then
                env
                    |> defineType ( x, inputTypes )
                        (asConstructors def (call (Var x) inputTypes))
                    |> eval_ y

            else
                apply e1 e2 env

        App e1 e2 ->
            apply e1 e2 env

        -- exp (Int e1) (Int e2) ->
        --     eval (Int (e1 ^ e2)) env
        -- exp (Num e1) (Num e2) ->
        --     eval (Num (e1 ^ e2)) env
        -- exp e1 e2 ->
        --     Debug.todo "eval exp"
        -- mul e1 e2 ->
        --     Debug.todo "eval mul"
        -- add e1 e2 ->
        --     Debug.todo "eval add"
        TE e t ->
            evalType t env
                |> map Tuple.first
                |> map (Tuple.pair e)

        -- Or e1 e2 ->
        --     Debug.todo "eval Or"
        Fnc t1 t2 ->
            map2 (\( x, xt ) ( y, yt ) -> ( Fnc x y, Fnc xt yt ))
                (evalType t1 env)
                (evalType t2)

        Pos source start end e ->
            Debug.todo "eval Pos"


apply : Expr -> Expr -> Env a -> Env ( Expr, Type )
apply e1 e2 env =
    andThen3
        (\outT ( v1, t1 ) ( v2, t2 ) env_ ->
            map (\_ -> ( v1, v2, outT ))
                (unify (Fnc t2 outT) t1 env_)
        )
        (newTypeVar env)
        (eval_ e1)
        (eval_ e2)
        |> andThen
            (\( v1, v2, outT ) env_ ->
                case v1 of
                    -- Apply to a lambda abstraction, reduce! 🎉
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


eval_ : Expr -> Env a -> Env ( Expr, Type )
eval_ =
    -- Workaround for: https://github.com/elm/compiler/issues/2186
    eval


{-| Evaluates a type Expression.

    env : Env ()
    env =
        newEnv
            |> define "x" (Int 42)
            |> defineType ( "Bool", [] )
                [ ( "True", Var "Bool" )
                , ( "False", Var "Bool" )
                ]
            -- |> defineType ( "Maybe", [ Var "a" ] )
            --     [ ( "Just", funcType [ Var "a" ] (App (Var "Maybe") (Var "a")) ]
            --     , ( "Nothing", [], App (Var "Maybe") (Var "a") )
            --     ]

    -- If a name doesn't exist, it's interpreted as a type variable.
    -- Type variables look like (TE (Var name) type)
    evalType (TE (Var "a") (Var "Type")) env |> .result --> Ok (TE (Var "a") (Var "Type"), Var "Type")
    evalType (TE (Var "a") (Var "Int")) env |> .result  --> Ok (TE (Var "a") (Var "Int"), Var "Int")
    evalType (Var "a") env |> .result                   --> Ok (TE (Var "a") (Var "Type"), Var "Type")

    -- Any valid Expression can be a valid type.
    evalType (Int 42) env |> .result     --> Ok (Int 42, Var "Int")
    evalType (Var "x") env |> .result    --> Ok (Int 42, Var "Int")
    evalType (Var "Int") env |> .result  --> Ok (Var "Int", Var "Type")
    evalType (Var "Bool") env |> .result --> Ok (Var "Bool", Typ [ "True", "False" ])
    evalType (Var "True") env |> .result --> Ok (Var "True", Var "Bool")
    evalType (App (Var "Maybe") (Var "Int")) env |> .result -- Ok (App (Var "Maybe") (Var "Int"), Var "Type")
    evalType (App (Var "Just") (Var "x")) env |> .result    -- Ok (App (Var "Just") (Int 42), App (Var "Maybe") (Var "Int"))

-}
evalType : Type -> Env a -> Env ( Type, Type )
evalType typ env =
    case typ of
        Var x ->
            if Dict.member x env.names then
                eval typ env

            else
                DisjointSet.find typ env.types
                    |> Maybe.map (\t -> eval t env)
                    |> Maybe.withDefault
                        -- (withValue ( typ, typ ) env)
                        (withValue ( TE typ (Var "Type"), Var "Type" ) env)

        -- Type variable
        TE (Var _) t ->
            withValue ( typ, t ) env

        _ ->
            eval typ env


{-| Tries to unify two types or get a `TypeMismatch` error.

    import Lambda

    -- Bound types
    unify (Var "Int") (Var "Int") newEnv |> .result --> Ok (Var "Int")
    unify (Var "Int") (Var "Num") newEnv |> .result --> Err (TypeMismatch (Var "Int") (Var "Num"))

    -- Type variables
    unify (Var "a") (Var "a") newEnv |> .result --> Ok (TE (Var "a") (Var "Type"))
    unify (Var "a") (Var "b") newEnv |> .result --> Ok (TE (Var "a") (Var "Type"))

    -- Bound types with type variables
    unify (Var "Int") (Var "a") newEnv |> .result --> Ok (Var "Int")
    unify (Var "a") (Var "Int") newEnv |> .result --> Ok (Var "Int")

    -- Lamtraction types
    unify (Fnc (Var "Int") (Var "Num")) (Fnc (Var "Int") (Var "Num")) newEnv |> .result -- Ok (Fnc (Var "Int") (Var "Num"))
    unify (Fnc (Var "Int") (Var "Num")) (Var "a") newEnv |> .result                      -- Ok (Fnc (Var "Int") (Var "Num"))
    unify (Var "a") (Fnc (Var "Int") (Var "Num")) newEnv |> .result                      -- Ok (Fnc (Var "Int") (Var "Num"))
    unify (Fnc (Var "Int") (Var "Num")) (Fnc (Var "a") (Var "b")) newEnv |> .result     -- Ok (Fnc (Var "Int") (Var "Num"))
    unify (Fnc (Var "a") (Var "b")) (Fnc (Var "Int") (Var "Num")) newEnv |> .result     -- Ok (Fnc (Var "Int") (Var "Num"))

-}
unify : Type -> Type -> Env a -> Env Type
unify type1 type2 env =
    andThen2
        (\( t1, _ ) ( t2, _ ) env_ ->
            case ( t1, t2 ) of
                ( Fnc a1 b1, Fnc a2 b2 ) ->
                    map2 Fnc
                        (unify_ a1 a2 env_)
                        (unify_ b1 b2)

                ( _, TE (Var _) _ ) ->
                    -- Bind the type variable `t2` to `t1`.
                    -- `unify a b` should return `a`.
                    withValue t1
                        { env_ | types = DisjointSet.union t1 t2 env_.types }

                ( TE (Var _) _, _ ) ->
                    unify t2 t1 env

                _ ->
                    if t1 == t2 then
                        withValue t1 env

                    else
                        withError (TypeMismatch t1 t2) env
        )
        (evalType type1 env)
        (evalType type2)


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
    -- TODO: change to andThen2 : (a -> .. -> Env c) -> (Env a -> Env b) -> (Env b -> Env c) -> Env a -> Env c
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


flip : (a -> b -> c) -> b -> a -> c
flip f x y =
    f y x



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
    in
    Parser.Expression.expression
        [ [ fromLeft App spaces ]
        , [ fromRight exp (char '^') ]
        , [ fromLeft mul (char '*') ]
        , [ fromLeft add (char '+') ]
        , [ fromRight Fnc (text "->") ]
        , [ fromLeft TE (char ':') ]

        -- , [ fromLeft Or (text "|") ]
        , [ inbetween identity (char '(') (char ')')
          , term identity funcP
          , term identity letVarP
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
        -- TODO: adjust these 100s
        Typ _ ->
            100

        Int _ ->
            100

        Num _ ->
            100

        Var _ ->
            100

        App (Var "^") _ ->
            6

        App (Var "*") _ ->
            5

        App (Var "+") _ ->
            4

        App _ _ ->
            7

        Fnc _ _ ->
            3

        TE _ _ ->
            2

        -- Or _ _ ->
        --     1
        Lam _ _ ->
            0

        FrA _ _ ->
            0

        Pos _ _ _ e ->
            precedence e

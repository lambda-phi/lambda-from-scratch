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
    , asForAll
    , asFunc
    , asFuncType
    , call
    , define
    , defineType
    , eval
    , evalType
    , exp
    , forAll
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
             Γ ⊢ f s : ((\x:S. T) s)

    Lam
        if   Γ{x:S} ⊢ y : T
             Γ ⊢ (∀x:S. T) : *n
        then Γ ⊢ (λx. y) : (∀a b. a -> b)

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
    = Any -- @                  anything / free type / type variable
    | Int Int -- 42             integer value
    | Num Float -- 3.14         number value / floating point value
    | Var String -- x           variable
    | Fnc Type Type -- a -> b   function type
    | Lam String Expr -- λx. y  lambda abstraction
    | For String Type -- ∀a. b  for all types / type abstraction
    | App Expr Expr -- f x      application
    | TE Expr Type -- x : a     typed expression


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

    -- Function type
    read "a -> b" --> Ok (Fnc (Var "a") (Var "b"))

    -- Lambda abstraction
    read "λx. y"   --> Ok (Lam "x" (Var "y"))
    read "λx y. z" --> Ok (Lam "x" (Lam "y" (Var "z")))

    -- Type abstraction
    read "∀a. b" --> Ok (For "a" (Var "b"))
    read "∀a b. c" --> Ok (For "a" (For "b" (Var "c")))

    -- Application
    read "f x" --> Ok (App (Var "f") (Var "x"))

    -- Typed Expression
    read "x : a" --> Ok (TE (Var "x") (Var "a"))

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
    read "x λy. z"    --> Ok (App (Var "x") (Lam "y" (Var "z")))

    read "x ^ y z"     --> Ok (exp (Var "x") (App (Var "y") (Var "z")))
    read "x ^ y ^ z"   --> Ok (exp (Var "x") (exp (Var "y") (Var "z")))
    read "x ^ y * z"   --> Ok (mul (exp (Var "x") (Var "y")) (Var "z"))
    read "x ^ y + z"   --> Ok (add (exp (Var "x") (Var "y")) (Var "z"))
    read "x ^ y -> z"  --> Ok (Fnc (exp (Var "x") (Var "y")) (Var "z"))
    read "x ^ y : z"   --> Ok (TE (exp (Var "x") (Var "y")) (Var "z"))
    read "x ^ λy. z"   --> Ok (exp (Var "x") (Lam "y" (Var "z")))

    read "x * y z"     --> Ok (mul (Var "x") (App (Var "y") (Var "z")))
    read "x * y ^ z"   --> Ok (mul (Var "x") (exp (Var "y") (Var "z")))
    read "x * y * z"   --> Ok (mul (mul (Var "x") (Var "y")) (Var "z"))
    read "x * y + z"   --> Ok (add (mul (Var "x") (Var "y")) (Var "z"))
    read "x * y : z"   --> Ok (TE (mul (Var "x") (Var "y")) (Var "z"))
    read "x * y -> z"  --> Ok (Fnc (mul (Var "x") (Var "y")) (Var "z"))
    read "x * λy. z"   --> Ok (mul (Var "x") (Lam "y" (Var "z")))

    read "x + y z"     --> Ok (add (Var "x") (App (Var "y") (Var "z")))
    read "x + y ^ z"   --> Ok (add (Var "x") (exp (Var "y") (Var "z")))
    read "x + y * z"   --> Ok (add (Var "x") (mul (Var "y") (Var "z")))
    read "x + y + z"   --> Ok (add (add (Var "x") (Var "y")) (Var "z"))
    read "x + y -> z"  --> Ok (Fnc (add (Var "x") (Var "y")) (Var "z"))
    read "x + y : z"   --> Ok (TE (add (Var "x") (Var "y")) (Var "z"))
    read "x + λy. z"   --> Ok (add (Var "x") (Lam "y" (Var "z")))

    read "x -> y z"     --> Ok (Fnc (Var "x") (App (Var "y") (Var "z")))
    read "x -> y ^ z"   --> Ok (Fnc (Var "x") (exp (Var "y") (Var "z")))
    read "x -> y * z"   --> Ok (Fnc (Var "x") (mul (Var "y") (Var "z")))
    read "x -> y + z"   --> Ok (Fnc (Var "x") (add (Var "y") (Var "z")))
    read "x -> y : z"   --> Ok (TE (Fnc (Var "x") (Var "y")) (Var "z"))
    read "x -> y -> z"  --> Ok (Fnc (Var "x") (Fnc (Var "y") (Var "z")))
    read "x -> λy. z"   --> Ok (Fnc (Var "x") (Lam "y" (Var "z")))

    read "x : y z"     --> Ok (TE (Var "x") (App (Var "y") (Var "z")))
    read "x : y ^ z"   --> Ok (TE (Var "x") (exp (Var "y") (Var "z")))
    read "x : y * z"   --> Ok (TE (Var "x") (mul (Var "y") (Var "z")))
    read "x : y + z"   --> Ok (TE (Var "x") (add (Var "y") (Var "z")))
    read "x : y -> z"  --> Ok (TE (Var "x") (Fnc (Var "y") (Var "z")))
    read "x : y : z"   --> Ok (TE (TE (Var "x") (Var "y")) (Var "z"))
    read "x : λy. z"   --> Ok (TE (Var "x") (Lam "y" (Var "z")))

    read "λx. y z"     --> Ok (Lam "x" (App (Var "y") (Var "z")))
    read "λx. y ^ z"   --> Ok (Lam "x" (exp (Var "y") (Var "z")))
    read "λx. y * z"   --> Ok (Lam "x" (mul (Var "y") (Var "z")))
    read "λx. y + z"   --> Ok (Lam "x" (add (Var "y") (Var "z")))
    read "λx. y -> z"  --> Ok (Lam "x" (Fnc (Var "y") (Var "z")))
    read "λx. y : z"   --> Ok (Lam "x" (TE (Var "y") (Var "z")))
    read "λx. λy. z"   --> Ok (Lam "x" (Lam "y" (Var "z")))

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
    write (Fnc (Var "a") (Var "b")) --> "a -> b"

    -- Lambda abstraction
    write (Lam "x" (Var "y")) --> "λx. y"

    -- Type abstraction
    write (For "a" (Var "b")) --> "∀a. b"

    -- Application
    write (App (Var "f") (Var "x")) --> "f x"

    -- Typed Expression
    write (TE (Var "x") (Var "a")) --> "x : a"

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
    write (App (Var "x") (Lam "y" (Var "z")))        --> "x (λy. z)"

    write (exp (Var "x") (App (Var "y") (Var "z")))  --> "x ^ y z"
    write (exp (Var "x") (exp (Var "y") (Var "z")))  --> "x ^ y ^ z"
    write (mul (exp (Var "x") (Var "y")) (Var "z"))  --> "x ^ y * z"
    write (add (exp (Var "x") (Var "y")) (Var "z"))  --> "x ^ y + z"
    write (Fnc (exp (Var "x") (Var "y")) (Var "z")) --> "x ^ y -> z"
    write (TE (exp (Var "x") (Var "y")) (Var "z"))   --> "x ^ y : z"
    write (exp (Var "x") (Lam "y" (Var "z")))        --> "x ^ (λy. z)"

    write (mul (Var "x") (App (Var "y") (Var "z")))  --> "x * y z"
    write (mul (Var "x") (exp (Var "y") (Var "z")))  --> "x * y ^ z"
    write (mul (mul (Var "x") (Var "y")) (Var "z"))  --> "x * y * z"
    write (add (mul (Var "x") (Var "y")) (Var "z"))  --> "x * y + z"
    write (Fnc (mul (Var "x") (Var "y")) (Var "z")) --> "x * y -> z"
    write (TE (mul (Var "x") (Var "y")) (Var "z"))   --> "x * y : z"
    write (mul (Var "x") (Lam "y" (Var "z")))        --> "x * (λy. z)"

    write (add (Var "x") (App (Var "y") (Var "z")))  --> "x + y z"
    write (add (Var "x") (exp (Var "y") (Var "z")))  --> "x + y ^ z"
    write (add (Var "x") (mul (Var "y") (Var "z")))  --> "x + y * z"
    write (add (add (Var "x") (Var "y")) (Var "z"))  --> "x + y + z"
    write (Fnc (add (Var "x") (Var "y")) (Var "z")) --> "x + y -> z"
    write (TE (add (Var "x") (Var "y")) (Var "z"))   --> "x + y : z"
    write (add (Var "x") (Lam "y" (Var "z")))        --> "x + (λy. z)"

    write (Fnc (Var "x") (App (Var "y") (Var "z")))  --> "x -> y z"
    write (Fnc (Var "x") (exp (Var "y") (Var "z")))  --> "x -> y ^ z"
    write (Fnc (Var "x") (mul (Var "y") (Var "z")))  --> "x -> y * z"
    write (Fnc (Var "x") (add (Var "y") (Var "z")))  --> "x -> y + z"
    write (Fnc (Var "x") (Fnc (Var "y") (Var "z"))) --> "x -> y -> z"
    write (TE (Fnc (Var "x") (Var "y")) (Var "z"))   --> "x -> y : z"
    write (Fnc (Var "x") (Lam "y" (Var "z")))        --> "x -> (λy. z)"

    write (TE (Var "x") (App (Var "y") (Var "z")))  --> "x : y z"
    write (TE (Var "x") (exp (Var "y") (Var "z")))  --> "x : y ^ z"
    write (TE (Var "x") (mul (Var "y") (Var "z")))  --> "x : y * z"
    write (TE (Var "x") (add (Var "y") (Var "z")))  --> "x : y + z"
    write (TE (Var "x") (Fnc (Var "y") (Var "z"))) --> "x : y -> z"
    write (TE (TE (Var "x") (Var "y")) (Var "z"))   --> "x : y : z"
    write (TE (Var "x") (Lam "y" (Var "z")))        --> "x : (λy. z)"

    write (Lam "x" (App (Var "y") (Var "z")))  --> "λx. y z"
    write (Lam "x" (exp (Var "y") (Var "z")))  --> "λx. y ^ z"
    write (Lam "x" (mul (Var "y") (Var "z")))  --> "λx. y * z"
    write (Lam "x" (add (Var "y") (Var "z")))  --> "λx. y + z"
    write (Lam "x" (TE (Var "y") (Var "z")))   --> "λx. y : z"
    write (Lam "x" (Fnc (Var "y") (Var "z")))  --> "λx. y -> z"
    write (Lam "x" (Lam "y" (Var "z")))        --> "λx y. z"

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
        Any ->
            "@"

        Int k ->
            String.fromInt k

        Num k ->
            String.fromFloat k

        Var x ->
            x

        Fnc t1 t2 ->
            unop2 expr t1 ++ " -> " ++ unop1 expr t2

        Lam _ _ ->
            tupleMap
                (\xs y -> "λ" ++ String.join " " xs ++ ". " ++ write y)
                (asFunc expr)

        For _ _ ->
            tupleMap
                (\xs y -> "∀" ++ String.join " " xs ++ ". " ++ write y)
                (asForAll expr)

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

    Ok (letVar "x" (Var "y") (Var "z"))                --> read "(λx. z) y"
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
    define "x" (TE (Var "y") (For "a" (Var "a"))) newEnv |> .names
    --> Dict.insert "x" (Var "y", Var "a") newEnv.names

    define "x" (TE (Var "y") (Var "a")) newEnv |> .names
    --> Dict.insert "x" (Var "y", Var "a") newEnv.names

-}
define : String -> Expr -> Env a -> Env a
define name value env =
    case value of
        TE expr typ ->
            andThen
                (\t _ ->
                    { env | names = Dict.insert name ( expr, t ) env.names }
                )
                (map Tuple.first (evalType typ env))

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
    --> [ "T : Type = T"
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
        |> eval (funcType typeInputs (Var "Type"))
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
    newTypeVar newEnv |> .result --> Ok (Var "a")

    -- Goes alphabetically if a type is already defined.
    newTypeVar { newEnv | names = Dict.singleton "a" (Var "a", Var "Type") } |> .result --> Ok (Var "b")

    -- Names can have multiple letters when a-z are all used up.
    newTypeVar { newEnv | nameSeed = 1234 } |> .result --> Ok (Var "ast")

-}
newTypeVar : Env a -> Env Type
newTypeVar env =
    Dict.keys env.names
        |> Set.fromList
        |> newVarName env.nameSeed
        |> tupleMap (\t seed -> withValue (Var t) { env | nameSeed = seed })


{-| Evaluates an Expression.

    import Lambda

    def : String -> String -> Env a -> Env a
    def x y env =
        withResult (read y) env
            |> andThen (define x)
            |> withResult env.result

    evalExpr : String -> Result Error (String, String)
    evalExpr txt =
        newEnv
            |> def "x" "42"
            |> def "y" "y : Num"
            |> def "f" "f : Int -> Num"
            |> def "g" "g : ∀a. a -> a"
            |> withResult (read txt)
            |> andThen eval
            |> .result
            |> Result.map (Tuple.mapBoth write write)

    -- Anything / free types / type variables
    evalExpr "@" --> Ok ( "@", "Type" )

    -- Values
    evalExpr "42"   --> Ok ( "42", "Int" )
    evalExpr "3.14" --> Ok ( "3.14", "Num" )

    -- Variables
    evalExpr "x" --> Ok ( "42", "Int" )
    evalExpr "y" --> Ok ( "y", "Num" )
    evalExpr "z" --> Err (VariableNotFound (Var "z"))
    evalExpr "f" --> Ok ( "f", "Int -> Num" )
    evalExpr "g" --> Ok ( "g", "a -> a" )
    evalExpr "h" -- Err (VariableNotFound (Var "h"))

    -- Function types
    evalExpr "a -> 42"     --> Ok ( "a -> 42", "@ -> Int" )
    evalExpr "42 -> a"     --> Ok ( "42 -> a", "Int -> @" )
    evalExpr "a -> a"      --> Ok ( "a -> a", "@ -> @" )
    evalExpr "a -> b"      --> Ok ( "a -> b", "@ -> @" )
    evalExpr "a -> b -> c" --> Ok ( "a -> b -> c", "@ -> @ -> @" )

    -- Lambda abstraction
    evalExpr "λx. x" --> Ok ( "λx. x", "∀a. a -> a" )
    evalExpr "λx. y" --> Ok ( "λx. y", "∀a. a -> Num" )
    evalExpr "λy. x" --> Ok ( "λy. 42", "∀a. a -> Int" )
    evalExpr "λx. z" --> Err (VariableNotFound (Var "z"))

    -- Type abstraction
    evalExpr "∀x. x" --> Ok ( "x", "@" )
    evalExpr "∀x. y" --> Ok ( "y", "Num" )
    evalExpr "∀y. x" --> Ok ( "42", "Int" )
    evalExpr "∀x. x -> y" --> Ok ( "x -> y", "@ -> Num" )
    evalExpr "∀x. z" --> Err (VariableNotFound (Var "z"))

    -- Application
    evalExpr "f x" --> Ok ( "f 42", "Num" )
    evalExpr "f y" --> Err (TypeMismatch (Var "Num") (Var "Int"))
    evalExpr "g x" --> Ok ( "g 42", "Int" )
    evalExpr "g y" --> Ok ( "g y", "Num" )
    evalExpr "h z" --> Err (VariableNotFound (Var "h"))
    evalExpr "f z" --> Err (VariableNotFound (Var "z"))
    evalExpr "(λx. x) 42"    --> Ok ( "42", "Int" )
    evalExpr "(λx. 3.14) 42" --> Ok ( "3.14", "Num" )

    -- Typed Expression (lazy evaluation)
    evalExpr "x : Int" --> Ok ( "x", "Int" )
    evalExpr "x : a"   --> Ok ( "x", "a" )
    evalExpr "z : a"   --> Ok ( "z", "a" )

    -- Variable definitions (inferred by lowercase name)
    evalExpr "x := 42; x"   -- Ok ( "42", "Int" )
    evalExpr "x : a = y; x" -- Ok ( "y", "Num" )

    -- Named type definitions (inferred by uppercase name)
    -- Undefined names are defined as constructors.
    evalExpr "Maybe a := Just a | Nothing; Just 42" -- Ok ( "Just 42", "Maybe Int" )
    evalExpr "Maybe a := Just a | Nothing; Nothing" -- Ok ( "Nothing", "Maybe a" )

    -- Case application
    evalExpr "Bool := True | False; True 1 2" -- Ok ( "1", "Int" )
    evalExpr "Bool := True | False; False 1 2" -- Ok ( "2", "Int" )

-}
eval : Expr -> Env a -> Env ( Expr, Type )
eval expr env =
    -- TODO: make tests that actually ensure the Env semantics
    case expr of
        Any ->
            withValue ( Any, Var "Type" ) env

        Int _ ->
            withValue ( expr, Var "Int" ) env

        Num _ ->
            withValue ( expr, Var "Num" ) env

        Var x ->
            case Dict.get x env.names of
                Just ( value, typ ) ->
                    if value == Var x then
                        -- Tautology: (x : T) = (x : T)
                        withValue ( value, typ ) env

                    else
                        -- Lazy evaluation through typed Expressions
                        -- TODO: unify the result type with the definition type.
                        -- TODO: update the name definition to the result
                        eval value env

                Nothing ->
                    withError (VariableNotFound (Var x)) env

        Fnc t1 t2 ->
            -- a -> 42 ==> (∀a. a -> 42) : (Type -> Int)
            -- 42 -> a ==> (∀a. 42 -> a) : (Int -> Type)
            -- a -> b  ==> (∀a b. a -> b) : (Type -> Type)
            map2
                (\( x, xt ) ( y, yt ) ->
                    let
                        ( xVars, xDef ) =
                            asForAll x

                        ( yVars, yDef ) =
                            asForAll y
                    in
                    ( forAll (xVars ++ yVars) (Fnc xDef yDef), Fnc xt yt )
                )
                (evalType t1 env)
                (evalType t2)

        Lam x y ->
            -- (λx. x)  ==> (λx. x)  : (∀a. a -> a)
            -- (λx. 42) ==> (λx. 42) : (∀a. a -> Int)
            let
                xT =
                    Dict.keys env.names
                        |> Set.fromList
                        |> newVarName env.nameSeed
                        |> Tuple.first
            in
            env
                |> eval_ (For xT (Var xT))
                |> define x (TE (Var x) (Var xT))
                |> eval_ y
                |> map
                    (Tuple.mapBoth
                        (Lam x)
                        (\yT -> For xT (Fnc (Var xT) yT))
                    )

        For x y ->
            -- a      ==> (∀a. a) : *
            -- a -> a ==> (∀a. a -> a) : * -> *
            env
                |> define x (TE (Var x) Any)
                |> eval_ y

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

        TE e t ->
            evalType t env
                |> map Tuple.first
                |> map (Tuple.pair e)


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


{-| Evaluates a type expression.

    def : String -> String -> Env a -> Env a
    def x y env =
        withResult (read y) env
            |> andThen (define x)
            |> withResult env.result

    evalTyp : String -> Result Error (String, String)
    evalTyp txt =
        newEnv
            |> def "x" "42"
            |> def "y" "y : Num"
            |> def "f" "f : Int -> Num"
            |> def "g" "g : a -> a"
            |> withResult (read txt)
            |> andThen evalType
            |> .result
            |> Result.map (Tuple.mapBoth write write)

    -- Values
    evalTyp "42"   --> Ok ( "42", "Int" )
    evalTyp "3.14" --> Ok ( "3.14", "Num" )

    -- Variables
    evalTyp "x" --> Ok ( "42", "Int" )
    evalTyp "y" --> Ok ( "y", "Num" )
    evalTyp "z" --> Ok ( "z", "@" )

-}
evalType : Type -> Env a -> Env ( Type, Type )
evalType typ env =
    case typ of
        Var x ->
            case Dict.get x env.names of
                Just _ ->
                    eval typ env

                Nothing ->
                    eval (For x typ) env

        _ ->
            eval typ env


{-| Tries to unify two types or get a `TypeMismatch` error.

    import Lambda

    -- Named types
    unify (Var "Int") (Var "Int") newEnv |> .result --> Ok (Var "Int", Var "Type")
    unify (Var "Int") (Var "Num") newEnv |> .result --> Err (TypeMismatch (Var "Int") (Var "Num"))

    -- Type variables
    unify (Var "a") (Var "a") newEnv |> .result --> Ok (Var "a", Any)
    unify (Var "a") (Var "b") newEnv |> .result --> Ok (Var "a", Any)

    -- Named types with type variables
    unify (Var "Int") (Var "a") newEnv |> .result --> Ok (Var "Int", Var "Type")
    unify (Var "a") (Var "Int") newEnv |> .result --> Ok (Var "Int", Var "Type")

    -- Function types
    unify (Fnc (Var "Int") (Var "Num")) (Fnc (Var "Int") (Var "Num")) newEnv |> .result --> Ok (Fnc (Var "Int") (Var "Num"), Fnc (Var "Type") (Var "Type"))
    unify (Fnc (Var "Int") (Var "Num")) (Var "a") newEnv |> .result                     --> Ok (Fnc (Var "Int") (Var "Num"), Fnc (Var "Type") (Var "Type"))
    unify (Var "a") (Fnc (Var "Int") (Var "Num")) newEnv |> .result                     --> Ok (Fnc (Var "Int") (Var "Num"), Fnc (Var "Type") (Var "Type"))

    unify (Fnc (Var "Int") (Var "Num")) (Fnc (Var "a") (Var "b")) newEnv |> .result     --> Ok (Fnc (Var "Int") (Var "Num"), Fnc (Var "Type") (Var "Type"))
    unify (Fnc (Var "Int") (Var "Num")) (Fnc (Var "a") (Var "Num")) newEnv |> .result   --> Ok (Fnc (Var "Int") (Var "Num"), Fnc (Var "Type") (Var "Type"))
    unify (Fnc (Var "Int") (Var "Num")) (Fnc (Var "Int") (Var "b")) newEnv |> .result   --> Ok (Fnc (Var "Int") (Var "Num"), Fnc (Var "Type") (Var "Type"))
    unify (Fnc (Var "Int") (Var "Num")) (Fnc (Var "Num") (Var "b")) newEnv |> .result   --> Err (TypeMismatch (Var "Int") (Var "Num"))
    unify (Fnc (Var "Int") (Var "Num")) (Fnc (Var "a") (Var "Int")) newEnv |> .result   --> Err (TypeMismatch (Var "Num") (Var "Int"))

    unify (Fnc (Var "a") (Var "b")) (Fnc (Var "Int") (Var "Num")) newEnv |> .result     --> Ok (Fnc (Var "Int") (Var "Num"), Fnc (Var "Type") (Var "Type"))
    unify (Fnc (Var "a") (Var "Num")) (Fnc (Var "Int") (Var "Num")) newEnv |> .result   --> Ok (Fnc (Var "Int") (Var "Num"), Fnc (Var "Type") (Var "Type"))
    unify (Fnc (Var "Int") (Var "b")) (Fnc (Var "Int") (Var "Num")) newEnv |> .result   --> Ok (Fnc (Var "Int") (Var "Num"), Fnc (Var "Type") (Var "Type"))
    unify (Fnc (Var "Num") (Var "b")) (Fnc (Var "Int") (Var "Num")) newEnv |> .result   --> Err (TypeMismatch (Var "Num") (Var "Int"))
    unify (Fnc (Var "a") (Var "Int")) (Fnc (Var "Int") (Var "Num")) newEnv |> .result   --> Err (TypeMismatch (Var "Int") (Var "Num"))

    unify (Fnc (Var "a") (Var "b")) (Fnc (Var "c") (Var "Int")) newEnv |> .result     --> Ok (Fnc (Var "a") (Var "Int"), Fnc Any (Var "Type"))
    unify (Fnc (Var "c") (Var "Int")) (Fnc (Var "a") (Var "b")) newEnv |> .result     --> Ok (Fnc (Var "c") (Var "Int"), Fnc Any (Var "Type"))

-}
unify : Type -> Type -> Env a -> Env ( Type, Type )
unify type1 type2 env =
    andThen2
        (\typ1 typ2 env_ ->
            case ( typ1, typ2 ) of
                ( ( Fnc a1 b1, _ ), ( Fnc a2 b2, _ ) ) ->
                    map2 (\( a, aT ) ( b, bT ) -> ( Fnc a b, Fnc aT bT ))
                        (unify_ a1 a2 env_)
                        (unify_ b1 b2)

                ( ( t1, _ ), ( t2, Any ) ) ->
                    withValue typ1
                        { env_ | types = DisjointSet.union t1 t2 env_.types }

                ( ( t1, Any ), ( t2, _ ) ) ->
                    unify_ t2 t1 env_

                ( ( t1, _ ), ( t2, _ ) ) ->
                    if typ1 == typ2 then
                        withValue typ1 env

                    else
                        withError (TypeMismatch t1 t2) env
        )
        (evalType type1 env)
        (evalType type2)


unify_ : Type -> Type -> Env a -> Env ( Type, Type )
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
        , [ fromRight Fnc (text "->") ]
        , [ fromLeft TE (char ':') ]

        -- , [ fromLeft Or (text "|") ]
        , [ inbetween identity (char '(') (char ')')
          , term identity funcP
          , term identity forAllP
          , term identity letVarP
          , term Int int
          , term Num number
          , term Var identifierP
          , term (\_ -> Any) (char '@')
          ]
        ]


precedence : Expr -> Int
precedence expr =
    -- TODO: find a way to have only one definition for operator precedence
    --       both for readers/parsers and writers/formatters.
    case expr of
        -- TODO: adjust these 100s
        Any ->
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

        For _ _ ->
            0

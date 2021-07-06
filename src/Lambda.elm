module Lambda exposing
    ( Error(..)
    , Expr(..)
    , eval
    )

{-| A simple Lambda calculus implementation with type inference.

TODO: validate that this Elm issue works: <https://github.com/elm/compiler/issues/2186>

[Calculus of constructions](https://en.wikipedia.org/wiki/Calculus_of_constructions)

TODO: figure out how to have a "Formatter" type to write, like "Parser" to read
TODO: rename "Parser" to "Reader" and "Formatter" to "Writer" (?)

-}

import Parser


{-| A Lambda calculus Expression.

Closure calculus:
<https://www.youtube.com/watch?v=ogXlQf8lDD4>
<https://slides.yowconference.com/yowlambdajam2019/XuanyiChew-AnAlienLambdaCalculus.pdf>
<https://blog.chewxy.com/wp-content/uploads/personal/dissertation31482.pdf>

Bidirectional Type Checking
<https://youtu.be/utyBNDj7s2w>

An Introduction to Polymorphic Lambda Calculus with Subtyping
<http://soft.vub.ac.be/Publications/1994/vub-tinf-tr-94-01.pdf>

Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism
<https://www.cl.cam.ac.uk/~nk480/bidir.pdf>

-- map : (a -> b) -> Vec n a -> Vec n b
-- toStr : Int -> Str
-- xs : Vec 7 Int

-- map toStr xs -- map : (Int -> Str) -> Vec 7 Int -> Vec 7 Str

-- Dependent types examples: map and filter with Vec

-- type Maybe a = Just a | Nothing
-- Maybe : = λa. (a -> b) -> b -> b
-- Just : Maybe a = λa. λj n. j a
-- Nothing : Maybe a = λj n. n

-}
type Expr
    = Type --               Type        Type of types (Kind)
    | IntT --               Int         Integer type
    | NumT --               Num         Number type
    | Int Int --            42          Integer value
    | Num Float --          3.14        Number value
    | Var String --         x           Variable
    | Fun Expr Expr --      t1 -> t2    Function type
    | Lam String Expr --    λx. e       Lambda abstraction
    | App Expr Expr --      e1 e2       Application
    | Ann Expr Expr --      e : t       Type annotation


{-| An error from parsing or evaluation.
-}
type Error
    = SyntaxError Parser.Error
    | VariableNotFound String
    | CannotApply Expr Expr
    | TypeMismatch Expr Expr


{-|

    import Lambda exposing (Error(..), Expr(..), eval)

    -- Types
    eval Type --> Ok ( Type, Type )
    eval IntT --> Ok ( IntT, Type )
    eval NumT --> Ok ( NumT, Type )

    -- Values
    eval (Int 42)   --> Ok ( Int 42, IntT )
    eval (Num 3.14) --> Ok ( Num 3.14, NumT )

    -- Variable
    eval (Var "x") --> Err (VariableNotFound "x")

    -- Function type
    eval (Fun IntT Type) --> Ok ( Fun IntT Type, Fun Type Type )
    -- eval (Fun (Var "a") IntT) -- Ok ( Fun (Var "a") IntT, Fun (Var "a") Type )

    -- Lambda abstraction
    eval (Lam "x" Type)                            --> Ok ( Lam "x" Type, Fun (Var "x") Type )
    eval (Lam "x" IntT)                            --> Ok ( Lam "x" IntT, Fun (Var "x") Type )
    eval (Lam "x" (Int 1))                         --> Ok ( Lam "x" (Int 1), Fun (Var "x") IntT )
    -- eval (Lam "x" (Var "x"))                       -- Ok ( Lam "x" (Var "x"), Fun (Var "x") (Var "x") )
    -- eval (Lam "x" (Fun (Var "x") IntT))            -- Ok ( Lam "x" (Fun (Var "x") IntT), Fun (Var "x") (Fun (Var "x") Type) )
    -- eval (Lam "x" (Lam "y" (Var "x")))             -- Ok ( Lam "x" (Lam "y" (Var "x")), Fun (Var "x") (Fun (Var "y") (Var "x")) )
    eval (Lam "x" (App (Lam "y" (Int 1)) (Int 2))) --> Ok ( Lam "x" (Int 1), Fun (Var "x") IntT )

    -- Application
    eval (App Type (Int 1))                                        --> Err (CannotApply Type (Int 1))
    eval (App IntT (Int 1))                                        --> Err (CannotApply IntT (Int 1))
    eval (App (Int 0) (Int 1))                                     --> Err (CannotApply (Int 0) (Int 1))
    eval (App (Var "x") (Int 1))                                   --> Err (CannotApply (Var "x") (Int 1))
    eval (App (Fun IntT Type) (Int 1))                             --> Err (CannotApply (Fun IntT Type) (Int 1))
    eval (App (App (Lam "x" (Lam "y" (Var "x"))) (Int 1)) (Int 2)) --> Ok ( Int 1, IntT )

    -- Let closure
    eval (App (Lam "x" Type) (Int 1))                              --> Ok ( Type, Type )
    eval (App (Lam "x" IntT) (Int 1))                              --> Ok ( IntT, Type )
    eval (App (Lam "x" (Int 0)) (Int 1))                           --> Ok ( Int 0, IntT )
    eval (App (Lam "x" (Var "x")) (Int 1))                         --> Ok ( Int 1, IntT )
    -- eval (App (Lam "x" (Var "y")) (Int 1))                         -- Ok ( Var "y", Var "y" )
    eval (App (Lam "x" (Fun (Var "x") Type)) (Int 1))              --> Ok ( Fun (Int 1) Type, Fun IntT Type )
    eval (App (Lam "x" (Lam "y" (Var "x"))) (Int 1))               --> Ok ( Lam "y" (Int 1), Fun (Var "y") IntT )
    eval (App (Lam "x" (App (Lam "y" (Int 0)) Type)) (Int 1))      --> Ok ( Int 0, IntT )

    -- Type annotation
    eval (Ann Type Type) --> Ok ( Type, Type )
    eval (Ann IntT Type) --> Ok ( IntT, Type )
    eval (Ann Type IntT) --> Err (TypeMismatch Type IntT)
    eval (Ann (Int 1) IntT) --> Ok ( Int 1, IntT )
    eval (Ann (Int 1) (Int 2)) --> Err (TypeMismatch IntT (Int 2))
    -- TODO: Ann Var *
    eval (Ann )

-}
eval : Expr -> Result Error ( Expr, Expr )
eval expr =
    case expr of
        Type ->
            Ok ( Type, Type )

        IntT ->
            Ok ( IntT, Type )

        NumT ->
            Ok ( NumT, Type )

        Int n ->
            Ok ( Int n, IntT )

        Num n ->
            Ok ( Num n, NumT )

        Var x ->
            Err (VariableNotFound x)

        Fun e1 e2 ->
            Result.map2
                (\( t1, k1 ) ( t2, k2 ) -> ( Fun t1 t2, Fun k1 k2 ))
                (eval e1)
                (eval e2)

        Lam x e ->
            Result.map
                (\( ee, k ) -> ( Lam x ee, Fun (Var x) k ))
                (eval (App (Lam x e) (Var x)))

        -- x=ex; e1 -> e2
        -- ⇒  (x=ex; e1) (x=ex; e2)
        App (Lam x (Fun e1 e2)) ex ->
            eval (Fun (App (Lam x e1) ex) (App (Lam x e2) ex))

        -- x=ex; λy. e
        -- ⇒  λy. (x=ex; e)
        -- ⇒  λy. ((λx. e) ex)
        App (Lam x (Lam y e)) ex ->
            eval (Lam y (App (Lam x e) ex))

        -- x=ex; y=ey; e
        -- ⇒  y=(x=ex; ey); (x=ex; e)
        -- ⇒  y=((λx. ey) ex); ((λx. e) ex)
        -- ⇒  (λy. ((λx. e) ex)) ((λx. ey) ex)
        App (Lam x (App (Lam y e) ey)) ex ->
            Result.andThen
                (\( vy, _ ) -> eval (App (Lam y e) vy))
                (eval (App (Lam x ey) ex))

        App (Lam x e) ex ->
            if e == Var x then
                -- x=ex; x  ⇒  ex
                eval ex

            else
                -- x=ex; e  ⇒  e
                eval e

        App ((App _ _) as app) e2 ->
            Result.andThen
                (\( e1, _ ) -> eval (App e1 e2))
                (eval app)

        App e1 e2 ->
            Err (CannotApply e1 e2)

        Ann e t ->
            resultAndThen2
                (\( ee, et ) ( tt, _ ) ->
                    if et == tt then
                        Ok ( ee, tt )

                    else
                        Err (TypeMismatch et tt)
                )
                (eval e)
                (eval t)


resultAndThen2 : (a -> b -> Result err c) -> Result err a -> Result err b -> Result err c
resultAndThen2 f r1 r2 =
    Result.andThen (\x -> Result.andThen (f x) r2) r1

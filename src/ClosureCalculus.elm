module ClosureCalculus exposing (..)

import Dict exposing (Dict)


type alias Env =
    Dict String Expr


type Expr
    = Num Float --          3.14     Number value
    | Var String --         x        Variable
    | App Expr Expr --      e1 e2    Application
    | Lam String Expr --    λx. e    Lambda abstraction


type Error
    = UndefinedVar String
    | NotAFunction Expr


newEnv : Env
newEnv =
    Dict.empty


define : String -> Expr -> Env -> Env
define name expr env =
    Dict.insert name expr env


letVar : String -> Expr -> Expr -> Expr
letVar name value expr =
    App (Lam name expr) value


{-|

    -- ✅ Γ ⊢ 3.14  ⇒  3.14
    eval (Num 3.14) newEnv --> Ok (Num 3.14)

    -- ❌ Γ ⊢ x  ⇒  Undefined variable
    eval (Var "x") newEnv --> Err (UndefinedVar "x")

    -- ✅ x=1 ∈ Γ ⊢ x  ⇒  1
    eval (Var "x") (define "x" (Num 1) newEnv) --> Ok (Num 1)

    -- ✅ x=x ∈ Γ ⊢ x  ⇒  x
    eval (Var "x") (define "x" (Var "x") newEnv) --> Ok (Var "x")

    -- ✅ Γ ⊢ x=1; 2  ⇒  2
    eval (letVar "x" (Num 1) (Num 2)) newEnv --> Ok (Num 2)

    -- ✅ Γ ⊢ x=1; x  ⇒  1
    eval (letVar "x" (Num 1) (Var "x")) newEnv --> Ok (Num 1)

    -- ✅ Γ ⊢ x=1; y=2; x  ⇒  1
    eval (letVar "x" (Num 1) (letVar "y" (Num 2) (Var "x"))) newEnv --> Ok (Num 1)

    -- ✅ Γ ⊢ x=1; y=2; y  ⇒  2
    eval (letVar "x" (Num 1) (letVar "y" (Num 2) (Var "y"))) newEnv --> Ok (Num 2)

    -- ✅ Γ ⊢ x=1; y=x; y  ⇒  1
    eval (letVar "x" (Num 1) (letVar "y" (Var "x") (Var "y"))) newEnv --> Ok (Num 1)

    -- ✅ Γ ⊢ x=y; y=1; x  ⇒  1
    eval (letVar "x" (Var "y") (letVar "y" (Num 1) (Var "x"))) newEnv --> Ok (Num 1)

    -- ❌ Γ ⊢ x=y; x  ⇒  Undefined variable
    eval (letVar "x" (Var "y") (Var "x")) newEnv --> Err (UndefinedVar "y")

    -- ✅ y=1 ∈ Γ ⊢ x=y; x  ⇒  1
    eval (letVar "x" (Var "y") (Var "x")) (define "y" (Num 1) newEnv) --> Ok (Num 1)

    -- ✅ Γ ⊢ λx. 1  ⇒  λx. 1
    eval (Lam "x" (Num 1)) newEnv --> Ok (Lam "x" (Num 1))

    -- ✅ Γ ⊢ λx. x  ⇒  λx. x
    eval (Lam "x" (Var "x")) newEnv --> Ok (Lam "x" (Var "x"))

    -- ❌ Γ ⊢ λx. y  ⇒  Undefined variable
    eval (Lam "x" (Var "y")) newEnv --> Err (UndefinedVar "y")

    -- ✅ y=1 ∈ Γ ⊢ λx. y  ⇒  λx. 1
    eval (Lam "x" (letVar "y" (Num 1) (Var "y"))) (define "y" (Num 1) newEnv) --> Ok (Lam "x" (Num 1))

    -- ✅ Γ ⊢ λx. y=1; y  ⇒  λx. 1
    eval (Lam "x" (letVar "y" (Num 1) (Var "y"))) newEnv --> Ok (Lam "x" (Num 1))

    -- ✅ Γ ⊢ λx. λy. y  ⇒  λx. λy. y
    eval (Lam "x" (Lam "y" (Var "y"))) newEnv --> Ok (Lam "x" (Lam "y" (Var "y")))

    -- ✅ Γ ⊢ λx. λy. x  ⇒  λx. λy. x
    eval (Lam "x" (Lam "y" (Var "x"))) newEnv --> Ok (Lam "x" (Lam "y" (Var "x")))

    -- ✅ Γ ⊢ x=1; λx. x  ⇒  λx. x
    eval (letVar "x" (Num 1) (Lam "x" (Var "x"))) newEnv --> Ok (Lam "x" (Var "x"))

    -- ✅ Γ ⊢ x=1; λy. x  ⇒  λy. 1
    eval (letVar "x" (Num 1) (Lam "y" (Var "x"))) newEnv --> Ok (Lam "y" (Num 1))

    -- ❌ Γ ⊢ 1 2  ⇒  Not a function
    eval (App (Num 1) (Num 2)) newEnv --> Err (NotAFunction (Num 1))

    -- ❌ Γ ⊢ f 1  ⇒  Undefined variable
    eval (App (Var "f") (Num 1)) newEnv --> Err (UndefinedVar "f")

    -- ✅ f=λx. 1 ∈ Γ ⊢ f 2  ⇒  1
    eval (App (Var "f") (Num 2)) (define "f" (Lam "x" (Num 1)) newEnv) --> Ok (Num 1)

    -- ✅ Γ ⊢ f=λx. 1; f 2  ⇒  1
    eval (letVar "f" (Lam "x" (Num 1)) (App (Var "f") (Num 2))) newEnv --> Ok (Num 1)

    -- ✅ Γ ⊢ f=λx. f x; f  ⇒  λx. f x
    -- TODO: have a way to recognize `f` is a recursive function and get its definition
    eval (letVar "f" (Lam "x" (App (Var "f") (Var "x"))) (Var "f")) newEnv --> Ok (Lam "x" (App (Var "f") (Var "x")))

-}
eval : Expr -> Env -> Result Error Expr
eval expr env =
    case expr of
        Num k ->
            -- Γ ⊢ K  ⇒  K
            Ok (Num k)

        Var x ->
            case Dict.get x env of
                Just ex ->
                    if ex == Var x then
                        -- x=x ∈ Γ ⊢ x  ⇒  x
                        Ok (Var x)

                    else
                        -- x=ex ∈ Γ ⊢ x  ⇒  x=x ∈ Γ ⊢ ex
                        -- TODO: redefine x with a recursive definition, maybe a Fix combinator?
                        eval ex (define x (Var x) env)

                Nothing ->
                    -- Γ ⊢ x  ⇒  Undefined variable
                    Err (UndefinedVar x)

        App (Num k) _ ->
            -- Γ ⊢ K e  ⇒  Not a function
            Err (NotAFunction (Num k))

        App (Var x) e2 ->
            case Dict.get x env of
                Just e1 ->
                    if e1 == Var x then
                        -- x=x ∈ Γ ⊢ x e2  ⇒  x (x=x ∈ Γ ⊢ e2)
                        Result.map (App (Var x))
                            (eval e2 env)

                    else
                        -- x=e1 ∈ Γ ⊢ x e2  ⇒  x=e1 ∈ Γ ⊢ e1 e2
                        eval (App e1 e2) env

                Nothing ->
                    -- Γ ⊢ x e2  ⇒  Undefined variable
                    Err (UndefinedVar x)

        App (Lam x e) ex ->
            -- Γ ⊢ (λx. e) ex  ⇒  x=ex ∈ Γ ⊢ e
            eval e (define x ex env)

        App (App a b) e2 ->
            -- Γ ⊢ (a b) e2  ⇒  Γ ⊢ (Γ ⊢ a b) e2
            Result.andThen (\e1 -> eval (App e1 e2) env)
                (eval (App a b) env)

        Lam x e ->
            -- Γ ⊢ λx. e  ⇒  λx. (x=x ∈ Γ ⊢ e)
            Result.map (Lam x)
                (eval e (define x (Var x) env))

module L1_Untyped exposing (..)

import Dict exposing (Dict)


type alias Env =
    Dict String Expr


type Expr
    = Num Float --              3.14    Number value
    | Var String --             x       Variable
    | App Expr Expr --          e1 e2   Application
    | Lam String Expr --        λx. e   Lambda abstraction
    | Fix String Expr --        x@e     Fixed-point


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

    -- ✅ Γ ⊢ x@3.14  ⇒  3.14
    eval (Fix "x" (Num 3.14)) newEnv --> Ok (Num 3.14)

    -- ❌ Γ ⊢ x  ⇒  Undefined variable
    eval (Var "x") newEnv --> Err (UndefinedVar "x")

    -- ✅ Γ, x=1 ⊢ x  ⇒  1
    eval (Var "x") (define "x" (Num 1) newEnv) --> Ok (Num 1)

    -- ✅ Γ ⊢ x@x  ⇒  x@x
    eval (Fix "x" (Var "x")) newEnv --> Ok (Fix "x" (Var "x"))

    -- ❌ Γ ⊢ x@y  ⇒  Undefined variable
    eval (Fix "x" (Var "y")) newEnv --> Err (UndefinedVar "y")

    -- ✅ Γ, x=x@x ⊢ x  ⇒  x@x
    eval (Var "x") (define "x" (Fix "x" (Var "x")) newEnv) --> Ok (Fix "x" (Var "x"))

    -- ✅ Γ ⊢ x=1; 2  ⇒  2
    eval (letVar "x" (Num 1) (Num 2)) newEnv --> Ok (Num 2)

    -- ✅ Γ ⊢ x=x; x  ⇒  x@x
    eval (letVar "x" (Var "x") (Var "x")) newEnv --> Ok (Fix "x" (Var "x"))

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

    -- ✅ Γ, y=1 ⊢ x=y; x  ⇒  1
    eval (letVar "x" (Var "y") (Var "x")) (define "y" (Num 1) newEnv) --> Ok (Num 1)

    -- ✅ Γ ⊢ λx. 1  ⇒  λx. 1
    eval (Lam "x" (Num 1)) newEnv --> Ok (Lam "x" (Num 1))

    -- ✅ Γ ⊢ λx. x  ⇒  λx. x
    eval (Lam "x" (Var "x")) newEnv --> Ok (Lam "x" (Var "x"))

    -- ❌ Γ ⊢ λx. y  ⇒  Undefined variable
    eval (Lam "x" (Var "y")) newEnv --> Err (UndefinedVar "y")

    -- ✅ Γ, y=1 ⊢ λx. y  ⇒  λx. 1
    eval (Lam "x" (letVar "y" (Num 1) (Var "y"))) (define "y" (Num 1) newEnv) --> Ok (Lam "x" (Num 1))

    -- ✅ Γ ⊢ λx. y=1; y  ⇒  λx. 1
    eval (Lam "x" (letVar "y" (Num 1) (Var "y"))) newEnv --> Ok (Lam "x" (Num 1))

    -- ✅ Γ ⊢ λx. λy. y  ⇒  λx. λy. y
    eval (Lam "x" (Lam "y" (Var "y"))) newEnv --> Ok (Lam "x" (Lam "y" (Var "y")))

    -- ✅ Γ ⊢ λx. λy. x  ⇒  λx. λy. x
    eval (Lam "x" (Lam "y" (Var "x"))) newEnv --> Ok (Lam "x" (Lam "y" (Var "x")))

    -- ✅ Γ ⊢ λx. x@x  ⇒  λx. x
    eval (Lam "x" (Fix "x" (Var "x"))) newEnv --> Ok (Lam "x" (Var "x"))

    -- ✅ Γ ⊢ λx. y@y  ⇒  y@(λx. y)
    eval (Lam "x" (Fix "y" (Var "y"))) newEnv --> Ok (Fix "y" (Lam "x" (Var "y")))

    -- ✅ Γ ⊢ x=1; λx. x  ⇒  λx. x
    eval (letVar "x" (Num 1) (Lam "x" (Var "x"))) newEnv --> Ok (Lam "x" (Var "x"))

    -- ✅ Γ ⊢ x=1; λy. x  ⇒  λy. 1
    eval (letVar "x" (Num 1) (Lam "y" (Var "x"))) newEnv --> Ok (Lam "y" (Num 1))

    -- ❌ Γ ⊢ 1 2  ⇒  Not a function
    eval (App (Num 1) (Num 2)) newEnv --> Err (NotAFunction (Num 1))

    -- ❌ Γ ⊢ f 1  ⇒  Undefined variable
    eval (App (Var "f") (Num 1)) newEnv --> Err (UndefinedVar "f")

    -- ✅ Γ, f=λx. 1 ⊢ f 2  ⇒  1
    eval (App (Var "f") (Num 2)) (define "f" (Lam "x" (Num 1)) newEnv) --> Ok (Num 1)

    -- ✅ Γ ⊢ f=λx. 1; f 2  ⇒  1
    eval (letVar "f" (Lam "x" (Num 1)) (App (Var "f") (Num 2))) newEnv --> Ok (Num 1)

    -- ✅ Γ ⊢ f=λx. f 1; f  ⇒  f=λx. f 1; f
    eval (letVar "f" (Lam "x" (App (Var "f") (Num 1))) (Var "f")) newEnv --> Ok (letVar "f" (Lam "x" (App (Var "f") (Num 1))) (Var "f"))

    -- ---=== Recursive functions ===--- (TODO: merge these above)
    -- --
    -- -- ✅ Γ ⊢ f=λx. f 1; f  ⇒  f=λx. f 1; f
    -- eval (letVar "f" (Lam "x" (App (Var "f") (Num 1))) (Var "f")) newEnv -- Ok (letVar "f" (Lam "x" (App (Var "f") (Num 1))) (Var "f"))

    -- -- ✅ Γ, g=λy. 1 ⊢ f=λx. g 2; f  ⇒  λx. 1
    -- eval (letVar "f" (Lam "x" (App (Var "g") (Num 2))) (Var "f")) (define "g" (Lam "y" (Num 1)) newEnv) -- Ok (Lam "x" (Num 1))

    -- -- ✅ Γ ⊢ f=λx. (f=λx. f 1; f) 2; f  ⇒  λx. 1
    -- eval (letVar "f" (Lam "x" (App (Var "g") (Num 2))) (Var "f")) (define "g" (Lam "y" (Num 1)) newEnv) -- Ok (Lam "x" (Num 1))

    -- TODO: define a recursive function with more than one argument
    -- TODO: call a recursive function with one argument
    -- TODO: call a recursive function with more than one argument

    -- -- ✅ Γ ⊢ f=λx. λy. f 1 2; f  ⇒  f=λx. λy. f 1 2; f
    -- eval (letVar "f" (Lam "x" (Lam "y" (App (App (Var "f") (Num 1)) (Num 2)))) (Var "f")) newEnv -- Ok (letVar "f" (Lam "x" (Lam "y" (App (App (Var "f") (Num 1)) (Num 2)))) (Var "f"))

    -- @f = (λx. x x) (λx. f (x x))
    -- factorial n = if n == 0 then 1 else n * factorial (n - 1)
    -- factorial = \n. (n == 0) 1 (n * factorial (n - 1))

-}
eval : Expr -> Env -> Result Error Expr
eval expr env =
    case expr of
        Num k ->
            -- Γ ⊢ K  ⇒  K
            Ok (Num k)

        Fix _ (Num k) ->
            -- Γ ⊢ x@K  ⇒  K
            Ok (Num k)

        Var x ->
            case Dict.get x env of
                Just ex ->
                    -- Γ, x=ex ⊢ x  ⇒  Γ, x=x@ex ⊢ ex
                    eval ex (define x (Fix x ex) env)

                Nothing ->
                    -- Γ ⊢ x  ⇒  Undefined variable
                    Err (UndefinedVar x)

        Fix x (Var y) ->
            if x == y then
                -- Γ ⊢ x@x  ⇒  x@x
                Ok (Fix x (Var x))

            else
                -- Γ ⊢ x@y  ⇒  Γ ⊢ y
                eval (Var y) env

        App (Num k) _ ->
            -- Γ ⊢ K e  ⇒  Not a function
            Err (NotAFunction (Num k))

        App (Var x) e2 ->
            case eval (Var x) env of
                Ok e1 ->
                    -- Γ ⊢ x e2  ⇒  Γ ⊢ (Γ ⊢ x) e2
                    eval (App e1 e2) env

                Err err ->
                    Err err

        App (Fix x e) e2 ->
            Debug.todo (Debug.toString expr)

        App e1 (Fix x e) ->
            Debug.todo (Debug.toString expr)

        App (App a b) e2 ->
            case eval (App a b) env of
                Ok e1 ->
                    -- Γ ⊢ (a b) e2  ⇒  Γ ⊢ (Γ ⊢ a b) e2
                    eval (App e1 e2) env

                Err err ->
                    Err err

        App (Lam x e) ex ->
            -- Γ ⊢ x=ex; e  ⇒  Γ, x=ex ⊢ e
            eval e (define x ex env)

        Fix x (App e1 e2) ->
            Debug.todo (Debug.toString expr)

        Lam x e ->
            case define x (Var x) env |> eval e of
                Ok (Fix y ey) ->
                    if x == y then
                        -- Γ ⊢ λx. (Γ, x=x ⊢ x@e)  ⇒  λx. (Γ, x=x ⊢ e)
                        Ok (Lam x ey)

                    else
                        -- Γ ⊢ λx. (Γ, x=x ⊢ y@e)  ⇒  y@(λx. (Γ, x=x ⊢ e))
                        Ok (Fix y (Lam x ey))

                Ok ex ->
                    -- Γ ⊢ λx. e  ⇒  λx. (Γ, x=x ⊢ e)
                    Ok (Lam x ex)

                Err err ->
                    Err err

        Fix x (Lam y e) ->
            Debug.todo (Debug.toString expr)

        Fix x (Fix y e) ->
            Debug.todo (Debug.toString expr)

module L0_Minimal exposing (..)


type Expr
    = Var String --         x       Variable
    | Lam String Expr --    λx. e   Lambda abstraction
    | App Expr Expr --      e1 e2   Application


letVar : String -> Expr -> Expr -> Expr
letVar x ex e =
    App (Lam x e) ex


{-|

    -- ✅ x  ⇒  x
    eval (Var "x") --> Var "x"

    -- ✅ λx. y  ⇒  λx. y
    eval (Lam "x" (Var "y")) --> Lam "x" (Var "y")

    -- ✅ f x  ⇒  f x
    eval (App (Var "f") (Var "x")) --> App (Var "f") (Var "x")

    -- ✅ (λx. z) y  ⇒  z
    eval (App (Lam "x" (Var "z")) (Var "y")) --> Var "z"

    -- ✅ x=y; z  ⇒  z
    eval (letVar "x" (Var "y") (Var "z")) --> Var "z"

    -- ✅ x=y; x  ⇒  y
    eval (letVar "x" (Var "y") (Var "x")) --> Var "y"

    -- ✅ x=y; f x  ⇒  f y
    eval (letVar "x" (Var "y") (App (Var "f") (Var "x"))) --> App (Var "f") (Var "y")

    -- ✅ x=f; x y  ⇒  f y
    eval (letVar "x" (Var "f") (App (Var "x") (Var "y"))) --> App (Var "f") (Var "y")

    -- ✅ x=y; λz. x  ⇒  λz. y
    eval (letVar "x" (Var "y") (Lam "z" (Var "x"))) --> Lam "z" (Var "y")

    -- ✅ x=a; y=b; x  ⇒  a
    eval (letVar "x" (Var "a") (letVar "y" (Var "b") (Var "x"))) --> Var "a"

    -- ✅ x=a; y=b; y  ⇒  b
    eval (letVar "x" (Var "a") (letVar "y" (Var "b") (Var "y"))) --> Var "b"

    -- ✅ x=a; y=x; y  ⇒  a
    eval (letVar "x" (Var "a") (letVar "y" (Var "x") (Var "y"))) --> Var "a"

-}
eval : Expr -> Expr
eval expr =
    case expr of
        Var x ->
            Var x

        Lam x e ->
            Lam x (eval e)

        App (Var x) e2 ->
            App (Var x) e2

        App (App ea eb) e2 ->
            eval (App (eval (App ea eb)) e2)

        App (Lam x (Var y)) ex ->
            if x == y then
                -- x=ex; x  ⇒  ex
                eval ex

            else
                -- x=ex; y  ⇒  y
                eval (Var y)

        -- x=ex; e1 e2  ⇒  (x=ex; e1) (x=ex; e2)
        App (Lam x (App e1 e2)) ex ->
            eval (App (eval (letVar x ex e1)) (eval (letVar x ex e2)))

        -- x=ex; λy. e  ⇒  λy. (x=ex; e)
        App (Lam x (Lam y e)) ex ->
            eval (Lam y (eval (letVar x ex e)))

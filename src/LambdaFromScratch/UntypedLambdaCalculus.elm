module LambdaFromScratch.UntypedLambdaCalculus exposing (..)


type Expr
    = Var String --         x       Variable
    | Lam String Expr --    λx. e   Lambda abstraction
    | App Expr Expr --      e1 e2   Application


letV : String -> Expr -> Expr -> Expr
letV x ex e =
    App (Lam x e) ex


{-|

    -- x  ⇒  x
    eval (Var "x") --> Var "x"

    -- λx. e  ⇒  λx. e
    eval (Lam "x" (Var "e")) --> Lam "x" (Var "e")

    -- e1 e2  ⇒  e1 e2
    eval (App (Var "e1") (Var "e2")) --> App (Var "e1") (Var "e2")

    -- x=ex; x  ⇒  ex
    eval (letV "x" (Var "ex") (Var "x")) --> Var "ex"

    -- x=ex; e  ⇒  e
    eval (letV "x" (Var "ex") (Var "e")) --> Var "e"

    -- x=ex; λy. x  ⇒  λy. ex
    eval (letV "x" (Var "ex") (Lam "y" (Var "x"))) --> Lam "y" (Var "ex")

    -- x=ex; e1 x  ⇒  e1 ex
    eval (letV "x" (Var "ex") (App (Var "e1") (Var "x"))) --> App (Var "e1") (Var "ex")

    -- x=ex; x e2  ⇒  ex e2
    eval (letV "x" (Var "ex") (App (Var "x") (Var "e2"))) --> App (Var "ex") (Var "e2")

    -- x=ex; y=ey; x  ⇒  ex
    eval (letV "x" (Var "ex") (letV "y" (Var "ey") (Var "x"))) -- Var "ex"

    -- x=ex; y=ey; y  ⇒  ey
    eval (letV "x" (Var "ex") (letV "y" (Var "ey") (Var "y"))) -- Var "ey"

    -- x=ex; y=x; y  ⇒  ex
    eval (letV "x" (Var "ex") (letV "y" (Var "x") (Var "y"))) -- Var "ex"

-}
eval : Expr -> Expr
eval expr =
    case expr of
        -- x  ⇒  x
        Var x ->
            Var x

        -- λx. e  ⇒  λx. e
        Lam x e ->
            Lam x (eval e)

        -- x=ex; λy. e  ⇒  λy. (x=ex; e)
        App (Lam x (Lam y e)) ex ->
            eval (Lam y (letV x ex e))

        -- x=ex; y=ey; e  ⇒  y=(x=ex; ey); (x=ex; e)
        App (Lam x (App (Lam y e) ey)) ex ->
            eval (letV y (letV x ex ey) (letV x ex e))

        -- x=ex; e1 e2  ⇒  (x=ex; e1) (x=ex; e2)
        App (Lam x (App e1 e2)) ex ->
            eval (App (letV x ex e1) (letV x ex e2))

        App (Lam x e) ex ->
            if e == Var x then
                -- x=ex; x  ⇒  ex
                eval ex

            else
                -- x=ex; e  ⇒  e
                eval e

        -- e1 e2  ⇒  e1 e2
        App e1 e2 ->
            App (eval e1) (eval e2)

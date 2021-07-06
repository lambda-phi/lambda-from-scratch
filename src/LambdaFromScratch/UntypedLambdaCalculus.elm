module LambdaFromScratch.UntypedLambdaCalculus exposing (..)


type Expr
    = Var String --         x       Variable
    | Lam String Expr --    λx. e   Lambda abstraction
    | App Expr Expr --      e1 e2   Application


{-|

    -- x  ⇒  x
    eval (Var "x") --> Var "x"

    -- λx. e  ⇒  λx. e
    eval (Lam "x" (Var "e")) --> Lam "x" (Var "e")

    -- e1 e2  ⇒  e1 e2
    eval (App (Var "e1") (Var "e2")) --> App (Var "e1") (Var "e2")

    -- x=ex; x  ⇒  ex
    eval (App (Lam "x" (Var "x")) (Var "ex")) --> Var "ex"

    -- x=ex; e  ⇒  e
    eval (App (Lam "x" (Var "e")) (Var "ex")) --> Var "e"

    -- x=ex; λy. x  ⇒  λy. ex
    eval (App (Lam "x" (Lam "y" (Var "x"))) (Var "ex")) --> Lam "y" (Var "ex")

    -- x=ex; e1 x  ⇒  e1 ex
    eval (App (Lam "x" (App (Var "e1") (Var "x"))) (Var "ex")) --> App (Var "e1") (Var "ex")

    -- x=ex; x e2  ⇒  ex e2
    eval (App (Lam "x" (App (Var "x") (Var "e2"))) (Var "ex")) --> App (Var "ex") (Var "e2")

    -- x=ex; y=ey; x  ⇒  ex
    eval (App (Lam "x" (App (Lam "y" (Var "x")) (Var "ey"))) (Var "ex")) -- Var "ex"

    -- x=ex; y=ey; y  ⇒  ey
    eval (App (Lam "x" (App (Lam "y" (Var "y")) (Var "ey"))) (Var "ex")) -- Var "ey"

    -- x=ex; y=x; y  ⇒  ex
    eval (App (Lam "x" (App (Lam "y" (Var "x")) (Var "ey"))) (Var "ex")) -- Var "ex"

-}
eval : Expr -> Expr
eval expr =
    case expr of
        -- x
        -- ⇒  x
        Var x ->
            Var x

        -- λx. e
        -- ⇒  λx. e
        Lam x e ->
            Lam x (eval e)

        -- x=ex; λy. e
        -- ⇒  (λx. λy. e) ex
        -- ⇒  λy. (x=ex; e)
        -- ⇒  λy. ((λx. e) ex)
        App (Lam x (Lam y e)) ex ->
            eval (Lam y (App (Lam x e) ex))

        -- x=ex; y=ey; e
        -- ⇒  (λx. (λy. e) ey) ex
        -- ⇒  y=(x=ex; ey); (x=ex; e)
        -- ⇒  y=((λx. ey) ex); ((λx. e) ex)
        -- ⇒  (λy. ((λx. e) ex)) ((λx. ey) ex)
        App (Lam x (App (Lam y e) ey)) ex ->
            eval (App (Lam y (App (Lam x e) ex)) (App (Lam x ey) ex))

        -- x=ex; e1 e2
        -- ⇒  (λx. e1 e2) ex
        -- ⇒  (x=ex; e1) (x=ex; e2)
        -- ⇒  ((λx. e1) ex) ((λx. e2) ex)
        App (Lam x (App e1 e2)) ex ->
            eval (App (App (Lam x e1) ex) (App (Lam x e2) ex))

        App (Lam x e) ex ->
            if e == Var x then
                -- x=ex; x
                -- ⇒  ex
                eval ex

            else
                -- x=ex; e
                -- ⇒  e
                eval e

        -- e1 e2
        -- ⇒  e1 e2
        App e1 e2 ->
            App (eval e1) (eval e2)

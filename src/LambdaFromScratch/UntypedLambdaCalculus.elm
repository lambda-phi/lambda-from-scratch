module LambdaFromScratch.UntypedLambdaCalculus exposing (..)


type Expr
    = Int Int --            42      Integer value
    | Num Float --          3.14    Number value
    | Var String --         x       Variable
    | Lam String Expr --    λx. e   Lambda abstraction
    | App Expr Expr --      e1 e2   Application


letVar : String -> Expr -> Expr -> Expr
letVar x ex e =
    App (Lam x e) ex


{-|

    -- ✅ 1  ⇒  1
    eval (Int 1) --> Int 1

    -- ✅ 3.14  ⇒  3.14
    eval (Num 3.14) --> Num 3.14

    -- ✅ x  ⇒  x
    eval (Var "x") --> Var "x"

    -- ✅ λx. 1  ⇒  λx. 1
    eval (Lam "x" (Int 1)) --> Lam "x" (Int 1)

    -- ✅ λx. x  ⇒  λx. x
    eval (Lam "x" (Var "x")) --> Lam "x" (Var "x")

    -- ✅ λx. y  ⇒  λx. y
    eval (Lam "x" (Var "y")) --> Lam "x" (Var "y")

    -- ✅ 1 2  ⇒  1 2
    eval (App (Int 1) (Int 2)) --> App (Int 1) (Int 2)

    -- ✅ x=1; 2  ⇒  2
    eval (letVar "x" (Int 1) (Int 2)) --> Int 2

    -- ✅ x=1; x  ⇒  1
    eval (letVar "x" (Int 1) (Var "x")) --> Int 1

    -- ✅ x=1; y  ⇒  y
    eval (letVar "x" (Int 1) (Var "y")) --> Var "y"

    -- ✅ x=1; λy. x  ⇒  λy. 1
    eval (letVar "x" (Int 1) (Lam "y" (Var "x"))) --> Lam "y" (Int 1)

    -- ✅ x=1; x 2  ⇒  1 2
    eval (letVar "x" (Int 1) (App (Var "x") (Int 2))) --> App (Int 1) (Int 2)

    -- ✅ x=1; y=2; x  ⇒  1
    eval (letVar "x" (Int 1) (letVar "y" (Int 2) (Var "x"))) --> Int 1

    -- ✅ x=1; y=2; y  ⇒  2
    eval (letVar "x" (Int 1) (letVar "y" (Int 2) (Var "y"))) --> Int 2

    -- ✅ x=1; y=x; y  ⇒  1
    eval (letVar "x" (Int 1) (letVar "y" (Var "x") (Var "y"))) --> Int 1

    -- ✅ ((λx. x) (λy. 1)) 2  ⇒  1
    eval (App (App (Lam "x" (Var "x")) (Lam "y" (Int 1))) (Int 2)) --> Int 1

    -- ✅ (λx. 1) (y=2; y)  ⇒  1
    eval (App (Lam "x" (Int 1)) (letVar "y" (Int 2) (Var "y"))) --> Int 1

-}
eval : Expr -> Expr
eval expr =
    case expr of
        Int i ->
            Int i

        Num n ->
            Num n

        Var x ->
            Var x

        -- λx. e  ⇒  λx. e
        Lam x e ->
            Lam x (eval e)

        -- i e2  ⇒  i e2
        App (Int i) e2 ->
            App (Int i) e2

        -- n e2  ⇒  n e2
        App (Num n) e2 ->
            App (Num n) e2

        -- x e2  ⇒  x e2
        App (Var x) e2 ->
            App (Var x) e2

        -- x=ex; λy. e  ⇒  λy. (x=ex; e)
        App (Lam x (Lam y e)) ex ->
            eval (Lam y (letVar x ex e))

        -- x=ex; e1 e2  ⇒  (x=ex; e1) (x=ex; e2)
        App (Lam x (App e1 e2)) ex ->
            eval (App (eval (letVar x ex e1)) (eval (letVar x ex e2)))

        App (Lam x e) ex ->
            if e == Var x then
                -- x=ex; x  ⇒  ex
                eval ex

            else
                -- x=ex; e  ⇒  e
                eval e

        -- (e1 e2) e3  ⇒  (e1 e2) e3
        App (App e1 e2) e3 ->
            eval (App (eval (App e1 e2)) e3)

module L1_Untyped exposing (..)

import Dict exposing (Dict)


type alias Env =
    Dict String Expr


type Expr
    = Num Float --              3.14        Number value
    | Var String --             x           Variable
    | App Expr Expr --          e1 e2       Application
    | Lam String Expr --        λx. e       Lambda abstraction
    | Env Env Expr --           Γ e         Environment


type Error
    = UndefinedVar String
    | NotAFunction Expr


empty : Env
empty =
    Dict.empty


fromList : List ( String, Expr ) -> Env
fromList vars =
    Dict.fromList vars


get : String -> Env -> Maybe Expr
get x vars =
    Dict.get x vars


names : Env -> List String
names vars =
    Dict.keys vars


insert : String -> Expr -> Env -> Env
insert x e env =
    Dict.insert x e env


remove : String -> Env -> Env
remove x env =
    Dict.remove x env


merge : Env -> Env -> Env
merge vars1 vars2 =
    Dict.union vars1 vars2


map : (String -> Expr -> Result Error Expr) -> Env -> Result Error Env
map f vars =
    Dict.foldl (\x ex -> Result.map2 (insert x) (f x ex))
        (Ok Dict.empty)
        vars


letVar : String -> Expr -> Expr -> Expr
letVar x ex e =
    App (Lam x e) ex


{-|

    -- ✅ 3.14  ⇒  3.14
    eval (Num 3.14) empty --> Ok (Num 3.14)

    -- ❌ x  ⇒  Undefined variable
    eval (Var "x") empty --> Err (UndefinedVar "x")

    -- ❌ y ~ {x=1}  ⇒  Undefined variable
    eval (Var "y") (fromList [ ( "x", Num 1 ) ]) --> Err (UndefinedVar "y")

    -- ✅ x ~ {x=1}  ⇒  1
    eval (Var "x") (fromList [ ( "x", Num 1 ) ]) --> Ok (Num 1)

    -- ✅ x ~ {x=y, y=1}  ⇒  1
    eval (Var "x") (fromList [ ( "x", Var "y" ), ( "y", Num 1 ) ]) --> Ok (Num 1)

    -- ✅ x ~ {x=x}  ⇒  {x=x} x
    eval (Var "x") (fromList [ ( "x", Var "x" ) ]) --> Ok (Env (fromList [("x", Var "x")]) (Var "x"))

    -- ❌ 1 2  ⇒  Not a function
    eval (App (Num 1) (Num 2)) empty --> Err (NotAFunction (Num 1))

    -- ❌ x 1  ⇒  Undefined variable
    eval (App (Var "x") (Num 1)) empty --> Err (UndefinedVar "x")

    -- ✅ (λx. x) 1  ⇒  1
    eval (App (Lam "x" (Var "x")) (Num 1)) empty --> Ok (Num 1)

    -- ❌ (λx. y) 1  ⇒  Undefined variable
    eval (App (Lam "x" (Var "y")) (Num 1)) empty --> Err (UndefinedVar "y")

    -- ✅ (λx. y) 2 ~ {y=1}  ⇒  1
    eval (App (Lam "x" (Var "y")) (Num 2)) (fromList [ ( "y", Num 1 ) ]) --> Ok (Num 1)

    -- ✅ (λx. x) 2 ~ {x=1}  ⇒  2
    eval (App (Lam "x" (Var "x")) (Num 2)) (fromList [ ( "x", Num 1 ) ]) --> Ok (Num 2)

    -- ✅ ({f=f} f) 1  ⇒  {f=f} (f 1)
    eval (App (Env (fromList [ ( "f", Var "f" ) ]) (Var "f")) (Num 1)) empty --> Ok (Env (fromList [("f", Var "f")]) (App (Var "f") (Num 1)))

    -- ✅ ({f=f} f) 1 ~ {x=2}  ⇒  {f=f} (f 1)
    eval (App (Env (fromList [ ( "f", Var "f" ) ]) (Var "f")) (Num 1)) (fromList [ ( "x", Num 1 ) ]) --> Ok (Env (fromList [("f", Var "f")]) (App (Var "f") (Num 1)))

    -- ✅ ({f=f} f) ({x=x} x)  ⇒  {f=f, x=x} (f x)
    eval (App (Env (fromList [ ( "f", Var "f" ) ]) (Var "f")) (Env (fromList [ ( "x", Var "x" ) ]) (Var "x"))) empty --> Ok (Env (fromList [("f", Var "f"), ("x", Var "x")]) (App (Var "f") (Var "x")))

    -- ✅ ({f=f} f) ({x=x} x) ~ {y=1}  ⇒  {f=f, x=x} (f x)
    eval (App (Env (fromList [ ( "f", Var "f" ) ]) (Var "f")) (Env (fromList [ ( "x", Var "x" ) ]) (Var "x"))) (fromList [ ( "y", Num 1 ) ]) --> Ok (Env (fromList [("f", Var "f"), ("x", Var "x")]) (App (Var "f") (Var "x")))

    -- ✅ λx. 1  ⇒  λx. 1
    eval (Lam "x" (Num 1)) empty --> Ok (Lam "x" (Num 1))

    -- ✅ λx. {x=x} x  ⇒  λx. x
    eval (Lam "x" (Env (fromList [ ( "x", Var "x" ) ]) (Var "x"))) empty --> Ok (Lam "x" (Var "x"))

    -- ✅ λx. x  ⇒  λx. x
    eval (Lam "x" (Var "x")) empty --> Ok (Lam "x" (Var "x"))

    -- ❌ λx. y  ⇒  Undefined variable
    eval (Lam "x" (Var "y")) empty --> Err (UndefinedVar "y")

    -- ✅ {y=1} (λx. y)  ⇒  λx. 1
    eval (Env (fromList [ ( "y", Num 1 ) ]) (Lam "x" (Var "y"))) empty --> Ok (Lam "x" (Num 1))

    -- ✅ λx. λy. x  ⇒  λx. λy. x
    eval (Lam "x" (Lam "y" (Var "x"))) empty --> Ok (Lam "x" (Lam "y" (Var "x")))

    -- ✅ λx. λy. y  ⇒  λx. λy. y
    eval (Lam "x" (Lam "y" (Var "y"))) empty --> Ok (Lam "x" (Lam "y" (Var "y")))

    -- ❌ λx. λy. z  ⇒  λx. λy. z
    eval (Lam "x" (Lam "y" (Var "z"))) empty --> Err (UndefinedVar "z")

    -- ✅ [x=y] x ~ [y=1]  ⇒  1
    eval (Env (fromList [ ( "x", Var "y" ) ]) (Var "x")) (fromList [ ( "y", Num 1 ) ]) --> Ok (Num 1)

    -- ✅ [x=1] x ~ [x=2]  ⇒  1
    eval (Env (fromList [ ( "x", Num 1 ) ]) (Var "x")) (fromList [ ( "x", Num 2 ) ]) --> Ok (Num 1)

    -- ✅ [x=y] ([y=1] x)  ⇒  1
    eval (Env (fromList [ ( "x", Var "y" ) ]) (Env (fromList [ ( "y", Num 1 ) ]) (Var "x"))) empty --> Ok (Num 1)

-}
eval : Expr -> Env -> Result Error Expr
eval expr env =
    case expr of
        Num k ->
            -- K ~ Γ  ⇒  K
            Ok (Num k)

        Var x ->
            case get x env of
                Just ex ->
                    if ex == Var x then
                        -- x ~ (x=x :: Γ)  ⇒  [x=x] x
                        Ok (Env (fromList [ ( x, Var x ) ]) (Var x))

                    else
                        -- x ~ (x=ex :: Γ) x  ⇒  ex ~ (x=ex :: Γ)
                        eval ex env

                Nothing ->
                    -- x ~ Γ  ⇒  Undefined variable
                    Err (UndefinedVar x)

        App (Num k) _ ->
            -- K e2 ~ Γ  ⇒  Not a function
            Err (NotAFunction (Num k))

        App (Lam x e) ex ->
            -- (λx. e) ex ~ Γ  ⇒  e ~ (x=ex :: Γ)
            eval e (insert x ex env)

        App (Env vars1 e1) (Env vars2 e2) ->
            -- (Γ1 e1) (Γ2 e2) ~ Γ  ⇒  (Γ1 ++ Γ2) (e1 e2)
            Ok (Env (merge vars1 vars2) (App e1 e2))

        App (Env vars e1) e2 ->
            -- (Γ1 e1) e2 ~ Γ  ⇒  Γ1 (e1 e2)
            Ok (Env vars (App e1 e2))

        App e1 e2 ->
            -- e1 e2 ~ Γ  ⇒  (e1 ~ Γ) (e2 ~ Γ) ~ []
            Result.map2 App (eval e1 env) (eval e2 env)
                |> Result.andThen (\e -> eval e empty)

        Lam x e ->
            case eval e (insert x (Var x) env) of
                Ok (Env vars ee) ->
                    if vars == Dict.singleton x (Var x) then
                        -- λx. ({x=x} e ~ (x=x :: Γ))  ⇒  λx. e
                        Ok (Lam x ee)

                    else
                        -- λx. ({x=ex :: Γ1} e ~ (x=x :: Γ2))  ⇒  (Γ1 - x) (λx. e)
                        Ok (Env (remove x vars) (Lam x ee))

                Ok ee ->
                    -- λx. (e ~ (x=x :: Γ))  ⇒  λx. e
                    Ok (Lam x ee)

                Err err ->
                    Err err

        Env vars e ->
            -- Γ e  ⇒  e ~ Γ
            eval e (merge vars env)

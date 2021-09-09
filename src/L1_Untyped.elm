module L1_Untyped exposing (..)

import Dict exposing (Dict)


type Expr
    = Num Float --                      3.14        Number value
    | Var String --                     x           Variable
    | Lam String Expr --                λx. e       Lambda abstraction
    | App Expr Expr --                  e1 e2       Application
    | Let (List ( String, Expr )) --    x=ex; y=ey  Let bindings


type Error
    = UndefinedVar String
    | NotAFunction Expr


type Result
    = Value Expr
    | Lazy Expr
    | Error Error


type alias Env =
    Dict String Result


empty : Env
empty =
    Dict.empty


{-|

    import Dict

    fromList []                             --> Dict.empty
    fromList [("x", Num 1)]                 --> Dict.fromList [("x", Value (Num 1))]
    fromList [("x", Var "y"), ("y", Num 1)] --> Dict.fromList [("x", Value (Num 1)), ("y", Value (Num 1))]
    fromList [("x", Num 1), ("y", Var "x")] --> Dict.fromList [("x", Value (Num 1)), ("y", Value (Num 1))]

-}
fromList : List ( String, Expr ) -> Env
fromList vars =
    Dict.map (\_ e -> eval (App (Let vars) e) empty)
        (Dict.fromList vars)


get : String -> Env -> Maybe Result
get x vars =
    Dict.get x vars



-- names : Env -> List String
-- names vars =
--     Dict.keys vars


insert : String -> Expr -> Env -> Env
insert x e env =
    Dict.insert x (eval e (Dict.insert x (Value (Var x)) env)) env



-- remove : String -> Env -> Env
-- remove x env =
--     Dict.remove x env
-- merge : Env -> Env -> Env
-- merge env1 env2 =
--     Dict.union env1 env2
-- map : (String -> Expr -> Result Error Expr) -> Env -> Result Error Env
-- map f vars =
--     Dict.foldl (\x ex -> Result.map2 (insert x) (f x ex))
--         (Ok Dict.empty)
--         vars
-- letVar : String -> Expr -> Expr -> Expr
-- letVar x ex e =
--     App (Lam x e) ex


{-|

    --== Number value ==--
    -- ✅ 3.14  ⇒  3.14
    eval (Num 3.14) empty --> Value (Num 3.14)

    --== Variable ==--
    -- ❌ x  ⇒  Undefined variable
    eval (Var "x") empty --> Error (UndefinedVar "x")

    -- ✅ x ~ {x=1}  ⇒  1
    eval (Var "x") (fromList [ ( "x", Num 1 ) ]) --> Value (Num 1)

    -- ✅ x ~ {x=x}  ⇒  x
    eval (Var "x") (fromList [ ( "x", Var "x" ) ]) --> Value (Var "x")

    --== Lambda abstraction ==--
    -- ✅ λx. 1  ⇒  λx. 1
    eval (Lam "x" (Num 1)) empty --> Value (Lam "x" (Num 1))

    -- -- ✅ λx. f ~ {f=@f}  ⇒  @(λx. f)
    -- eval (Lam "x" (Env (fromList [ ( "x", Var "x" ) ]) (Var "x"))) empty -- Value (Lam "x" (Var "x"))
    -- -- ✅ λx. x  ⇒  λx. x
    -- eval (Lam "x" (Var "x")) empty -- Ok (Lam "x" (Var "x"))
    -- -- ❌ λx. y  ⇒  Undefined variable
    -- eval (Lam "x" (Var "y")) empty -- Err (UndefinedVar "y")
    --== Application ==--
    -- ❌ 1 2  ⇒  Not a function
    eval (App (Num 1) (Num 2)) empty --> Error (NotAFunction (Num 1))

    -- ❌ x 1  ⇒  Undefined variable
    eval (App (Var "x") (Num 1)) empty --> Error (UndefinedVar "x")

    -- ✅ (λx. x) 1  ⇒  1
    eval (App (Lam "x" (Var "x")) (Num 1)) empty --> Value (Num 1)

    -- ❌ (λx. y) 1  ⇒  Undefined variable
    eval (App (Lam "x" (Var "y")) (Num 1)) empty --> Error (UndefinedVar "y")

    -- ✅ (λx. y) 2 ~ {y=1}  ⇒  1
    eval (App (Lam "x" (Var "y")) (Num 2)) (fromList [ ( "y", Num 1 ) ]) --> Value (Num 1)

    -- ✅ (λx. x) 2 ~ {x=1}  ⇒  2
    eval (App (Lam "x" (Var "x")) (Num 2)) (fromList [ ( "x", Num 1 ) ]) --> Value (Num 2)

    --== Lazy recursive function ==--
    -- ✅ f ~ {f=λx. f x)  ⇒  f=λx. f x; f
    eval (Var "f") (fromList [ ( "f", Lam "x" (App (Var "f") (Var "x")) ) ]) --> Lazy (App (Let [("f", Lam "x" (App (Var "f") (Var "x")))]) (Var "f"))

    -- ✅ f ~ {f=λx. f ((λy. 1) x))  ⇒  f=λx. f 1; f
    eval (Var "f") (fromList [ ( "f", Lam "x" (App (Var "f") (App (Lam "y" (Num 1)) (Var "x"))) ) ]) --> Lazy (App (Let [("f", Lam "x" (App (Var "f") (Num 1)))]) (Var "f"))

    -- -- ✅ ({f=f} f) 1  ⇒  {f=f} (f 1)
    -- eval (App (Env (fromList [ ( "f", Var "f" ) ]) (Var "f")) (Num 1)) empty -- Ok (Env (fromList [("f", Var "f")]) (App (Var "f") (Num 1)))
    -- -- ✅ ({f=f} f) 1 ~ {x=2}  ⇒  {f=f} (f 1)
    -- eval (App (Env (fromList [ ( "f", Var "f" ) ]) (Var "f")) (Num 1)) (fromList [ ( "x", Num 1 ) ]) -- Ok (Env (fromList [("f", Var "f")]) (App (Var "f") (Num 1)))
    -- -- ✅ ({f=f} f) ({x=x} x)  ⇒  {f=f, x=x} (f x)
    -- eval (App (Env (fromList [ ( "f", Var "f" ) ]) (Var "f")) (Env (fromList [ ( "x", Var "x" ) ]) (Var "x"))) empty -- Ok (Env (fromList [("f", Var "f"), ("x", Var "x")]) (App (Var "f") (Var "x")))
    -- -- ✅ ({f=f} f) ({x=x} x) ~ {y=1}  ⇒  {f=f, x=x} (f x)
    -- eval (App (Env (fromList [ ( "f", Var "f" ) ]) (Var "f")) (Env (fromList [ ( "x", Var "x" ) ]) (Var "x"))) (fromList [ ( "y", Num 1 ) ]) -- Ok (Env (fromList [("f", Var "f"), ("x", Var "x")]) (App (Var "f") (Var "x")))
    -- -- ✅ {y=1} (λx. y)  ⇒  λx. 1
    -- eval (Env (fromList [ ( "y", Num 1 ) ]) (Lam "x" (Var "y"))) empty -- Ok (Lam "x" (Num 1))
    -- -- ✅ λx. λy. x  ⇒  λx. λy. x
    -- eval (Lam "x" (Lam "y" (Var "x"))) empty -- Ok (Lam "x" (Lam "y" (Var "x")))
    -- -- ✅ λx. λy. y  ⇒  λx. λy. y
    -- eval (Lam "x" (Lam "y" (Var "y"))) empty -- Ok (Lam "x" (Lam "y" (Var "y")))
    -- -- ❌ λx. λy. z  ⇒  λx. λy. z
    -- eval (Lam "x" (Lam "y" (Var "z"))) empty -- Err (UndefinedVar "z")
    -- -- ✅ [x=y] x ~ [y=1]  ⇒  1
    -- eval (Env (fromList [ ( "x", Var "y" ) ]) (Var "x")) (fromList [ ( "y", Num 1 ) ]) -- Ok (Num 1)
    -- -- ✅ [x=1] x ~ [x=2]  ⇒  1
    -- eval (Env (fromList [ ( "x", Num 1 ) ]) (Var "x")) (fromList [ ( "x", Num 2 ) ]) -- Ok (Num 1)
    -- -- ✅ [x=y] ([y=1] x)  ⇒  1
    -- eval (Env (fromList [ ( "x", Var "y" ) ]) (Env (fromList [ ( "y", Num 1 ) ]) (Var "x"))) empty -- Ok (Num 1)
    -- ✅ x=1; x  ⇒  1
    eval (App (Let [ ( "x", Num 1 ) ]) (Var "x")) empty --> Value (Num 1)

-}
eval : Expr -> Env -> Result
eval expr env =
    case expr of
        Num k ->
            -- K ~ Γ  ⇒  K
            Value (Num k)

        Var x ->
            case get x env of
                Just (Value (Var y)) ->
                    if x == y then
                        Value (Var x)

                    else
                        eval (Var y) env

                Just (Value ex) ->
                    eval ex env

                Just (Lazy ex) ->
                    -- TODO: add test
                    Lazy (App (Let [ ( x, ex ) ]) (Var x))

                Just (Error err) ->
                    Error err

                Nothing ->
                    Error (UndefinedVar x)

        Lam x e ->
            case eval e (Dict.insert x (Value (Var x)) env) of
                Value ee ->
                    Value (Lam x ee)

                Lazy ee ->
                    -- TODO: add test
                    Lazy (Lam x ee)

                Error err ->
                    Error err

        App e1 e2 ->
            case eval e1 env of
                Value (Num k) ->
                    Error (NotAFunction (Num k))

                Value (Var x) ->
                    Lazy (App (Var x) e2)

                Value (Lam x e) ->
                    eval (App (Let [ ( x, e2 ) ]) e) env

                Value (Let []) ->
                    eval e2 env

                Value (Let (( x, ex ) :: vars)) ->
                    eval (App (Let vars) e2) (insert x ex env)

                Value e ->
                    Debug.todo ("App: " ++ Debug.toString (Value e))

                -- Lazy (App (Let (( y, ey ) :: vars)) (Var z)) ->
                --     Lazy (App (Var z) e2)
                Lazy e ->
                    -- TODO: add test
                    -- Debug.todo ("App: " ++ Debug.toString (Lazy e))
                    -- TODO: eval e2
                    Lazy (App e1 e2)

                Error err ->
                    Error err

        Let vars ->
            Value (Let vars)

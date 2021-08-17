module L1_Untyped exposing (..)

import Dict exposing (Dict)


type alias Env =
    Dict String Expr


type Expr
    = Num Float --              3.14        Number value
    | Var String --             x           Variable
    | App Expr Expr --          e1 e2       Application
    | Lam String Expr --        λx. e       Lambda abstraction
    | Env Env --                Γ,x=ex      Environment


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
    eval (Num 3.14) --> Ok (Num 3.14)

    -- ✅ Γ 3.14  ⇒  3.14
    eval (App (Env empty) (Num 3.14)) --> Ok (Num 3.14)

    -- ❌ x  ⇒  Undefined variable
    eval (Var "x") --> Err (UndefinedVar "x")

    -- ✅ [x=1] x  ⇒  1
    eval (App (Env (insert "x" (Num 1) empty)) (Var "x")) --> Ok (Num 1)

    -- ✅ [x=y, y=1] x  ⇒  1
    eval (App (Env (empty |> insert "x" (Var "y") |> insert "y" (Num 1))) (Var "x")) --> Ok (Num 1)

    -- ❌ [x=1] y  ⇒  Undefined variable
    eval (App (Env (insert "x" (Num 1) empty)) (Var "y")) --> Err (UndefinedVar "y")

    -- ✅ [x=x] x  ⇒  [x=x] x
    eval (App (Env (insert "x" (Var "x") empty)) (Var "x")) --> Ok (App (Env (insert "x" (Var "x") empty)) (Var "x"))

    -- ❌ 1 2  ⇒  Not a function
    eval (App (Num 1) (Num 2)) --> Err (NotAFunction (Num 1))

    -- ❌ x 1  ⇒  Undefined variable
    eval (App (Var "x") (Num 1)) --> Err (UndefinedVar "x")

    -- ✅ (λx. x) 1  ⇒  1
    eval (App (Lam "x" (Var "x")) (Num 1)) --> Ok (Num 1)

    -- ❌ (λx. y) 1  ⇒  Undefined variable
    eval (App (Lam "x" (Var "y")) (Num 1)) --> Err (UndefinedVar "y")

    -- ✅ [y=1] ((λx. y) 2)  ⇒  1
    eval (App (Env (insert "y" (Num 1) empty)) (App (Lam "x" (Var "y")) (Num 2))) --> Ok (Num 1)

    -- ✅ [x=y] [y=1]  ⇒  [x=y, y=1]
    eval (App (Env (insert "x" (Var "y") empty)) (Env (insert "y" (Num 1) empty))) --> Ok (Env (fromList [ ( "x", Var "y" ), ( "y", Num 1 ) ]))

    -- ✅ [x=1] [y=x]  ⇒  [x=1, y=x]
    eval (App (Env (insert "x" (Num 1) empty)) (Env (insert "y" (Var "x") empty))) --> Ok (Env (fromList [ ( "x", Num 1 ), ( "y", Var "x" ) ]))

    -- ✅ [x=y] ([y=1] x)  ⇒  1
    eval (App (Env (insert "x" (Var "y") empty)) (App (Env (insert "y" (Num 1) empty)) (Var "x"))) --> Ok (Num 1)

    -- ✅ [x=1] ([y=x] y)  ⇒  1
    eval (App (Env (insert "x" (Num 1) empty)) (App (Env (insert "y" (Var "x") empty)) (Var "y"))) --> Ok (Num 1)

    -- ✅ λx. 1  ⇒  λx. 1
    eval (Lam "x" (Num 1)) --> Ok (Lam "x" (Num 1))

    -- ✅ λx. x  ⇒  λx. x
    eval (Lam "x" (Var "x")) --> Ok (Lam "x" (Var "x"))

    -- ❌ λx. y  ⇒  Undefined variable
    eval (Lam "x" (Var "y")) --> Err (UndefinedVar "y")

    -- ✅ [y=1] λx. y  ⇒  λx. 1
    eval (App (Env (insert "y" (Num 1) empty)) (Lam "x" (Var "y"))) --> Ok (Lam "x" (Num 1))

    -- ✅ [x=1] λx. x  ⇒  λx. x
    eval (App (Env (insert "x" (Var "x") empty)) (Lam "x" (Var "x"))) --> Ok (Lam "x" (Var "x"))

    -- ✅ λx. λy. x  ⇒  λx. λy. x
    eval (Lam "x" (Lam "y" (Var "x"))) --> Ok (Lam "x" (Lam "y" (Var "x")))

    -- ✅ λx. λy. y  ⇒  λx. λy. y
    eval (Lam "x" (Lam "y" (Var "y"))) --> Ok (Lam "x" (Lam "y" (Var "y")))

    -- ❌ λx. λy. z  ⇒  λx. λy. z
    eval (Lam "x" (Lam "y" (Var "z"))) --> Err (UndefinedVar "z")

    -- ✅ ((λx. x) (λy. 1)) 2  ⇒  1
    eval (App (App (Lam "x" (Var "x")) (Lam "y" (Num 1))) (Num 2)) --> Ok (Num 1)

    -- ✅ ([f=λx. 1] f) 2  ⇒  1
    eval (App (App (Env (insert "f" (Lam "x" (Num 1)) empty)) (Var "f")) (Num 2)) --> Ok (Num 1)

    -- ✅ [f=λx. 1] (f 2)  ⇒  1
    eval (App (Env (insert "f" (Lam "x" (Num 1)) empty)) (App (Var "f") (Num 2))) --> Ok (Num 1)

    -- ✅ []  ⇒  []
    eval (Env empty) --> Ok (Env empty)

    -- ✅ [x=1]  ⇒  [x=1]
    eval (Env (fromList [ ( "x", Num 1 ) ])) --> Ok (Env (fromList [ ( "x", Num 1 ) ]))

    -- ❌ [x=y]  ⇒  Undefined variable
    eval (Env (fromList [ ( "x", Var "y" ) ])) --> Err (UndefinedVar "y")

    -- ✅ [x=y, y=1]  ⇒  [x=1, y=1]
    eval (Env (fromList [ ( "x", Var "y" ), ( "y", Num 1 ) ])) --> Ok (Env (fromList [ ( "x", Num 1 ), ( "y", Num 1 ) ]))

    -- ✅ [f=λx. 1] f  ⇒  λx. 1
    eval (App (Env (fromList [ ( "f", Lam "x" (Num 1) ) ])) (Var "f")) --> Ok (Lam "x" (Num 1))



    -- TODO: RECURSIVE FUNCTIONS

    -- ✅ [f=λx. f 1] f  ⇒  [f=λx. f 1] f
    eval (App (Env (fromList [ ( "f", Lam "x" (App (Var "f") (Num 1)) ) ])) (Var "f")) --> Ok (Lam "x" (Num 1))






    -- -- ✅ Γ ⊢ f=λx. f 1; f  ⇒  f=λx. f 1; f
    -- eval (letVar "f" (Lam "x" (App (Var "f") (Num 1))) (Var "f")) empty -- Ok (letVar "f" (Lam "x" (App (Var "f") (Num 1))) (Var "f"))

    -- ---=== Recursive functions ===--- (TODO: merge these above)
    -- --
    -- -- ✅ Γ ⊢ f=λx. f 1; f  ⇒  f=λx. f 1; f
    -- eval (letVar "f" (Lam "x" (App (Var "f") (Num 1))) (Var "f")) empty -- Ok (letVar "f" (Lam "x" (App (Var "f") (Num 1))) (Var "f"))

    -- -- ✅ Γ, g=λy. 1 ⊢ f=λx. g 2; f  ⇒  λx. 1
    -- eval (letVar "f" (Lam "x" (App (Var "g") (Num 2))) (Var "f")) (insert "g" (Lam "y" (Num 1)) empty) -- Ok (Lam "x" (Num 1))

    -- -- ✅ Γ ⊢ f=λx. (f=λx. f 1; f) 2; f  ⇒  λx. 1
    -- eval (letVar "f" (Lam "x" (App (Var "g") (Num 2))) (Var "f")) (insert "g" (Lam "y" (Num 1)) empty) -- Ok (Lam "x" (Num 1))

    -- TODO: insert a recursive function with more than one argument
    -- TODO: call a recursive function with one argument
    -- TODO: call a recursive function with more than one argument

    -- -- ✅ Γ ⊢ f=λx. λy. f 1 2; f  ⇒  f=λx. λy. f 1 2; f
    -- eval (letVar "f" (Lam "x" (Lam "y" (App (App (Var "f") (Num 1)) (Num 2)))) (Var "f")) empty -- Ok (letVar "f" (Lam "x" (Lam "y" (App (App (Var "f") (Num 1)) (Num 2)))) (Var "f"))

    -- @f = (λx. x x) (λx. f (x x))
    -- factorial n = if n == 0 then 1 else n * factorial (n - 1)
    -- factorial = \n. (n == 0) 1 (n * factorial (n - 1))

-}
eval : Expr -> Result Error Expr
eval expr =
    case expr of
        Num k ->
            -- K  ⇒  K
            Ok (Num k)

        App (Env _) (Num k) ->
            -- Γ K  ⇒  K
            Ok (Num k)

        Var x ->
            Err (UndefinedVar x)

        App (Env env) (Var x) ->
            case get x env of
                Just ex ->
                    if ex == Var x then
                        -- (x=x :: Γ) x  ⇒  [x=x] x
                        Ok (App (Env (fromList [ ( x, Var x ) ])) (Var x))

                    else
                        -- (x=ex :: Γ) x  ⇒  @( (x=ex :: Γ) ex )
                        eval (App (Env env) ex)

                Nothing ->
                    -- Γ x  ⇒  Undefined variable
                    Err (UndefinedVar x)

        App (Num k) _ ->
            -- K e2  ⇒  Not a function
            Err (NotAFunction (Num k))

        App (Var x) _ ->
            -- x e2  ⇒  Undefined variable
            Err (UndefinedVar x)

        App (Lam x e) ex ->
            -- (λx. e) ex  ⇒  @( [x=ex] e )
            eval (App (Env (insert x ex empty)) e)

        App (Env env) (App (Lam x e) ex) ->
            -- Γ ((λx. e) ex)  ⇒  @( (x=ex :: Γ) e )
            eval (App (Env (insert x ex env)) e)

        App (Env vars1) (Env vars2) ->
            -- Γ1 Γ2  ⇒  Γ1 ++ Γ2
            Ok (Env (merge vars1 vars2))

        App (Env env1) (App (Env env2) e) ->
            -- Γ1 (Γ2 e)  ⇒  @( (Γ1 ++ Γ2) e )
            eval (App (Env (merge env1 env2)) e)

        Lam x e ->
            case eval (App (Env (insert x (Var x) empty)) e) of
                Ok (App (Env env) xe) ->
                    if names env == [ x ] then
                        -- λx. @( [x=ex] e )  ⇒  λx. e
                        Ok (Lam x xe)

                    else
                        -- λx. @( (x=ex :: Γ) e )  ⇒  Γ (λx. e)
                        Ok (App (Env (remove x env)) (Lam x xe))

                Ok xe ->
                    -- λx. e  ⇒  λx. @( [x=x] e )
                    Ok (Lam x xe)

                Err err ->
                    Err err

        App (Env vars) (Lam x e) ->
            -- Γ (λx. e)  ⇒  @( λx. Γ e )
            eval (Lam x (App (Env vars) e))

        App (App a b) e2 ->
            -- (a b) e2  ⇒  @( @(a b) @e2 )
            Result.map2 App
                (eval (App a b))
                (eval e2)
                |> Result.andThen eval

        App (Env vars) (App e1 e2) ->
            -- Γ (e1 e2)  ⇒  @( @(Γ e1) @(Γ e2) )
            Result.map2 App
                (eval (App (Env vars) e1))
                (eval (App (Env vars) e2))
                |> Result.andThen eval

        Env vars ->
            -- TODO: define a rule, maybe simplify
            Result.map Env
                (map (\_ e -> eval (App (Env vars) e)) vars)

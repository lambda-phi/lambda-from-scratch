module ClosureCalculus exposing (..)


type alias Env =
    List ( String, Expr )


type Expr
    = Num Float --              3.14        Number value
    | Var String --             x           Variable
    | Lam Env String Expr --    λ[Γ]x. e    Lambda closure
    | App Expr Expr --          e1 e2       Application


type Error
    = UndefinedVar String Expr
    | NotAFunction Expr


{-|

    letVar "x" (Num 0) (Num 1) --> App (Lam [] "x" (Num 1)) (Num 0)

-}
letVar : String -> Expr -> Expr -> Expr
letVar x ex e =
    App (Lam [] x e) ex


{-|

    letVars [] (Num 0) --> Num 0

    letVars [ ( "x", Num 1 ) ] (Num 0)
    --> App (Lam [] "x" (Num 0)) (Num 1)

    letVars [ ( "x", Num 1 ), ( "y", Num 2 ) ] (Num 0)
    --> App (Lam [] "x" (App (Lam [] "y" (Num 0)) (Num 2))) (Num 1)

-}
letVars : List ( String, Expr ) -> Expr -> Expr
letVars vars e =
    List.foldr (\( x, ex ) -> letVar x ex) e vars


{-|

    -- ✅ 3.14  ⇒  3.14
    eval (Num 3.14) --> Ok (Num 3.14)

    -- ❌ x  ⇒  Undefined variable
    eval (Var "x") --> Err (UndefinedVar "x" (Var "x"))

    -- ✅ λ[]x. 1  ⇒  λ[]x. 1
    eval (Lam [] "x" (Num 1)) --> Ok (Lam [] "x" (Num 1))

    -- ✅ λ[]x. x  ⇒  λ[x=x]x. x
    eval (Lam [] "x" (Var "x")) --> Ok (Lam [ ( "x", Var "x" ) ] "x" (Var "x"))

    -- ❌ λ[]x. y  ⇒  Undefined variable
    eval (Lam [] "x" (Var "y")) --> Err (UndefinedVar "y" (Lam [] "x" (Var "y")))

    -- ✅ λ[]x. λ[]y. x  ⇒  λ[]x. λ[x=x]y. x
    eval (Lam [] "x" (Lam [] "y" (Var "x"))) --> Ok (Lam [] "x" (Lam [ ( "x", Var "x" ) ] "y" (Var "x")))

    -- ✅ λ[]x. λ[]y. y  ⇒  λ[]x. λ[y=y]y. y
    eval (Lam [] "x" (Lam [] "y" (Var "y"))) --> Ok (Lam [] "x" (Lam [ ( "y", Var "y" ) ] "y" (Var "y")))

    -- ✅ λ[]x. (y=1; y)  ⇒  λ[]x. 1
    eval (Lam [] "x" (App (Lam [] "y" (Var "y")) (Num 1))) --> Ok (Lam [] "x" (Num 1))

    -- ✅ λ[]x. (y=x; y)  ⇒  λ[x=x]x. x
    eval (Lam [] "x" (App (Lam [] "y" (Var "y")) (Var "x"))) --> Ok (Lam [ ( "x", Var "x" ) ] "x" (Var "x"))

    -- ✅ λ[a=1]x. 2  ⇒  λ[]y. 2
    eval (Lam [ ( "a", Num 1 ) ] "x" (Num 2)) --> Ok (Lam [] "x" (Num 2))

    -- ✅ λ[a=1]x. a  ⇒  λ[]x. 1
    eval (Lam [ ( "a", Num 1 ) ] "x" (Var "a")) --> Ok (Lam [] "x" (Num 1))

    -- ✅ λ[x=1]x. x  ⇒  λ[]x. 1
    eval (Lam [ ( "x", Num 1 ) ] "x" (Var "x")) --> Ok (Lam [] "x" (Num 1))

    -- ✅ λ[a=a]x. a  ⇒  λ[a=a]x. a
    eval (Lam [ ( "a", Var "a" ) ] "x" (Var "a")) --> Ok (Lam [ ( "a", Var "a" ) ] "x" (Var "a"))

    -- ✅ λ[a=a]x. λ[]y. a  ⇒  λ[]x. λ[a=a]y. a
    eval (Lam [ ( "a", Var "a" ) ] "x" (Lam [] "y" (Var "a"))) --> Ok (Lam [] "x" (Lam [ ( "a", Var "a" ) ] "y" (Var "a")))

    -- ✅ λ[a=a]x. λ[b=b]y. a  ⇒  λ[]x. λ[a=a]y. a
    eval (Lam [ ( "a", Var "a" ) ] "x" (Lam [ ( "b", Var "b" ) ] "y" (Var "a"))) --> Ok (Lam [] "x" (Lam [ ( "a", Var "a" ) ] "y" (Var "a")))

    -- ✅ λ[a=a]x. λ[b=b]y. b  ⇒  λ[]x. λ[b=b]y. b
    eval (Lam [ ( "a", Var "a" ) ] "x" (Lam [ ( "b", Var "b" ) ] "y" (Var "b"))) --> Ok (Lam [] "x" (Lam [ ( "b", Var "b" ) ] "y" (Var "b")))

    -- ✅ λ[a=1]x. (y=2; a)  ⇒  λ[]x. 1
    eval (Lam [ ( "a", Num 1 ) ] "x" (App (Lam [] "y" (Var "a")) (Num 2))) --> Ok (Lam [] "x" (Num 1))

    -- ❌ 1 2  ⇒  Not a function
    eval (App (Num 1) (Num 2)) --> Err (NotAFunction (Num 1))

    -- ❌ f 1  ⇒  Undefined variable
    eval (App (Var "f") (Num 1)) --> Err (UndefinedVar "f" (App (Var "f") (Num 1)))

    -- ✅ x=1; 2  ⇒  2
    eval (App (Lam [] "x" (Num 2)) (Num 1)) --> Ok (Num 2)

    -- ✅ x=1; x  ⇒  1
    eval (App (Lam [] "x" (Var "x")) (Num 1)) --> Ok (Num 1)

    -- ❌ x=1; y  ⇒  Undefined variable
    eval (App (Lam [] "x" (Var "y")) (Num 1)) --> Err (UndefinedVar "y" (App (Lam [] "x" (Var "y")) (Num 1)))

    -- ✅ x=x; x  ⇒  x
    eval (App (Lam [] "x" (Var "x")) (Var "x")) --> Ok (Var "x")

    -- ✅ x=1; λ[]y. x  ⇒  λ[]y. 1
    eval (App (Lam [] "x" (Lam [] "y" (Var "x"))) (Num 1)) --> Ok (Lam [] "y" (Num 1))

    -- ✅ f=λ[]x. x; f 1  ⇒  1
    eval (App (Lam [] "f" (App (Var "f") (Num 1))) (Lam [] "x" (Var "x"))) --> Ok (Num 1)

    -- ✅ x=1; y=2; x  ⇒  1
    eval (App (Lam [] "x" (App (Lam [] "y" (Var "x")) (Num 2))) (Num 1)) --> Ok (Num 1)

    -- ✅ x=1; y=2; y  ⇒  2
    eval (App (Lam [] "x" (App (Lam [] "y" (Var "y")) (Num 2))) (Num 1)) --> Ok (Num 2)

    -- ✅ x=1; y=x; y  ⇒  1
    eval (App (Lam [] "x" (App (Lam [] "y" (Var "y")) (Var "x"))) (Num 1)) --> Ok (Num 1)

    -- ✅ x=y; y=1; x  ⇒  1
    eval (App (Lam [] "x" (App (Lam [] "y" (Var "x")) (Num 1))) (Var "y")) --> Ok (Num 1)

    -- ✅ (λ[a=0]x. 2) 1  ⇒  2
    eval (App (Lam [ ( "a", Num 0 ) ] "x" (Num 2)) (Num 1)) --> Ok (Num 2)

    -- ✅ (λ[a=1]x. a) 2  ⇒  1
    eval (App (Lam [ ( "a", Num 1 ) ] "x" (Var "a")) (Num 2)) --> Ok (Num 1)

    -- ✅ (λ[a=1]x. λ[]y. a) 2  ⇒  λ[]y. 1
    eval (App (Lam [ ( "a", Num 1 ) ] "x" (Lam [] "y" (Var "a"))) (Num 2)) --> Ok (Lam [] "y" (Num 1))

    -- ✅ (λ[a=1]x. y=a; y) 2  ⇒  1
    eval (App (Lam [ ( "a", Num 1 ) ] "x" (App (Lam [] "y" (Var "y")) (Var "a"))) (Num 2)) --> Ok (Num 1)

    -- ✅ (λ[a=1]x. y=x; y) 2  ⇒  2
    eval (App (Lam [ ( "a", Num 1 ) ] "x" (App (Lam [] "y" (Var "y")) (Var "x"))) (Num 2)) --> Ok (Num 2)

    -- ✅ ((λ[]x. x) (λ[]y. y)) 1  ⇒  1
    eval (App (App (Lam [] "x" (Var "x")) (Lam [] "y" (Var "y"))) (Num 1)) --> Ok (Num 1)

-}
eval : Expr -> Result Error Expr
eval expr =
    case expr of
        Num k ->
            Ok (Num k)

        Var x ->
            Err (UndefinedVar x expr)

        Lam _ x (Num k) ->
            -- λ[Γ]x. k  ⇒  λ[]x. k
            Ok (Lam [] x (Num k))

        Lam [] x (Var y) ->
            if x == y then
                -- λ[]x. x  ⇒  λ[x=x]x. x
                Ok (Lam [ ( x, Var x ) ] x (Var x))

            else
                Err (UndefinedVar y expr)

        Lam [] x (Lam env2 y e) ->
            -- λ[]x. λ[Γ]y. e  ⇒  λ[]x. λ[Γ,x=x]y. e
            Result.map (Lam [] x)
                (eval (Lam (( x, Var x ) :: env2) y e))

        Lam [] x (App e1 e2) ->
            -- λ[]x. e1 e2  ⇒  λ[]x. (x=x; e1) (x=x; e2)
            Result.map2 App
                (eval (letVar x (Var x) e1))
                (eval (letVar x (Var x) e2))
                |> Result.andThen eval
                |> Result.map (Lam [] x)

        Lam (( a, ea ) :: env) x (Var y) ->
            if a == y then
                if ea == Var a then
                    -- λ[Γ,a=a]x. a  ⇒  λ[a=a]x. a
                    Ok (Lam [ ( a, Var a ) ] x (Var a))

                else
                    -- λ[Γ,a=ea]x. a  ⇒  λ[Γ]x. ea
                    eval (Lam env x ea)

            else
                -- λ[Γ,a=ea]x. y  ⇒  λ[Γ]x. y
                eval (Lam env x (Var y))

        Lam (( a, ea ) :: env) x e ->
            -- λ[Γ,a=ea]x. e  ⇒  λ[Γ]x. a=ea; e
            Result.map (Lam env x)
                (eval (letVar a ea e))
                |> Result.andThen eval

        App (Num k) _ ->
            Err (NotAFunction (Num k))

        App (Var x) _ ->
            Err (UndefinedVar x expr)

        App (Lam _ _ (Num k)) _ ->
            -- x=ex; K  ⇒  K
            Ok (Num k)

        App (Lam [] x (Var y)) ex ->
            if x == y then
                if ex == Var x then
                    -- x=x; x  ⇒  x
                    Ok (Var x)

                else
                    -- x=ex; x  ⇒  ex
                    eval ex

            else
                Err (UndefinedVar y expr)

        App (Lam [] x (Lam env y e)) ex ->
            -- x=ex; λ[Γ]y. e  ⇒  λ[Γ,x=ex]y. e
            eval (Lam (( x, ex ) :: env) y e)

        App (Lam (( a, ea ) :: env1) x (Lam env2 y e)) ex ->
            -- (λ[Γ1,a=ea]x. λ[Γ2]y. e) ex   ⇒  (λ[Γ1]x. λ[Γ2,a=ea]y. e) ex
            eval (App (Lam env1 x (Lam (( a, ea ) :: env2) y e)) ex)

        App (Lam [] x (App e1 e2)) ex ->
            -- x=ex; e1 e2  ⇒  (x=ex; e1) (x=ex; e2)
            Result.map2 App
                (eval (letVar x ex e1))
                (eval (letVar x ex e2))
                |> Result.andThen eval

        App (Lam (( a, ea ) :: env) x (App e1 e2)) ex ->
            -- (λ[Γ,a=ea]x. e1 e2) ex  ⇒  (λ[Γ]x. (a=ea; e1) (a=ea; e2)) ex
            eval (App (Lam env x (App (letVar a ea e1) (letVar a ea e2))) ex)

        App (Lam (( a, ea ) :: env) x e) ex ->
            -- (λ[Γ,a=ea]x. e) ex  ⇒  x=ex; (λ[Γ]a. e) ea)
            eval (letVar x ex (App (Lam env a e) ea))

        App (App ea eb) e2 ->
            -- (ea eb) e2  ⇒  (ea eb) e2
            Result.map2 App
                (eval (App ea eb))
                (eval e2)
                |> Result.andThen eval

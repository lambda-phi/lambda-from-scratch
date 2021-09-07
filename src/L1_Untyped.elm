module L1_Untyped exposing (..)

import Dict exposing (Dict)


type alias Env =
    Dict String Expr


type Expr
    = Num Float --              3.14        Number constant
    | Var String --             x           Variable
    | App Expr Expr --          e1 e2       Application
    | Lam String Expr --        λx. e       Lambda abstraction
    | Fix String Expr --        @f. e       Fixed point


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

    --== Number constant ==--
    -- ✅ 3.14  ⇒  3.14
    eval (Num 3.14) empty --> Ok (Num 3.14)

    --== Variable ==--
    -- ❌ x  ⇒  Undefined variable
    eval (Var "x") empty --> Err (UndefinedVar "x")

    -- ❌ y ~ {x=1}  ⇒  Undefined variable
    eval (Var "y") (fromList [ ( "x", Num 1 ) ]) --> Err (UndefinedVar "y")

    -- ✅ x ~ {x=1}  ⇒  1
    eval (Var "x") (fromList [ ( "x", Num 1 ) ]) --> Ok (Num 1)

    -- ✅ x ~ {x=y, y=1}  ⇒  1
    eval (Var "x") (fromList [ ( "x", Var "y" ), ( "y", Num 1 ) ]) --> Ok (Num 1)

    -- ✅ x ~ {x=x}  ⇒  x
    eval (Var "x") (fromList [ ( "x", Var "x" ) ]) --> Ok (Var "x")

    -- ✅ x ~ {x=@x. x}  ⇒  @x. x
    eval (Var "x") (fromList [ ( "x", Fix "x" (Var "x") ) ]) --> Ok (Fix "x" (Var "x"))

    -- ✅ x ~ {x=@y. y}  ⇒  @y. y
    eval (Var "x") (fromList [ ( "x", Fix "y" (Var "y") ) ]) --> Ok (Fix "y" (Var "y"))

    --== Application ==--
    -- ❌ 1 2  ⇒  Not a function
    eval (App (Num 1) (Num 2)) empty --> Err (NotAFunction (Num 1))

    -- ❌ f 1  ⇒  Undefined variable
    eval (App (Var "f") (Num 1)) empty --> Err (UndefinedVar "f")

    -- ✅ f 1 ~ {f=f}  ⇒  f 1
    eval (App (Var "f") (Num 1)) (fromList [ ( "f", Var "f" ) ]) --> Ok (App (Var "f") (Num 1))

    -- ✅ f 1 ~ {f=λx. x}  ⇒  1
    eval (App (Var "f") (Num 1)) (fromList [ ( "f", Lam "x" (Var "x") ) ]) --> Ok (Num 1)

    -- ✅ f 1 ~ {f=@f. f}  ⇒  @f. f 1
    eval (App (Var "f") (Num 1)) (fromList [ ( "f", Fix "f" (Var "f") ) ]) --> Ok (Fix "f" (App (Var "f") (Num 1)))

    -- ✅ (λx. x) 1  ⇒  1
    eval (App (Lam "x" (Var "x")) (Num 1)) empty --> Ok (Num 1)

    -- ❌ (λx. y) 1  ⇒  Undefined variable
    eval (App (Lam "x" (Var "y")) (Num 1)) empty --> Err (UndefinedVar "y")

    -- ✅ (λx. y) 2 ~ {y=1}  ⇒  1
    eval (App (Lam "x" (Var "y")) (Num 2)) (fromList [ ( "y", Num 1 ) ]) --> Ok (Num 1)

    -- ✅ (λx. x) 2 ~ {x=1}  ⇒  2
    eval (App (Lam "x" (Var "x")) (Num 2)) (fromList [ ( "x", Num 1 ) ]) --> Ok (Num 2)

    -- ✅ (@f. f) 1  ⇒  @f. f 1
    eval (App (Fix "f" (Var "f")) (Num 1)) empty --> Ok (Fix "f" (App (Var "f") (Num 1)))

    -- ✅ (@f. f) (@x. x)  ⇒  @f. f (@x. x)
    eval (App (Fix "f" (Var "f")) (Fix "x" (Var "x"))) empty --> Ok (Fix "f" (App (Var "f") (Fix "x" (Var "x"))))

    -- ✅ (@f. f) (@x. x)  ⇒  @f. f (@x. x)
    eval (App (Fix "f" (Var "f")) (Fix "x" (Var "x"))) empty --> Ok (Fix "f" (App (Var "f") (Fix "x" (Var "x"))))

    --== Lambda abstraction ==--
    -- ✅ λx. 1  ⇒  λx. 1
    eval (Lam "x" (Num 1)) empty --> Ok (Lam "x" (Num 1))

    -- ✅ λx. x  ⇒  λx. x
    eval (Lam "x" (Var "x")) empty --> Ok (Lam "x" (Var "x"))

    -- ❌ λx. y  ⇒  Undefined variable
    eval (Lam "x" (Var "y")) empty --> Err (UndefinedVar "y")

    -- ✅ λx. λy. x  ⇒  λx. λy. x
    eval (Lam "x" (Lam "y" (Var "x"))) empty --> Ok (Lam "x" (Lam "y" (Var "x")))

    -- ✅ λx. λy. y  ⇒  λx. λy. y
    eval (Lam "x" (Lam "y" (Var "y"))) empty --> Ok (Lam "x" (Lam "y" (Var "y")))

    -- ❌ λx. λy. z  ⇒  λx. λy. z
    eval (Lam "x" (Lam "y" (Var "z"))) empty --> Err (UndefinedVar "z")

    -- ✅ λx. @f. f x  ⇒  @f. λx. f x
    eval (Lam "x" (Fix "f" (App (Var "f") (Var "x")))) empty --> Ok (Fix "f" (Lam "x" (App (Var "f") (Var "x"))))

    --== Fixed point ==--
    -- ✅ @x. x  ⇒  @x. x
    eval (Fix "x" (Var "x")) empty --> Ok (Fix "x" (Var "x"))

    -- ✅ @x. x ~ {x=1}  ⇒  @x. x
    eval (Fix "x" (Var "x")) (fromList [ ( "x", Num 1 ) ]) --> Ok (Fix "x" (Var "x"))

    -- ✅ @x. y ~ {y=1}  ⇒  1
    eval (Fix "x" (Var "y")) (fromList [ ( "y", Num 1 ) ]) --> Ok (Num 1)

    -- ✅ @x. @y. @z. x  ⇒  @x. x
    eval (Fix "x" (Fix "y" (Fix "z" (Var "x")))) empty --> Ok (Fix "x" (Var "x"))

    -- ✅ @x. @y. @z. z  ⇒  @z. z
    eval (Fix "x" (Fix "y" (Fix "z" (Var "z")))) empty --> Ok (Fix "z" (Var "z"))

    -- ✅ (λf. f) (λx. f x)  ⇒  @f. λx. f x
    eval (App (Lam "f" (Var "f")) (Lam "x" (App (Var "f") (Var "x")))) empty --> Ok (Fix "f" (Lam "x" (App (Var "f") (Var "x"))))

    --== Built-in functions ==--
    -- ✅ (+) 1  ⇒  λb. $add 1 b
    eval (App (Var "+") (Num 1)) empty --> Ok (Lam "b" (Add (Num 1) (Var "b")))

    -- ✅ (+) 1 2  ⇒  3
    eval (App (App (Var "+") (Num 1)) (Num 2)) empty --> Ok (Num 3)



    --== Recursive functions ==--
    -- f = \n. (n == 0) 1 (n * f (n - 1))

    --== Mutually recursive functions ==--
    -- https://en.wikipedia.org/wiki/Hofstadter_sequence#Hofstadter_Female_and_Male_sequences
    -- f = \n. (n == 0) 1 (n - m (f (n - 1)))
    -- m = \n. (n == 0) 0 (n - f (m (n - 1)))

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
                        -- TODO: rule
                        Ok (Var x)

                    else
                        -- TODO: rule
                        eval ex (insert x (Fix x (Var x)) env)

                Nothing ->
                    -- x ~ Γ  ⇒  Undefined variable
                    Err (UndefinedVar x)

        App (Num k) _ ->
            -- K e2 ~ Γ  ⇒  Not a function
            Err (NotAFunction (Num k))

        App (Var x) e2 ->
            case get x env of
                Just e1 ->
                    if e1 == Var x then
                        -- TODO: rule
                        Result.map (App (Var x))
                            (eval e2 env)

                    else
                        -- TODO: rule
                        eval (App e1 e2) env

                Nothing ->
                    Err (UndefinedVar x)

        App (App a b) e2 ->
            -- (a b) e2 ~ Γ  ⇒  (a b ~ Γ) e2 ~ {}
            Result.andThen (\e1 -> eval (App e1 e2) env)
                (eval (App a b) env)

        App (Lam x e) ex ->
            -- (λx. e) ex ~ Γ  ⇒  e ~ (x=ex :: Γ)
            eval e (insert x ex env)

        App (Fix x e1) e2 ->
            case eval (Fix x e1) env of
                Ok (Fix y e) ->
                    -- TODO: rule
                    Ok (Fix y (App e e2))

                Ok e ->
                    eval (App e e2) env

                Err err ->
                    Err err

        Lam x e ->
            case eval e (insert x (Var x) env) of
                Ok (Fix y ee) ->
                    if x == y then
                        Ok (Lam x ee)

                    else
                        -- TODO: rule
                        Ok (Fix y (Lam x ee))

                Ok ee ->
                    -- λx. (e ~ (x=x :: Γ))  ⇒  λx. e
                    Ok (Lam x ee)

                Err err ->
                    Err err

        Fix x (Var y) ->
            if x == y then
                -- TODO: rule
                Ok (Fix x (Var x))

            else
                -- TODO: rule
                eval (Var y) env

        Fix x e ->
            -- TODO: rule
            eval e (insert x (Fix x (Var x)) env)

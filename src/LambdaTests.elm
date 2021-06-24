module LambdaTests exposing (..)

{-|

    import Lambda exposing (Error(..), Term(..), eval)

    -- Type
    eval Type --> Ok ( Type, Type )

    -- Integer type
    eval IntT --> Ok ( IntT, Type )

    -- Integer value
    eval (Int 1) --> Ok ( Int 1, IntT )

    -- Variable
    eval (Var "x") --> Ok ( Var "x", Var "x" )

    -- Function type
    eval (Fun (Var "a") IntT) --> Ok ( Fun (Var "a") IntT, Fun (Var "a") Type )

    -- Lambda abstraction
    eval (Lam "x" Type)                            --> Ok ( Lam "x" Type, Fun (Var "x") Type )
    eval (Lam "x" IntT)                            --> Ok ( Lam "x" IntT, Fun (Var "x") Type )
    eval (Lam "x" (Int 1))                         --> Ok ( Lam "x" (Int 1), Fun (Var "x") IntT )
    eval (Lam "x" (Var "x"))                       --> Ok ( Lam "x" (Var "x"), Fun (Var "x") (Var "x") )
    eval (Lam "x" (Fun (Var "x") IntT))            --> Ok ( Lam "x" (Fun (Var "x") IntT), Fun (Var "x") (Fun (Var "x") Type) )
    eval (Lam "x" (Lam "y" (Var "x")))             --> Ok ( Lam "x" (Lam "y" (Var "x")), Fun (Var "x") (Fun (Var "y") (Var "x")) )
    eval (Lam "x" (App (Lam "y" (Int 1)) (Int 2))) --> Ok ( Lam "x" (Int 1), Fun (Var "x") IntT )

    -- Application
    eval (App Type (Int 1))                                        --> Err (CannotApply Type (Int 1))
    eval (App IntT (Int 1))                                        --> Err (CannotApply IntT (Int 1))
    eval (App (Int 0) (Int 1))                                     --> Err (CannotApply (Int 0) (Int 1))
    eval (App (Var "x") (Int 1))                                   --> Err (CannotApply (Var "x") (Int 1))
    eval (App (Fun IntT Type) (Int 1))                             --> Err (CannotApply (Fun IntT Type) (Int 1))
    eval (App (Lam "x" Type) (Int 1))                              --> Ok ( Type, Type )
    eval (App (Lam "x" IntT) (Int 1))                              --> Ok ( IntT, Type )
    eval (App (Lam "x" (Int 0)) (Int 1))                           --> Ok ( Int 0, IntT )
    eval (App (Lam "x" (Var "x")) (Int 1))                         --> Ok ( Int 1, IntT )
    eval (App (Lam "x" (Var "y")) (Int 1))                         --> Ok ( Var "y", Var "y" )
    eval (App (Lam "x" (Fun (Var "x") Type)) (Int 1))              --> Ok ( Fun (Int 1) Type, Fun IntT Type )
    eval (App (Lam "x" (Lam "y" (Var "x"))) (Int 1))               --> Ok ( Lam "y" (Int 1), Fun (Var "y") IntT )
    eval (App (Lam "x" (App (Lam "y" (Int 0)) Type)) (Int 1))      --> Ok ( Int 0, IntT )
    eval (App (App (Lam "x" (Lam "y" (Var "x"))) (Int 1)) (Int 2)) --> Ok ( Int 1, IntT )

-}


tests =
    True

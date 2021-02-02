module Lambda.Lang.Python exposing (write)

import Lambda exposing (Expr(..))


{-|

    import LVM.Lambda.Read
    import LVM.Lang.Python
    import LVM.Procedural
    import Parser exposing (parse)

    pyExpr : String -> Maybe String
    pyExpr txt =
        parse txt LVM.Lambda.Read.expression
            |> Result.map LVM.Procedural.fromLambda
            |> Result.map LVM.Lang.Python.write
            |> Result.toMaybe

    -- Boolean values
    pyExpr "" --> Just ""

-}
write : Expr -> String
write expr =
    ""

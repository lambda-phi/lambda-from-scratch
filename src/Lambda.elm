module Lambda exposing (Expression(..), Type(..))


type Type
    = Type String
    | GenericType String
    | LambdaType Type Type


type Expression
    = Integer Int
    | Number Float
    | Variable String
    | Lambda ( String, Type ) Expression
    | Apply Expression Expression
    | Builtin String Type (List Expression)

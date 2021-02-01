module Lambda.Util exposing (newLowercaseName, toBase)

{-|

    import Lambda.Util exposing (newLowercaseName)

    newLowercaseName 1 [] --> "a"
    newLowercaseName 26 [] --> "z"
    newLowercaseName 27 [] --> "aa"
    newLowercaseName 28 [] --> "ab"

    newLowercaseName 1 [ "a" ] --> "b"
    newLowercaseName 1 [ "a", "b" ] --> "c"

-}


newLowercaseName : Int -> List String -> String
newLowercaseName seed existing =
    let
        name =
            (case seed |> toBase (Char.toCode 'z' - Char.toCode 'a' + 2) of
                x :: xs ->
                    x - 1 :: xs

                [] ->
                    [ 0 ]
            )
                |> List.map (\x -> x + Char.toCode 'a')
                |> List.map Char.fromCode
                |> String.fromList
    in
    if List.any ((==) name) existing then
        newLowercaseName (seed + 1) existing

    else
        name


{-|

    import Lambda.Util exposing (toBase)

    99 |> toBase 2 --> [1,1,0,0,0,1,1]
    99 |> toBase 3 --> [1,0,2,0,0]
    99 |> toBase 4 --> [1,2,0,3]
    99 |> toBase 5 --> [3,4,4]
    99 |> toBase 6 --> [2,4,3]
    99 |> toBase 7 --> [2,0,1]
    99 |> toBase 8 --> [1,4,3]
    99 |> toBase 9 --> [1,2,0]

-}
toBase : Int -> Int -> List Int
toBase base num =
    if num == 0 then
        []

    else
        ((num // base) |> toBase base) ++ [ num |> modBy base ]

module QR exposing (addDummyData, encode, splitList, stringToDataCode, toBinary)

import Binary
import Dict exposing (Dict)


mode =
    [ 0, 1, 0, 0 ]


periodCount =
    8


dummyData =
    [ [ 1, 1, 1, 0, 1, 1, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 1 ] ]


encode : Int -> String -> List (List Int)
encode len input =
    stringToDataCode input |> addDummyData len dummyData |> splitList periodCount


splitList : Int -> List Int -> List (List Int)
splitList n list =
    if List.length list <= n then
        [ list ]

    else
        List.take n list :: (splitList n <| List.drop n list)


addDummyData : Int -> List (List Int) -> List Int -> List Int
addDummyData len dummy list =
    if List.length list // periodCount < len then
        List.concat [ list, List.head dummy |> Maybe.withDefault [] ] |> addDummyData len (List.reverse dummy)

    else
        list


stringToDataCode : String -> List Int
stringToDataCode input =
    let
        charList =
            String.toList input

        stringCountBits =
            toDataBits periodCount <| List.length charList

        dataCode =
            List.map toBinary charList |> List.concat

        endPattern =
            [ 0, 0, 0, 0 ]
    in
    List.concat [ mode, stringCountBits, dataCode, endPattern ]


toBinary : Char -> List Int
toBinary ascii =
    Char.toCode ascii |> toDataBits periodCount


toDataBits : Int -> Int -> List Int
toDataBits len n =
    let
        bits =
            n |> Binary.fromDecimal |> Binary.toIntegers

        fillZeroBits =
            List.repeat (len - List.length bits) 0
    in
    List.append fillZeroBits bits

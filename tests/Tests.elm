module Tests exposing (suite)

import Expect exposing (Expectation)
import QR
import Test exposing (..)


suite : Test
suite =
    describe "The QR module"
        [ describe "toBinary"
            [ test "can translate A" <|
                \_ ->
                    let
                        input =
                            'A'
                    in
                    Expect.equal [ 0, 1, 0, 0, 0, 0, 0, 1 ] (QR.toBinary input)
            , test "can translate :" <|
                \_ ->
                    let
                        input =
                            'a'
                    in
                    Expect.equal [ 0, 1, 1, 0, 0, 0, 0, 1 ] (QR.toBinary input)
            ]
        , describe "stringToDataCode"
            [ test "can translate A" <|
                \_ ->
                    let
                        input =
                            "A"
                    in
                    Expect.equal [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ] (QR.stringToDataCode input)
            ]
        , describe "addDummyData"
            [ test "can add dummy data" <|
                \_ ->
                    let
                        len =
                            3

                        dummyData =
                            [ [ 1, 1, 1, 1, 1, 1, 1, 1 ], [ 0, 0, 0, 0, 0, 0, 0, 0 ] ]

                        input =
                            [ 0, 1, 0, 0, 0, 1, 0, 0 ]
                    in
                    Expect.equal [ 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 ] (QR.addDummyData len dummyData input)
            ]
        , describe "splitList"
            [ test "can split dataCode" <|
                \_ ->
                    let
                        input =
                            [ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0 ]
                    in
                    Expect.equal [ [ 0, 1, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 1, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0 ] ] (QR.splitList 8 input)
            ]
        , describe "encode"
            [ test "can encode abc" <|
                \_ ->
                    let
                        input =
                            "abc"
                    in
                    Expect.equal [ [ 0, 1, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 1, 1, 0, 1, 1, 0 ], [ 0, 0, 0, 1, 0, 1, 1, 0 ], [ 0, 0, 1, 0, 0, 1, 1, 0 ], [ 0, 0, 1, 1, 0, 0, 0, 0 ], [ 1, 1, 1, 0, 1, 1, 0, 0 ] ] (QR.encode 6 input)
            ]
        ]

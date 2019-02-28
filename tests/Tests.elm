module Tests exposing (suite)

import Array
import Decode
import ErrorCorrection
import Expect exposing (Expectation)
import QR
import Test exposing (..)


suite : Test
suite =
    describe "QR quzushi"
        [ describe "The QR module"
            [ describe "toBinary"
                [ test "can translate A" <|
                    \_ ->
                        let
                            input =
                                'A'
                        in
                        Expect.equal [ 0, 1, 0, 0, 0, 0, 0, 1 ] (QR.toBinary input)
                , test "can translate a" <|
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
        , describe "The ErrorCorrection module"
            [ test "can getECPolynomial" <|
                \_ ->
                    let
                        res =
                            Result.Ok <| Array.fromList [ 1, 119, 66, 83, 120, 119, 22, 197, 83, 249, 41, 143, 134, 85, 53, 125, 99, 79 ]
                    in
                    Expect.equal res (ErrorCorrection.getECPolynomial 17)
            , test "can get" <|
                \_ ->
                    let
                        input =
                            [ [ 32, 65, 205, 69, 41, 220, 46, 128, 236 ] ]
                    in
                    Expect.equal (Result.Ok [ [ 42, 159, 74, 221, 244, 169, 239, 150, 138, 70, 237, 85, 224, 96, 74, 219, 61 ] ]) (ErrorCorrection.get 17 input)
            , test "can mod" <|
                \_ ->
                    let
                        p1 =
                            Array.fromList [ 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]

                        p2 =
                            Array.fromList [ 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1 ]
                    in
                    Expect.equal (Result.Ok <| Array.fromList [ 42, 159, 74, 221, 244, 169, 239, 150, 138, 70, 237, 85, 224, 96, 74, 219, 61 ]) (ErrorCorrection.mod p1 p2)
            ]
        , describe "The Decode module"
            [ test "can generate Syndrome if no error" <|
                \_ ->
                    let
                        input =
                            Array.fromList [ 8, 19, 38, 91, 190, 156, 44, 252, 73, 221 ]
                    in
                    Expect.equal [ 0, 0, 0, 0 ] <| Decode.genSyndrome 10 6 input
            , test "can generate Syndrome if error exist" <|
                \_ ->
                    let
                        input =
                            Array.fromList [ 16, 19, 38, 91, 190, 156, 44, 203, 73, 221 ]
                    in
                    Expect.equal [ 47, 150, 200, 225 ] <| Decode.genSyndrome 10 6 input
            , test "can gaussianElimination" <|
                \_ ->
                    let
                        input =
                            [ [ 47, 150, 200 ], [ 47, 150, 200 ], [ 150, 200, 225 ] ]
                    in
                    Expect.equal [ [ 1, 0, 232 ], [ 0, 1, 62 ], [ 0, 0, 0 ] ] <| Decode.gaussianElimination input
            , test "can fix error" <|
                \_ ->
                    let
                        input =
                            Array.fromList [ 16, 19, 38, 91, 190, 156, 44, 203, 73, 221 ]
                    in
                    Expect.equal (Array.fromList [ 8, 19, 38, 91, 190, 156, 44, 252, 73, 221 ]) <| Decode.errorCorrect 10 6 input
            ]
        ]

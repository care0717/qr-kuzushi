module Decode exposing (errorCorrect, gaussianElimination, genSyndrome)

import Array exposing (Array)
import Bitwise
import ErrorCorrection exposing (..)


genCheckMatrix : Int -> Int -> List Polynomial
genCheckMatrix n k =
    let
        reverse aArray =
            Array.toList aArray |> List.reverse |> Array.fromList
    in
    Array.initialize (n - k) identity
        |> Array.toList
        |> List.map (\i -> Array.initialize n (\j -> i * j |> getExp) |> reverse)


sum : Polynomial -> Int
sum poly =
    Array.foldr Bitwise.xor 0 poly



-- error 握りつぶしているのが微妙


times : Int -> Int -> Int
times a b =
    if a /= 0 && b /= 0 then
        unsafeGetLog a + unsafeGetLog b |> getExp

    else
        0


innerProduct : Polynomial -> Polynomial -> Int
innerProduct p1 p2 =
    List.map2 times (Array.toList p1) (Array.toList p2)
        |> List.foldr Bitwise.xor 0


genSyndrome : Int -> Int -> Polynomial -> List Int
genSyndrome n k poly =
    genCheckMatrix n k |> List.map (innerProduct poly)


slice : Int -> Int -> List a -> List a
slice start end aList =
    List.take end aList |> List.reverse |> List.take (end - start) |> List.reverse


genSyndromeMatrix : Int -> Int -> Polynomial -> List (List Int)
genSyndromeMatrix n k poly =
    let
        t =
            (n - k) // 2

        syndrome =
            genSyndrome n k poly
    in
    List.range 0 (t - 1)
        |> List.map (\index -> slice index (index + t + 1) syndrome)


isNotAllZero : List Int -> Bool
isNotAllZero syndrome =
    List.sum syndrome /= 0


top : List Int -> Int
top list =
    let
        t =
            List.head list
    in
    case t of
        Just 0 ->
            List.drop 1 list |> top

        Just n ->
            n

        Nothing ->
            0


normalize : List Int -> List Int
normalize list =
    List.map
        (\i ->
            if i /= 0 then
                unsafeGetLog i - unsafeGetLog (top list) |> getExp

            else
                0
        )
        list


postponeRow : List (List Int) -> List (List Int)
postponeRow matrix =
    List.append (List.drop 1 matrix) [ List.head matrix |> Maybe.withDefault [] ]


gaussianElimination : List (List Int) -> List (List Int)
gaussianElimination syndromeMatrix =
    let
        progressElimination normlist list =
            let
                n =
                    List.map2 times normlist list |> top
            in
            List.map2 (\a b -> Bitwise.xor b (times a n)) normlist list

        isJordan matrix =
            case matrix of
                h :: t ->
                    let
                        head =
                            List.head h |> Maybe.withDefault 0
                    in
                    (head == 1 || not (isNotAllZero h)) && isJordan (List.map (List.drop 1) t)

                _ ->
                    True
    in
    case syndromeMatrix of
        h :: t ->
            if isJordan syndromeMatrix then
                syndromeMatrix

            else if List.sum h /= 0 then
                let
                    normlist =
                        normalize h
                in
                normlist :: List.map (\list -> progressElimination normlist list) t |> postponeRow |> gaussianElimination

            else
                gaussianElimination <| postponeRow syndromeMatrix

        _ ->
            syndromeMatrix


solveJordanMatrix : List (List Int) -> List Int
solveJordanMatrix jordanSyndromeMatrix =
    let
        exceptZero =
            List.filter isNotAllZero jordanSyndromeMatrix

        rank =
            List.length exceptZero
    in
    List.map (\list -> List.drop rank list |> List.head |> Maybe.withDefault 0) exceptZero


errorPositionPolynomial : List Int -> Int -> Int
errorPositionPolynomial coefficients alpha =
    List.foldr (\x acc -> Bitwise.xor (times acc alpha) x) 1 coefficients


getErrorPosition : List Int -> List Int
getErrorPosition coefficients =
    List.range 1 255 |> List.filter (\alpha -> errorPositionPolynomial coefficients alpha == 0) |> List.map unsafeGetLog


makeErrorCodeMatrix : List Int -> List Int -> List (List Int)
makeErrorCodeMatrix errorPosition syndrome =
    List.range 0 (List.length errorPosition - 1)
        |> List.map (\i -> List.map (\j -> i * j |> getExp) errorPosition)
        |> List.map2 (\i list -> List.reverse list |> (::) i |> List.reverse) syndrome


getErrorInfo : List Int -> List Int -> List ( Int, Int )
getErrorInfo errorPosition syndrome =
    let
        errorCodeMatrix =
            makeErrorCodeMatrix errorPosition syndrome

        jordanErrorCodeMatrix =
            gaussianElimination errorCodeMatrix

        errorInfo =
            solveJordanMatrix jordanErrorCodeMatrix
    in
    List.map2 (\a b -> ( a, b )) errorPosition errorInfo


setErrorInfo : Int -> List ( Int, Int ) -> Polynomial -> Polynomial
setErrorInfo n errorInfo poly =
    let
        ( pos, info ) =
            List.head errorInfo |> Maybe.withDefault ( 0, 0 )

        beforeValue =
            Array.get (n - pos - 1) poly |> Maybe.withDefault 0
    in
    if List.isEmpty errorInfo then
        poly

    else
        setErrorInfo n (List.drop 1 errorInfo) (Array.set (n - pos - 1) (Bitwise.xor beforeValue info) poly)


errorCorrect : Int -> Int -> Polynomial -> Polynomial
errorCorrect n k poly =
    let
        syndromeMatrix =
            genSyndromeMatrix n k poly

        coefficients =
            gaussianElimination syndromeMatrix |> solveJordanMatrix

        errorPosition =
            getErrorPosition coefficients

        errorInfo =
            getErrorInfo errorPosition (genSyndrome n k poly)
    in
    setErrorInfo n errorInfo poly

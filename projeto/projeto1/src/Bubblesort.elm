module Bubblesort exposing (..)

-- Implementação do Bubble Sort
bubbleSort : List Int -> List Int
bubbleSort list =
    case list of
        [] ->
            []

        _ ->
            let
                ( newList, sorted ) =
                    pass list
            in
            if sorted then
                newList
            else
                bubbleSort newList


pass : List Int -> ( List Int, Bool )
pass list =
    case list of
        x :: y :: rest ->
            if x > y then
                let
                    ( newRest, sorted ) =
                        pass (x :: rest)
                in
                ( y :: newRest, False )
            else
                let
                    ( newRest, sorted ) =
                        pass (y :: rest)
                in
                ( x :: newRest, sorted )

        otherwise ->
            ( list, True )

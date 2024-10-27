module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)

-- Modelo inicial com uma lista desordenada e o campo de entrada de texto
type alias Model =
    { unsortedList : List Int
    , sortedList : List Int
    , input : String
    }

-- Mensagens que podem ser enviadas ao Update
type Msg
    = Sort
    | UpdateInput String
    | AddNumbers
    | Clean

-- inicializando o model
init : Model
init =
    { unsortedList = []
    , sortedList = []
    , input = ""
    }

--  atualização do model
update : Msg -> Model -> Model
update msg model =
    case msg of
        Sort ->
            { model | sortedList = bubbleSort model.unsortedList }
        UpdateInput newInput ->
            { model | input = newInput }
        AddNumbers ->
            let
                numbers = List.map String.toInt (String.split "," model.input)
                validNumbers = List.filterMap identity numbers
            in
            { model | unsortedList = validNumbers, input = "" }
        Clean ->
            { model | unsortedList = [], sortedList = [], input = "" }

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

-- Função de visualização
view : Model -> Html Msg
view model =
    div []
        [ input [ placeholder "Adicione os numeros", value model.input, onInput UpdateInput ] []
        , button [ onClick AddNumbers ] [ text "Add Numeros" ]
        , div [] [ text "Lista não organizada: ", text (String.join ", " (List.map String.fromInt model.unsortedList)) ]
        , button [ onClick Sort ] [ text "organizador" ]
        , div [] [ text "Lista organizada: ", text (String.join ", " (List.map String.fromInt model.sortedList)) ]
        ]

-- Programa principal
main =
    Browser.sandbox { init = init, update = update, view = view }

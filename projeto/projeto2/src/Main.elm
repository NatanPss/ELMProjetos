module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)

-- Modelo inicial com uma lista desordenada e o campo de entrada de texto
type alias Model =
    { listaDesordenada : List Int
    , listaOrdenada : List Int
    , input : String
    }

-- Mensagens que podem ser enviadas ao Update
type Msg
    = Sort
    | UpdateInput String
    | AddNumbers
    | Clean

-- Função inicial de modelo
init : Model
init =
    { listaDesordenada = []
    , listaOrdenada = []
    , input = ""
    }

-- Função de atualização
update : Msg -> Model -> Model
update msg model =
    case msg of
        Sort ->
            { model | listaOrdenada = bubbleSort model.listaDesordenada }
        UpdateInput newInput ->
            { model | input = newInput }
        AddNumbers ->
            let
                numbers = List.map String.toInt (String.split "," model.input)
                validNumbers = List.filterMap identity numbers
            in
            { model | listaDesordenada = validNumbers, input = "" }
        Clean ->
            { model | listaDesordenada = [], listaOrdenada = [], input = "" }

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
        [ input [ placeholder "Enter numbers separated by commas", value model.input, onInput UpdateInput ] []
        , button [ onClick AddNumbers ] [ text "Add Numbers" ]
        , button [ onClick Clean ] [ text "Clean" ]
        , div [] [ text "Lista desordenada: ", text (String.join ", " (List.map String.fromInt model.listaDesordenada)) ]
        , button [ onClick Sort ] [ text "Sort" ]
        , div [] [ text "Lista ordenada: ", text (String.join ", " (List.map String.fromInt model.listaOrdenada)) ]
        ]

-- Programa principal
main =
    Browser.sandbox { init = init, update = update, view = view }

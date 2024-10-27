module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- Modelo
type alias Model =
    { count : Int }

-- Mensagens
type Msg
    = Increment
    | Decrement

-- Inicializa o modelo
init : Model
init =
    { count = 0 }

-- Atualiza o modelo com base na mensagem
update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }

-- Função de visualização
view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+" ]
        , div [] [ text (String.fromInt model.count) ]
        , button [ onClick Decrement ] [ text "-" ]
        ]

-- Programa principal
main =
    Browser.sandbox { init = init, update = update, view = view }

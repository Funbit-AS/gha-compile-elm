module UI exposing (..)

import Html exposing (Attribute, Html, button, div, h4, text)
import Html.Attributes exposing (class)
import Html.Events exposing (keyCode, on, onClick, stopPropagationOn)
import Json.Decode as Decode


searchButton : msg -> Html msg
searchButton msg =
    button [ class "btn py-2", onClick msg ] [ text "Søk" ]


wrapControl : String -> Html msg -> Html msg
wrapControl heading content =
    div [ class "flex flex-col gap-3" ]
        [ h4 [ class "text-base font-medium" ] [ text heading ]
        , content
        ]


onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Decode.succeed msg

            else
                Decode.fail "not ENTER"
    in
    on "keydown" (Decode.andThen isEnter keyCode)


onToggle : (Bool -> msg) -> Attribute msg
onToggle tagger =
    on "toggle" (Decode.map tagger targetOpen)


targetOpen : Decode.Decoder Bool
targetOpen =
    Decode.at [ "target", "open" ] Decode.bool

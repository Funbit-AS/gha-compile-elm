module Search exposing (InitialValue, Model, Msg, hasFilters, init, toQueryParameters, update, view)

{-| Search bar with a button to trigger a new search (updated filters)
-}

import GraphQL.Request.Builder.Arg as Arg
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import UI
import Url.Builder as UrlBuilder



-- MODEL


type alias InitialValue =
    String


type alias Model =
    { name : String
    , raw : String
    , filters : List ( String, Arg.Value () )
    }


init : String -> InitialValue -> Model
init name initialValue =
    let
        model =
            { name = name, raw = initialValue, filters = [] }
    in
    { model | filters = model |> toFilters }


toArgValue : String -> Maybe (Arg.Value ())
toArgValue rawValue =
    if String.isEmpty rawValue then
        Nothing

    else
        rawValue |> Arg.string |> Just


toQueryParameters : Model -> List UrlBuilder.QueryParameter
toQueryParameters model =
    if String.isEmpty model.raw then
        []

    else
        [ UrlBuilder.string model.name model.raw ]


hasFilters : Model -> Bool
hasFilters model =
    not (List.isEmpty model.filters)



-- UPDATE


type Msg
    = UpdateRaw String
    | UpdateFilter
    | ClearFilter


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateRaw newStr ->
            { model | raw = newStr }

        UpdateFilter ->
            let
                filters =
                    model |> toFilters
            in
            { model | filters = filters }

        ClearFilter ->
            { model | raw = "", filters = [] }


toFilters : Model -> List ( String, Arg.Value () )
toFilters model =
    model.raw
        |> toArgValue
        |> Maybe.map (\v -> ( model.name, v ))
        |> Maybe.map List.singleton
        |> Maybe.withDefault []



-- VIEW


view : Model -> Html Msg
view model =
    ul [ class "flex gap-4" ]
        [ div [ class "grow" ]
            [ label [ hidden True, for "search" ] [ text "Søk" ]
            , input [ name "search", id "search", placeholder "Søk...", type_ "text", value model.raw, onInput UpdateRaw, UI.onEnter UpdateFilter ]
                []
            ]
        , UI.searchButton UpdateFilter
        ]

module Range exposing (FieldType(..), InitialValue, Model, Msg(..), hasFilters, init, toQueryParameters, update, view)

{-| Two text inputs and a search button. When button clicked the filters are updated.
-}

import GraphQL.Request.Builder.Arg as Arg
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import UI
import Url.Builder as UrlBuilder



-- MODEL


type FieldType
    = IntField
    | FloatField


type alias InitialValue =
    { min : String
    , max : String
    }


type alias Model =
    { name : String
    , labelSuffix : String
    , fieldType : FieldType
    , rawMin : String
    , rawMax : String
    , filters : List ( String, Arg.Value () )
    }


init : String -> String -> FieldType -> InitialValue -> Model
init name labelSuffix fieldType initialValue =
    { name = name
    , labelSuffix = labelSuffix
    , fieldType = fieldType
    , rawMin = initialValue.min
    , rawMax = initialValue.max
    , filters = []
    }
        |> updateFilters


toArgValue : FieldType -> String -> Maybe (Arg.Value ())
toArgValue fieldType rawValue =
    case fieldType of
        IntField ->
            rawValue
                |> String.replace "," "."
                |> String.toFloat
                |> Maybe.map round
                |> Maybe.map Arg.int

        FloatField ->
            rawValue
                |> String.replace "," "."
                |> String.toFloat
                |> Maybe.map Arg.float


toQueryParameters : Model -> List UrlBuilder.QueryParameter
toQueryParameters model =
    let
        minParam =
            if String.isEmpty model.rawMin then
                []

            else
                model.rawMin
                    |> UrlBuilder.string (model.name ++ "Min")
                    |> List.singleton

        maxParam =
            if String.isEmpty model.rawMax then
                []

            else
                model.rawMax
                    |> UrlBuilder.string (model.name ++ "Max")
                    |> List.singleton
    in
    minParam ++ maxParam


hasFilters : Model -> Bool
hasFilters model =
    not (List.isEmpty model.filters)



-- UPDATE


type Msg
    = UpdateRawMin String
    | UpdateRawMax String
    | UpdateFilter
    | ClearFilter


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateRawMin newStr ->
            { model | rawMin = newStr }

        UpdateRawMax newStr ->
            { model | rawMax = newStr }

        UpdateFilter ->
            model |> updateFilters

        ClearFilter ->
            { model | rawMin = "", rawMax = "", filters = [] }


{-| Uses the raw values on the model to update its filters field
-}
updateFilters : Model -> Model
updateFilters model =
    let
        minFilter =
            model.rawMin
                |> toArgValue model.fieldType
                |> Maybe.map (\v -> ( model.name ++ "_Gte", v ))
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        maxFilter =
            model.rawMax
                |> toArgValue model.fieldType
                |> Maybe.map (\v -> ( model.name ++ "_Lte", v ))
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    { model | filters = minFilter ++ maxFilter }



-- VIEW


view : String -> Model -> Html Msg
view heading model =
    let
        cleanName =
            model.name
                |> String.toLower
                |> String.replace " " "_"

        minId =
            "min_" ++ cleanName

        maxId =
            "max_" ++ cleanName
    in
    div [ class "flex items-end gap-4" ]
        [ div [ class "grow" ]
            [ label [ class "block font-light pb-1", for minId ] [ text ("Fra " ++ model.labelSuffix) ]
            , input [ name minId, type_ "number", value model.rawMin, onInput UpdateRawMin, UI.onEnter UpdateFilter ] []
            ]
        , div [ class "grow" ]
            [ label [ class "block font-light pb-1", for maxId ] [ text ("Til " ++ model.labelSuffix) ]
            , input [ name maxId, id maxId, type_ "number", value model.rawMax, onInput UpdateRawMax, UI.onEnter UpdateFilter ] []
            ]
        , UI.searchButton UpdateFilter
        ]
        |> UI.wrapControl heading

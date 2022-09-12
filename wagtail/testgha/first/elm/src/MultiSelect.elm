module MultiSelect exposing (InitialValue, Model, Msg(..), hasFilters, init, toFilters, toQueryParameters, update, view)

import GraphQL.Request.Builder.Arg as Arg
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import UI
import Url.Builder as UrlBuilder



-- MODEL


type alias InitialValue =
    List String


type alias Item =
    { label : String
    , value : String
    , selected : Bool
    }


type alias Model =
    { items : List Item
    , name : String
    }


initItem : String -> Item
initItem value =
    { label = value
    , value = value
    , selected = False
    }


{-| Given a list of strings, set selected property on item based on whether its value is
contained in list
-}
applySelection : List String -> Item -> Item
applySelection selection item =
    { item | selected = List.member item.value selection }


init : String -> List String -> InitialValue -> Model
init name labelsValues initialValues =
    { items = labelsValues |> List.map initItem |> List.map (applySelection initialValues)
    , name = name
    }


toFilters : Model -> List ( String, Arg.Value () )
toFilters model =
    let
        selection =
            model.items
                |> List.filter .selected
                |> List.map .value
    in
    if List.isEmpty selection then
        []

    else
        let
            v =
                selection
                    |> List.map Arg.string
        in
        [ ( model.name ++ "_In", Arg.list v ) ]


hasFilters : Model -> Bool
hasFilters model =
    model
        |> toFilters
        |> List.isEmpty
        |> not


toQueryParameters : Model -> List UrlBuilder.QueryParameter
toQueryParameters model =
    model.items
        |> List.filter .selected
        |> List.map .value
        |> List.map (UrlBuilder.string model.name)



-- UPDATE


type Msg
    = ToggleItem Item
    | ClearSelection


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleItem item ->
            { model
                | items =
                    model.items
                        |> List.map
                            (\i ->
                                if i == item then
                                    { i | selected = not i.selected }

                                else
                                    i
                            )
            }

        ClearSelection ->
            { model
                | items =
                    model.items
                        |> List.map (\i -> { i | selected = False })
            }


view : String -> Model -> Maybe (List String) -> Html Msg
view heading model maybeCurrentResults =
    let
        anyCheckboxesSelected =
            -- Are any of the options included in this group of checkboxes selected?
            model |> hasFilters
    in
    -- Two columns of checkboxes on mobile
    -- Four columns of checkboxes on tablet
    -- Two for large because the filter palette is no longer full screen
    (model.items
        |> List.indexedMap (viewItem model.name anyCheckboxesSelected maybeCurrentResults)
        |> div [ class "grid grid-cols-2 md:grid-cols-4 lg:grid-cols-2 justify-start gap-2" ]
    )
        |> UI.wrapControl heading


viewItem : String -> Bool -> Maybe (List String) -> Int -> Item -> Html Msg
viewItem name anyCheckboxesSelected maybeCurrentResults idx item =
    let
        fieldId =
            name ++ "_" ++ String.fromInt idx

        -- Show how many results would be match this filter's predicate
        -- If there are no results yet then just show the item label
        maybeNumberOfMatches =
            maybeCurrentResults
                |> Maybe.map (List.filter (\r -> r == item.value))
                |> Maybe.map List.length

        noMatches =
            maybeNumberOfMatches
                |> Maybe.map (\n -> n == 0)
                |> Maybe.withDefault False

        displayAsGrey =
            noMatches && not anyCheckboxesSelected
    in
    div [ class "flex items-center" ]
        [ input [ class "w-4 h-4 text-blue-600 bg-white border-black focus:ring-blue-500 focus:ring-2", id fieldId, checked item.selected, type_ "checkbox", value "", onClick <| ToggleItem item ]
            []
        , label
            [ classList
                [ ( "ml-2 text-sm font-medium", True )
                , ( "text-gray-400", displayAsGrey )
                , ( "text-gray-900", not displayAsGrey )
                ]
            , for fieldId
            ]
            [ text
                (if anyCheckboxesSelected then
                    -- The way Finn works is that if you already have some checkboxes
                    -- selected, the numbers added to the others will tell you how
                    -- many *extra* matches you would get if you selected this one.
                    -- We can't know that without having an extra dataset in our model
                    -- so we just show the label.
                    item.label

                 else
                    -- If no checkboxes are selected for this field, then we can show
                    -- the subset of matches that will be available if this checkbox
                    -- is selected. This replicates Finn.no behaviour.
                    case maybeNumberOfMatches of
                        Just numMatches ->
                            item.label ++ " (" ++ String.fromInt numMatches ++ ")"

                        Nothing ->
                            item.label
                )
            ]
        ]

port module Main exposing (..)

import API
import Browser
import FormatNumber
import FormatNumber.Locales exposing (frenchLocale)
import GraphQL.Request.Builder as GraphQL
import GraphQL.Request.Builder.Arg as Arg
import Html exposing (..)
import Html.Attributes exposing (..)
import MultiSelect
import Range
import Search
import Svg
import Svg.Attributes
import Task
import UI
import Url
import Url.Builder as UrlBuilder



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Visibility
    = Closed
    | Open



-- MODEL


type alias InitialFilters =
    { search : Search.InitialValue
    , makes : MultiSelect.InitialValue
    , bodyTypes : MultiSelect.InitialValue
    , transmissions : MultiSelect.InitialValue
    , engineFuels : MultiSelect.InitialValue
    , wheelDrives : MultiSelect.InitialValue
    , price : Range.InitialValue
    , year : Range.InitialValue
    , mileage : Range.InitialValue
    }


type alias Flags =
    { url : String
    , isMobile : Bool
    , pagePathComponents : List String
    , initialFilters : InitialFilters
    , fallbackThumbnail : String
    , makes : List String
    , bodyTypes : List String
    , transmissions : List String
    , engineFuels : List String
    , wheelDrives : List String
    }


type alias CarPage =
    { title : String
    , pageUrl : String
    , headline : String
    , introduction : String
    , thumbnail : String

    -- Technical specs
    , bodyType : String
    , engineFuel : String
    , make : String
    , transmission : String
    , wheelDrive : String
    , priceMain : Maybe Int
    , year : Maybe Int
    , mileage : Maybe Int
    }


type alias Model =
    { flags : Flags

    -- Whether the filter palette is collapsed or expanded
    , filterVisibility : Visibility

    -- The current state of the filter selection
    , search : Search.Model
    , makes : MultiSelect.Model
    , bodyTypes : MultiSelect.Model
    , transmissions : MultiSelect.Model
    , engineFuels : MultiSelect.Model
    , wheelDrives : MultiSelect.Model
    , price : Range.Model
    , year : Range.Model
    , mileage : Range.Model

    -- The data returned by the API
    , cars : API.Data (List CarPage)
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { flags = flags
            , filterVisibility =
                if flags.isMobile then
                    Closed

                else
                    Open
            , search = Search.init "search" flags.initialFilters.search
            , makes = MultiSelect.init "make" flags.makes flags.initialFilters.makes
            , bodyTypes = MultiSelect.init "bodyType" flags.bodyTypes flags.initialFilters.bodyTypes
            , transmissions = MultiSelect.init "transmission" flags.transmissions flags.initialFilters.transmissions
            , engineFuels = MultiSelect.init "engineFuel" flags.engineFuels flags.initialFilters.engineFuels
            , wheelDrives = MultiSelect.init "wheelDrive" flags.wheelDrives flags.initialFilters.wheelDrives
            , cars = API.Loading Nothing
            , price = Range.init "priceMain" "kr" Range.IntField flags.initialFilters.price
            , year = Range.init "year" "år" Range.IntField flags.initialFilters.year
            , mileage = Range.init "mileage" "km" Range.IntField flags.initialFilters.mileage
            }
    in
    ( model, getCars model )



-- API FETCHING


carValueSpec : GraphQL.ValueSpec GraphQL.NonNull GraphQL.ObjectType CarPage vars
carValueSpec =
    GraphQL.object CarPage
        |> GraphQL.with (GraphQL.field "title" [] GraphQL.string)
        |> GraphQL.with (GraphQL.field "pageUrl" [] GraphQL.string)
        |> GraphQL.with (GraphQL.field "headline" [] GraphQL.string)
        |> GraphQL.with (GraphQL.field "introduction" [] GraphQL.string)
        |> GraphQL.with (GraphQL.field "thumbnail" [] GraphQL.string)
        |> GraphQL.with (GraphQL.field "bodyType" [] GraphQL.string)
        |> GraphQL.with (GraphQL.field "engineFuel" [] GraphQL.string)
        |> GraphQL.with (GraphQL.field "make" [] GraphQL.string)
        |> GraphQL.with (GraphQL.field "transmission" [] GraphQL.string)
        |> GraphQL.with (GraphQL.field "wheelDrive" [] GraphQL.string)
        |> GraphQL.with (GraphQL.field "priceMain" [] (GraphQL.nullable GraphQL.int))
        |> GraphQL.with (GraphQL.field "year" [] (GraphQL.nullable GraphQL.int))
        |> GraphQL.with (GraphQL.field "mileage" [] (GraphQL.nullable GraphQL.int))


carsQuery : List ( String, Arg.Value () ) -> GraphQL.Document GraphQL.Query (API.GraphQLData (List CarPage)) ()
carsQuery filters =
    carValueSpec
        |> API.connectionNodes
        |> GraphQL.field "cars" filters
        |> GraphQL.extract
        |> GraphQL.queryDocument


carsRequest : Model -> GraphQL.Request GraphQL.Query (API.GraphQLData (List CarPage))
carsRequest model =
    let
        filters =
            List.concat
                [ model.search.filters
                , model.makes
                    |> MultiSelect.toFilters
                , model.bodyTypes
                    |> MultiSelect.toFilters
                , model.transmissions
                    |> MultiSelect.toFilters
                , model.engineFuels
                    |> MultiSelect.toFilters
                , model.wheelDrives
                    |> MultiSelect.toFilters
                , model.price.filters
                , model.year.filters
                , model.mileage.filters
                ]
    in
    filters
        |> carsQuery
        |> GraphQL.request ()


port replaceUrl : String -> Cmd msg


{-| Builds a querystring from the current filters to allow link sharing
-}
updateUrlQuerystring : Model -> Cmd Msg
updateUrlQuerystring model =
    List.concat
        [ Search.toQueryParameters model.search
        , MultiSelect.toQueryParameters model.makes
        , MultiSelect.toQueryParameters model.bodyTypes
        , MultiSelect.toQueryParameters model.transmissions
        , MultiSelect.toQueryParameters model.engineFuels
        , MultiSelect.toQueryParameters model.wheelDrives
        , Range.toQueryParameters model.price
        , Range.toQueryParameters model.year
        , Range.toQueryParameters model.mileage
        ]
        -- JS will ignore empty urls so we have to always give it something, even
        -- when we have no querystring. This is why we used an absolute url
        |> UrlBuilder.absolute model.flags.pagePathComponents
        -- Elm only lets you use its replaceUrl method if you are running your app
        -- as a full web application so we just tell JS to do the work for us.
        |> replaceUrl


getCars : Model -> Cmd Msg
getCars model =
    model
        |> carsRequest
        |> API.sendRequest model.flags.url
        |> Task.attempt (API.fromResult >> GotCars)



-- Model helpers


{-| If we have API results then return a list of values for a given field.
If we don't have results then return Nothing.

Example:
model
|> maybeFieldResults .make

This would return the make value of each car in the current list of results.

-}
maybeFieldResults : (CarPage -> String) -> Model -> Maybe (List String)
maybeFieldResults gettr model =
    model.cars
        |> API.toMaybe
        |> Maybe.map (List.map gettr)



-- UPDATE


type Msg
    = GotCars (API.Data (List CarPage))
    | FilterVisibilityToggled Bool
    | GotSearchMsg Search.Msg
    | GotMakesMsg MultiSelect.Msg
    | GotBodyTypesMsg MultiSelect.Msg
    | GotTransmissionsMsg MultiSelect.Msg
    | GotEngineFuelsMsg MultiSelect.Msg
    | GotWheelDrivesMsg MultiSelect.Msg
    | GotPriceMsg Range.Msg
    | GotYearMsg Range.Msg
    | GotMileageMsg Range.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCars carsData ->
            ( { model | cars = carsData }, Cmd.none )

        FilterVisibilityToggled newValue ->
            -- Check that the newValue is the opposite of the current value
            case ( newValue, model.filterVisibility ) of
                ( False, Open ) ->
                    ( { model | filterVisibility = Closed }, Cmd.none )

                ( True, Closed ) ->
                    ( { model | filterVisibility = Open }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotSearchMsg subMsg ->
            let
                newModel =
                    { model | search = Search.update subMsg model.search }

                cmd =
                    if model.search.filters == newModel.search.filters then
                        Cmd.none

                    else
                        Cmd.batch [ updateUrlQuerystring newModel, getCars newModel ]

                cars =
                    if model.search.filters == newModel.search.filters then
                        model.cars

                    else
                        API.toLoading model.cars
            in
            ( { newModel | cars = cars }, cmd )

        GotMakesMsg subMsg ->
            let
                newModel =
                    { model | cars = API.toLoading model.cars, makes = MultiSelect.update subMsg model.makes }
            in
            ( newModel, Cmd.batch [ updateUrlQuerystring newModel, getCars newModel ] )

        GotBodyTypesMsg subMsg ->
            let
                newModel =
                    { model | cars = API.toLoading model.cars, bodyTypes = MultiSelect.update subMsg model.bodyTypes }
            in
            ( newModel, Cmd.batch [ updateUrlQuerystring newModel, getCars newModel ] )

        GotTransmissionsMsg subMsg ->
            let
                newModel =
                    { model | cars = API.toLoading model.cars, transmissions = MultiSelect.update subMsg model.transmissions }
            in
            ( newModel, Cmd.batch [ updateUrlQuerystring newModel, getCars newModel ] )

        GotEngineFuelsMsg subMsg ->
            let
                newModel =
                    { model | cars = API.toLoading model.cars, engineFuels = MultiSelect.update subMsg model.engineFuels }
            in
            ( newModel, Cmd.batch [ updateUrlQuerystring newModel, getCars newModel ] )

        GotWheelDrivesMsg subMsg ->
            let
                newModel =
                    { model | cars = API.toLoading model.cars, wheelDrives = MultiSelect.update subMsg model.wheelDrives }
            in
            ( newModel, Cmd.batch [ updateUrlQuerystring newModel, getCars newModel ] )

        GotPriceMsg subMsg ->
            -- The change in user input might not actually result in new filters
            let
                newModel =
                    { model | price = Range.update subMsg model.price }

                cmd =
                    if model.price.filters == newModel.price.filters then
                        Cmd.none

                    else
                        Cmd.batch [ updateUrlQuerystring newModel, getCars newModel ]

                cars =
                    if model.price.filters == newModel.price.filters then
                        model.cars

                    else
                        API.toLoading model.cars
            in
            ( { newModel | cars = cars }, cmd )

        GotYearMsg subMsg ->
            -- The change in user input might not actually result in new filters
            let
                newModel =
                    { model | year = Range.update subMsg model.year }

                cmd =
                    if model.year.filters == newModel.year.filters then
                        Cmd.none

                    else
                        Cmd.batch [ updateUrlQuerystring newModel, getCars newModel ]

                cars =
                    if model.year.filters == newModel.year.filters then
                        model.cars

                    else
                        API.toLoading model.cars
            in
            ( { newModel | cars = cars }, cmd )

        GotMileageMsg subMsg ->
            -- The change in user input might not actually result in new filters
            let
                newModel =
                    { model | mileage = Range.update subMsg model.mileage }

                cmd =
                    if model.mileage.filters == newModel.mileage.filters then
                        Cmd.none

                    else
                        Cmd.batch [ updateUrlQuerystring newModel, getCars newModel ]

                cars =
                    if model.mileage.filters == newModel.mileage.filters then
                        model.cars

                    else
                        API.toLoading model.cars
            in
            ( { newModel | cars = cars }, cmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    section [ class "w-full max-w-screen-2xl mx-auto grid grid-cols-1 lg:grid-cols-3 p-5 lg:p-10 xl:p-14 gap-5 lg:gap-10 xl:gap-14" ]
        ([ -- Filter controls
           -- We use the standard details:summary element to handle hiding and showing filters
           -- However, as there is no tailwind selector for seeing if child is in open state
           -- we also keep track of open/close state in our model and use it to alter the
           -- row span in the top level div, below.
           let
            isOpenAttribute =
                case model.filterVisibility of
                    Open ->
                        [ attribute "open" "" ]

                    Closed ->
                        []

            lg_row_span =
                case model.filterVisibility of
                    Open ->
                        "lg:row-span-3"

                    Closed ->
                        "lg:row-span-1"
           in
           div [ class (lg_row_span ++ " h-min bg-primary lg:rounded p-5 flex flex-col gap-5 ") ]
            [ h2 [] [ text "Søk etter biler" ]
            , Search.view model.search
                |> Html.map GotSearchMsg
            , details ([ class "group", UI.onToggle FilterVisibilityToggled ] ++ isOpenAttribute)
                [ summary [ class "marker:none flex flex-row items-center gap-2" ]
                    [ Svg.svg [ Svg.Attributes.class "cursor-pointer fill-current opacity-75 w-6 h-6 rotate-90 group-open:-rotate-90 transition-all", Svg.Attributes.viewBox "0 0 20 20", attribute "xmlns" "http://www.w3.org/2000/svg" ]
                        [ Svg.path [ Svg.Attributes.d "M12.95 10.707l.707-.707L8 4.343 6.586 5.757 10.828 10l-4.242 4.243L8 15.657l4.95-4.95z" ]
                            []
                        ]
                    , span [ class "cursor-pointer font-light text-secondary underline" ]
                        [ text
                            -- Number of active filters
                            (let
                                activeFilters =
                                    -- NB: Don't include search which is separate in UI
                                    -- [ model.search |> Search.hasFilters
                                    [ model.makes |> MultiSelect.hasFilters
                                    , model.bodyTypes |> MultiSelect.hasFilters
                                    , model.transmissions |> MultiSelect.hasFilters
                                    , model.engineFuels |> MultiSelect.hasFilters
                                    , model.wheelDrives |> MultiSelect.hasFilters
                                    , model.price |> Range.hasFilters
                                    , model.year |> Range.hasFilters
                                    , model.mileage |> Range.hasFilters
                                    ]
                                        |> List.filter identity
                                        |> List.length
                             in
                             if activeFilters == 0 then
                                case model.filterVisibility of
                                    Open ->
                                        "Skjul filtre"

                                    Closed ->
                                        "Se filtre"

                             else if activeFilters == 1 then
                                "1 filter"

                             else
                                String.fromInt activeFilters ++ " filtre"
                            )
                        ]

                    -- Number of search results
                    , case model.cars of
                        API.Loading (Just data) ->
                            viewNumberOfResults data.data

                        API.Success data ->
                            viewNumberOfResults data.data

                        _ ->
                            text ""
                    ]
                , div [ class "pt-5 flex flex-col gap-5" ]
                    [ MultiSelect.view "Merke" model.makes (model |> maybeFieldResults .make)
                        |> Html.map GotMakesMsg
                    , MultiSelect.view "Type" model.bodyTypes (model |> maybeFieldResults .bodyType)
                        |> Html.map GotBodyTypesMsg
                    , MultiSelect.view "Girkasse" model.transmissions (model |> maybeFieldResults .transmission)
                        |> Html.map GotTransmissionsMsg
                    , MultiSelect.view "Drivstoff" model.engineFuels (model |> maybeFieldResults .engineFuel)
                        |> Html.map GotEngineFuelsMsg
                    , MultiSelect.view "Hjuldrift" model.wheelDrives (model |> maybeFieldResults .wheelDrive)
                        |> Html.map GotWheelDrivesMsg
                    , Range.view "Pris" model.price
                        |> Html.map GotPriceMsg
                    , Range.view "Årsmodell" model.year
                        |> Html.map GotYearMsg
                    , Range.view "Kilometerstand" model.mileage
                        |> Html.map GotMileageMsg
                    ]
                ]
            ]
         ]
            ++ -- Cars
               (case model.cars of
                    API.Loading maybeData ->
                        case maybeData of
                            Just data ->
                                viewData model.flags data.data True

                            Nothing ->
                                -- Initial load
                                [ text "Henter biler..." ]

                    API.Failure error ->
                        [ text "Det oppsto en feil" ]

                    API.Success data ->
                        viewData model.flags data.data False
               )
        )


viewData : Flags -> List CarPage -> Bool -> List (Html Msg)
viewData flags cars isLoading =
    cars
        |> List.map (viewCar flags isLoading)


viewCar : Flags -> Bool -> CarPage -> Html Msg
viewCar flags isLoading car =
    let
        thumbnailSrc =
            if String.isEmpty car.thumbnail then
                flags.fallbackThumbnail

            else
                car.thumbnail
    in
    -- This top element has x padding on mobile only so that the car previews are not
    -- full width but the filters box can be
    -- On desktop the filters box is also padded so it can be acheieved with a simple
    -- grid gap.
    a
        [ classList
            [ ( "h-full active:scale-95 focus:scale-100 ease-in-out transition duration-100 lg:hover:scale-105", True )
            , ( "opacity-50", isLoading )
            ]
        , href car.pageUrl
        ]
        [ div [ class "h-full shadow-2xl rounded-xl overflow-hidden flex flex-col" ]
            [ img [ class "w-full aspect-[4/3] object-cover", attribute "loading" "lazy", src thumbnailSrc, alt car.introduction ] []
            , p [ class "grow font-medium text-base text-left px-3 lg:px-5 pt-3 lg:pt-5 pb-5" ]
                [ text car.introduction ]
            , ul [ class "w-full flex flex-row justify-between px-3 lg:px-5 py-2 lg:py-4" ]
                [ li [ class "text-base" ]
                    [ text
                        (case car.year of
                            Just year ->
                                String.fromInt year

                            Nothing ->
                                "-"
                        )
                    ]
                , li [ class "text-base" ]
                    [ text (formatMaybeInt car.mileage ++ " km")
                    ]
                , li [ class "text-base" ]
                    [ text (formatMaybeInt car.priceMain ++ " kr")
                    ]
                ]
            ]
        ]


viewNumberOfResults : List a -> Html Msg
viewNumberOfResults results =
    p [ class "grow font-medium text-lg text-right" ]
        [ text
            (if List.isEmpty results then
                "Ingen treff"

             else
                (results |> List.length |> String.fromInt) ++ " treff."
            )
        ]


norwayLocale : FormatNumber.Locales.Locale
norwayLocale =
    { frenchLocale
        | decimals = FormatNumber.Locales.Exact 0
    }


formatMaybeInt : Maybe Int -> String
formatMaybeInt maybeInt =
    case maybeInt of
        Just int ->
            FormatNumber.format norwayLocale (toFloat int)

        Nothing ->
            "-"

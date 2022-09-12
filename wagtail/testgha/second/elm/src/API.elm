module API exposing (Data(..), GraphQLData, connectionNodes, fromResult, sendRequest, toLoading, toMaybe)

{-| This library abstracts away the details of working with the GraphQL API.
-}

import GraphQL.Client.Http as GQLHttp
import GraphQL.Request.Builder as GraphQL
import Task


type alias GraphQLData a =
    { data : a
    , startCursor : Maybe String
    , endCursor : Maybe String
    , hasNextPage : Bool
    }


type Data a
    = Loading (Maybe (GraphQLData a))
    | Failure GQLHttp.Error
    | Success (GraphQLData a)


fromResult : Result GQLHttp.Error (GraphQLData a) -> Data a
fromResult result =
    case result of
        Err e ->
            Failure e

        Ok x ->
            Success x


toMaybe : Data a -> Maybe a
toMaybe apiData =
    case apiData of
        Loading maybeData ->
            maybeData |> Maybe.map .data

        Success x ->
            Just x.data

        Failure _ ->
            Nothing


{-| Change status of Data to Loading, preserving any existing data
-}
toLoading : Data a -> Data a
toLoading data =
    case data of
        Loading _ ->
            data

        Failure _ ->
            Loading Nothing

        Success existing ->
            Loading (Just existing)


{-| A function that helps you extract node objects from paginated Relay connections.
-}
connectionNodes :
    GraphQL.ValueSpec GraphQL.NonNull GraphQL.ObjectType result vars
    -> GraphQL.ValueSpec GraphQL.NonNull GraphQL.ObjectType (GraphQLData (List result)) vars
connectionNodes spec =
    GraphQL.object GraphQLData
        |> GraphQL.with
            (GraphQL.field "edges"
                []
                (GraphQL.list
                    (GraphQL.extract
                        (GraphQL.field "node" [] spec)
                    )
                )
            )
        |> GraphQL.with
            (GraphQL.field "pageInfo"
                []
                (GraphQL.extract (GraphQL.field "startCursor" [] (GraphQL.nullable GraphQL.id)))
            )
        |> GraphQL.with
            (GraphQL.field "pageInfo"
                []
                (GraphQL.extract (GraphQL.field "endCursor" [] (GraphQL.nullable GraphQL.id)))
            )
        |> GraphQL.with
            (GraphQL.field "pageInfo"
                []
                (GraphQL.extract (GraphQL.field "hasNextPage" [] GraphQL.bool))
            )


sendRequest : String -> GraphQL.Request GraphQL.Query a -> Task.Task GQLHttp.Error a
sendRequest url request =
    GQLHttp.sendQuery url request

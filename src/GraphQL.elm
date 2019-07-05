module GraphQL exposing
    ( GraphQL, query, mutation, subscription, render, toDecoder
    , Request, get, post
    , withHeader, withHeaders, withBearerToken
    , withQueryParam, withQueryParams, withTimeout, withCredentials, withDataPath
    , Error(..), send, sendWithTracker, toTask, toTaskWithCacheBuster
    )

{-| Building and sending GraphQL.

Building of HTTP request has been based on [`elm-http-builder`](http://package.elm-lang.org/packages/lukewestby/elm-http-builder/latest).


# Build a GraphQL

@docs GraphQL, query, mutation, subscription, render, toDecoder


# Build a Request

@docs Request, get, post

@docs withHeader, withHeaders, withBearerToken
@docs withQueryParam, withQueryParams, withTimeout, withCredentials, withDataPath


# Make a Request

@docs Error, send, sendWithTracker, toTask, toTaskWithCacheBuster

-}

import GraphQL.Selector as Selector exposing (Selector)
import Http
import Internal
import Json.Decode as Decode exposing (Decoder, decodeString, errorToString)
import Json.Encode as Encode
import Task exposing (Task)
import Time
import Url


{-| -}
type GraphQL a
    = GraphQL String String (Selector a)


{-| Build a GraphQL named query.

    GraphQL.Selector.succeed identity
        |> GraphQL.Selector.field "me"
            []
            (GraphQL.Selector.succeed User
                |> GraphQL.Selector.field "id" [] GraphQL.Selector.string
                |> GraphQL.Selector.field "firstName" [] GraphQL.Selector.string
                |> GraphQL.Selector.field "lastName" [] GraphQL.Selector.string
            )
        |> query "Me"

It collects a GraphQL which is equal to:

    """
    query Me {
        me {
            id
            firstName
            lastName
        }
    }
    """

and decoder is equal to:

    Decode.field "me"
        (Decode.map3
            (Decode.field "id" Decode.string)
            (Decode.field "firstName" Decode.string)
            (Decode.field "lastName" Decode.string)
        )

-}
query : String -> Selector a -> GraphQL a
query =
    GraphQL "query"


{-| Build a GraphQL named mutation.

    GraphQL.Selector.succeed identity
        |> GraphQL.Selector.field "updateMe"
            [ ( "firstName", GraphQL.Argument.string "Tom" )
            ]
            (GraphQL.Selector.succeed User
                |> GraphQL.Selector.field "id" [] GraphQL.Selector.string
                |> GraphQL.Selector.field "firstName" [] GraphQL.Selector.string
                |> GraphQL.Selector.field "lastName" [] GraphQL.Selector.string
            )
        |> mutation "RenameUser"

It collects a GraphQL which is equal to:

    """
    mutation RenameUser {
        updateMe("firstName": "Tom") {
            id
            firstName
            lastName
        }
    }
    """

and decoder is equal to:

    Decode.field "updateMe"
        (Decode.map3
            (Decode.field "id" Decode.string)
            (Decode.field "firstName" Decode.string)
            (Decode.field "lastName" Decode.string)
        )

-}
mutation : String -> Selector a -> GraphQL a
mutation =
    GraphQL "mutation"


{-| Build a GraphQL named subscription.

    GraphQL.Selector.succeed identity
        |> GraphQL.Selector.field "onUpdateMe"
            []
            (GraphQL.Selector.succeed User
                |> GraphQL.Selector.field "id" [] GraphQL.Selector.string
                |> GraphQL.Selector.field "firstName" [] GraphQL.Selector.string
                |> GraphQL.Selector.field "lastName" [] GraphQL.Selector.string
            )
        |> subscription "OnUpdateUser"

It collects a GraphQL which is equal to:

    """
    subscription OnUpdateUser {
        onUpdateMe {
            id
            firstName
            lastName
        }
    }
    """

and decoder is equal to:

    Decode.field "onUpdateMe"
        (Decode.map3
            (Decode.field "id" Decode.string)
            (Decode.field "firstName" Decode.string)
            (Decode.field "lastName" Decode.string)
        )

-}
subscription : String -> Selector a -> GraphQL a
subscription =
    GraphQL "subscription"


{-| Render GraphQL string representation from a GraphQL.
-}
render : GraphQL a -> String
render (GraphQL operation name selector) =
    (operation ++ " " ++ name)
        ++ Internal.wrap "{" "}" (Selector.render selector)


{-| Build a Decoder from a GraphQL.
-}
toDecoder : GraphQL a -> Decoder a
toDecoder (GraphQL _ _ selector) =
    Selector.toDecoder selector


{-| A type for chaining request configuration.
-}
type Request a
    = Request
        { method : String
        , url : String
        , headers : List Http.Header
        , body : Http.Body
        , decoder : Decoder a
        , dataPath : List String
        , timeout : Maybe Float
        , withCredentials : Bool
        , queryParams : List ( String, String )
        }


requestBuilder : Bool -> String -> GraphQL a -> Request a
requestBuilder isGetMethod url graphql =
    let
        graph =
            render graphql

        ( method, queryParams, body ) =
            if isGetMethod then
                ( "GET"
                , [ ( "query", graph ) ]
                , Http.emptyBody
                )

            else
                ( "POST"
                , []
                , [ ( "query", Encode.string graph ) ]
                    |> Encode.object
                    |> Http.jsonBody
                )
    in
    Request
        { method = method
        , url = url
        , headers = []
        , body = body
        , decoder = toDecoder graphql
        , dataPath = [ "data" ]
        , timeout = Nothing
        , withCredentials = False
        , queryParams = queryParams
        }


{-| Start building a GET request with a given URL.

    GraphQL.Selector.succeed Tuple.pair
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"

-}
get : String -> GraphQL a -> Request a
get =
    requestBuilder True


{-| Start building a POST request with a given URL.

    GraphQL.Selector.succeed Tuple.pair
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"

-}
post : String -> GraphQL a -> Request a
post =
    requestBuilder False


{-| Add a single header to a request.

    GraphQL.Selector.succeed Tuple.pair
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"
        |> withHeader "Content-Type" "application/json"
        |> withHeader "Accept" "application/json"

-}
withHeader : String -> String -> Request a -> Request a
withHeader key value (Request builder) =
    Request { builder | headers = Http.header key value :: builder.headers }


{-| Add many headers to a request.

    GraphQL.Selector.succeed Tuple.pair
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"
        |> withHeaders
            [ ( "Content-Type", "application/json" )
            , ( "Accept", "application/json" )
            ]

-}
withHeaders : List ( String, String ) -> Request a -> Request a
withHeaders headerPairs (Request builder) =
    case List.map (\( key, value ) -> Http.header key value) headerPairs of
        [] ->
            Request builder

        newHeaders ->
            Request { builder | headers = builder.headers ++ newHeaders }


{-| Add a bearer token to a request.

    GraphQL.Selector.succeed Tuple.pair
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"
        |> withBearerToken "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhIjoiYSJ9.MvhYYpYBuN1rUaV0GGnQGvr889zY0xSc20Lnt8nMTfE"

-}
withBearerToken : String -> Request a -> Request a
withBearerToken value request =
    withHeader "Authorization" ("Bearer " ++ value) request


{-| Add a query param to the url for the request.

    GraphQL.Selector.succeed Tuple.pair
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"
        |> withQueryParams "hello" "world"
        |> withQueryParams "baz" "qux"

    -- sends a request to https://example.com/graphql?hello=world&baz=qux

-}
withQueryParam : String -> String -> Request a -> Request a
withQueryParam key value (Request builder) =
    Request { builder | queryParams = builder.queryParams ++ [ ( key, value ) ] }


{-| Add some query params to the url for the request.

    GraphQL.Selector.succeed Tuple.pair
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"
        |> withQueryParams [ ( "hello", "world" ), ( "foo", "bar" ) ]
        |> withQueryParams [ ( "baz", "qux" ) ]

    -- sends a request to https://example.com/graphql?hello=world&foo=bar&baz=qux

-}
withQueryParams : List ( String, String ) -> Request a -> Request a
withQueryParams queryParams (Request builder) =
    Request { builder | queryParams = builder.queryParams ++ queryParams }


{-| Set the `timeout` setting on the request.

    GraphQL.Selector.succeed Tuple.pair
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"
        |> withTimeout (10 * Time.second)

-}
withTimeout : Float -> Request a -> Request a
withTimeout timeout (Request builder) =
    Request { builder | timeout = Just timeout }


{-| Set the `withCredentials` flag on the request to True. Works via
[`XMLHttpRequest#withCredentials`](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/withCredentials).

    GraphQL.Selector.succeed Tuple.pair
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"
        |> withCredentials True

-}
withCredentials : Bool -> Request a -> Request a
withCredentials with (Request builder) =
    Request { builder | withCredentials = with }


{-| Set a path of graphql data. By default it set as `[ "data" ]`.

    GraphQL.Selector.succeed Tuple.pair
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"
        |> withDataPath [ "my", "custom", "data", "path" ]

-}
withDataPath : List String -> Request a -> Request a
withDataPath dataPath (Request builder) =
    Request { builder | dataPath = dataPath }


{-| A Request can fail in a couple ways:

  - BadUrl means you did not provide a valid URL.
  - Timeout means it took too long to get a response.
  - NetworkError means the user turned off their wifi, went in a cave, etc.
  - BadStatus means you got a response back, but the status code indicates failure.
  - BadPayload means you got a response back with a nice status code,
    but the body of the response was something unexpected.
    The String in this case is a debugging message that
    explains what went wrong with your JSON decoder or whatever.

-}
type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int
    | BadBody String


responseHandler : Decoder a -> Http.Response String -> Result Error a
responseHandler decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (BadUrl url)

        Http.Timeout_ ->
            Err Timeout

        Http.NetworkError_ ->
            Err NetworkError

        Http.BadStatus_ meta _ ->
            Err (BadStatus meta.statusCode)

        Http.GoodStatus_ _ body ->
            case decodeString decoder body of
                Ok value ->
                    Ok value

                Err err ->
                    Err (BadBody (errorToString err))


sendHttpRequest : Maybe String -> (Result Error a -> msg) -> Request a -> Cmd msg
sendHttpRequest tracker tagger (Request builder) =
    let
        config =
            { method = builder.method
            , headers = builder.headers
            , url = buildFullUrl builder.url builder.queryParams
            , body = builder.body
            , expect = Http.expectStringResponse tagger (responseHandler (Decode.at builder.dataPath builder.decoder))
            , timeout = builder.timeout
            , tracker = tracker
            }
    in
    if builder.withCredentials then
        Http.riskyRequest config

    else
        Http.request config


{-| Send the Http request.
-}
send : (Result Error a -> msg) -> Request a -> Cmd msg
send =
    sendHttpRequest Nothing


{-| Send the Http request with tracker.
-}
sendWithTracker : String -> (Result Error a -> msg) -> Request a -> Cmd msg
sendWithTracker =
    sendHttpRequest << Just


toHttpTask : Maybe String -> Request a -> Task Error a
toHttpTask cacheBuster request =
    Task.andThen
        (\(Request builder) ->
            let
                config =
                    { method = builder.method
                    , headers = builder.headers
                    , url = buildFullUrl builder.url builder.queryParams
                    , body = builder.body
                    , resolver = Http.stringResolver (responseHandler (Decode.at builder.dataPath builder.decoder))
                    , timeout = builder.timeout
                    }
            in
            if builder.withCredentials then
                Http.riskyTask config

            else
                Http.task config
        )
        (case cacheBuster of
            Nothing ->
                Task.succeed request

            Just buster ->
                Task.map
                    ((|>) request << withQueryParam buster << String.fromInt << Time.posixToMillis)
                    Time.now
        )


{-| Convert the `Request` to a `Task` with all options applied.

    GraphQL.Selector.succeed Tuple.pair
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"
        |> toTask

    -- makes a task of request to https://example.com/graphql

-}
toTask : Request a -> Task Error a
toTask =
    toHttpTask Nothing


{-| Convert the `Request` to a `Task` with a Time based cache buster added to the URL.
You provide a key for an extra query param, and when the request is sent that
query param will be given a value with the current timestamp.

    GraphQL.Selector.succeed Tuple.pair
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"
        |> toTaskWithCacheBuster "cache_buster"

    -- makes a task of request to https://example.com/graphql?cache_buster=1481633217383

-}
toTaskWithCacheBuster : String -> Request a -> Task Error a
toTaskWithCacheBuster =
    toHttpTask << Just


buildFullUrl : String -> List ( String, String ) -> String
buildFullUrl url queryParams =
    if List.isEmpty queryParams then
        url

    else
        url ++ "?" ++ joinUrlEncoded queryParams


joinUrlEncoded : List ( String, String ) -> String
joinUrlEncoded args =
    String.join "&" (List.map queryPair args)


queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    queryEscape key ++ "=" ++ queryEscape value


queryEscape : String -> String
queryEscape =
    Url.percentEncode >> replace "%20" "+"


replace : String -> String -> String -> String
replace old new =
    String.split old >> String.join new

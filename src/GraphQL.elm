module GraphQL
    exposing
        ( Error
        , GraphQL
        , Request
        , get
        , mutation
        , post
        , query
        , render
        , send
        , subscription
        , toDecoder
        , toHttpRequest
        , toTask
        , withBearerToken
        , withCacheBuster
        , withCredentials
        , withDataDecoder
        , withHeader
        , withHeaders
        , withQueryParam
        , withQueryParams
        , withTimeout
        )

{-| Building and sending GraphQL.

Building of HTTP request has been based on [`elm-http-builder`](http://package.elm-lang.org/packages/lukewestby/elm-http-builder/latest).


# Build a GraphQL

@docs GraphQL, query, mutation, subscription, render, toDecoder


# Build a Request

@docs Request, get, post

@docs withHeader, withHeaders, withBearerToken, withQueryParam
@docs withQueryParams, withTimeout, withCredentials, withCacheBuster, withDataDecoder


# Make a Request

@docs Error, toHttpRequest, toTask, send

-}

import GraphQL.Internal as Internal
import GraphQL.Selector as Selector exposing (Selector)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Task exposing (Task)
import Time exposing (Time)


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


{-| Render GraphQL representation from a GraphQL.
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
        , dataDecoder : Decoder a -> Decoder a
        , timeout : Maybe Time
        , withCredentials : Bool
        , queryParams : List ( String, String )
        , cacheBuster : Maybe String
        }


requestBuilder : Bool -> String -> GraphQL a -> Request a
requestBuilder isGetMethod url graphql =
    let
        query =
            render graphql

        ( methodStr, queryParams, body ) =
            if isGetMethod then
                ( "GET"
                , [ ( "query", query ) ]
                , Http.emptyBody
                )
            else
                ( "POST"
                , []
                , [ ( "query", Encode.string query ) ]
                    |> Encode.object
                    |> Http.jsonBody
                )
    in
    Request
        { method = methodStr
        , url = url
        , headers = []
        , body = body
        , decoder = toDecoder graphql
        , dataDecoder = Decode.field "data"
        , timeout = Nothing
        , withCredentials = False
        , queryParams = queryParams
        , cacheBuster = Nothing
        }


{-| Start building a GET request with a given URL.

    GraphQL.Selector.succeed (,)
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"

-}
get : String -> GraphQL a -> Request a
get =
    requestBuilder True


{-| Start building a POST request with a given URL.

    GraphQL.Selector.succeed (,)
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"

-}
post : String -> GraphQL a -> Request a
post =
    requestBuilder False


{-| Add a single header to a request.

    GraphQL.Selector.succeed (,)
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

    GraphQL.Selector.succeed (,)
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
    Request { builder | headers = List.map (uncurry Http.header) headerPairs ++ builder.headers }


{-| Add a bearer token to a request.

    GraphQL.Selector.succeed (,)
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"
        |> withBearerToken "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJhIjoiYSJ9.MvhYYpYBuN1rUaV0GGnQGvr889zY0xSc20Lnt8nMTfE"

-}
withBearerToken : String -> Request a -> Request a
withBearerToken value (Request builder) =
    Request { builder | headers = Http.header "Authorization" ("Bearer " ++ value) :: builder.headers }


{-| Add a query param to the url for the request.

    GraphQL.Selector.succeed (,)
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

    GraphQL.Selector.succeed (,)
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

    GraphQL.Selector.succeed (,)
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"
        |> withTimeout (10 * Time.second)

-}
withTimeout : Time -> Request a -> Request a
withTimeout timeout (Request builder) =
    Request { builder | timeout = Just timeout }


{-| Set the `withCredentials` flag on the request to True. Works via
[`XMLHttpRequest#withCredentials`](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/withCredentials).

    GraphQL.Selector.succeed (,)
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"
        |> withCredentials True

-}
withCredentials : Bool -> Request a -> Request a
withCredentials with (Request builder) =
    Request { builder | withCredentials = with }


{-| Send the request with a Time based cache buster added to the URL.
You provide a key for an extra query param, and when the request is sent that
query param will be given a value with the current timestamp.

    type Msg
        = InitialData (Result Http.Error (User, List Article))

    GraphQL.Selector.succeed (,)
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"
        |> withCacheBuster "cache_buster"
        |> send InitialData

    -- makes a request to https://example.com/graphql?cache_buster=1481633217383

-}
withCacheBuster : String -> Request a -> Request a
withCacheBuster paramName (Request builder) =
    Request { builder | cacheBuster = Just paramName }


{-| Set a decoder of data container. By default it set as `Decode.field "data"`.

    GraphQL.Selector.succeed (,)
        |> GraphQL.Selector.field "me" [] userSelector
        |> GraphQL.Selector.field "articles" [] (GraphQL.Selector.list articleSelector)
        |> query "InitialData"
        |> get "https://example.com/graphql"
        |> withDataDecoder (Decode.at [ "my", "custom", "data", "path" ])

-}
withDataDecoder : (Decoder a -> Decoder a) -> Request a -> Request a
withDataDecoder dataDecoder (Request builder) =
    Request { builder | dataDecoder = dataDecoder }


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
type alias Error =
    Http.Error


{-| Extract the `Http.Request` component of the builder in case you want to use it directly.
**This function is lossy** and will discard some of the extra stuff that HttpBuilder allows you to do.

Things that will be lost:

  - Attaching a cache buster to requests using `withCacheBuster`

-}
toHttpRequest : Request a -> Http.Request a
toHttpRequest (Request builder) =
    let
        fullUrl =
            case joinUrlEncoded builder.queryParams of
                "" ->
                    builder.url

                queryString ->
                    builder.url ++ "?" ++ queryString
    in
    Http.request
        { method = builder.method
        , url = fullUrl
        , headers = builder.headers
        , body = builder.body
        , expect = Http.expectJson (builder.dataDecoder builder.decoder)
        , timeout = builder.timeout
        , withCredentials = builder.withCredentials
        }


toPlainTask : Request a -> Task Error a
toPlainTask =
    Http.toTask << toHttpRequest


{-| Convert the `Request` to a `Task` with all options applied.
`toTask` differs from `toRequest` in that it retains all extra behavior allowed by
HttpBuilder, including:

  - Attaching a cache buster to requests using `withCacheBuster`

-}
toTask : Request a -> Task Error a
toTask (Request builder) =
    case builder.cacheBuster of
        Nothing ->
            toPlainTask (Request builder)

        Just buster ->
            Time.now
                |> Task.map (flip (withQueryParam buster) (Request builder) << toString)
                |> Task.andThen toPlainTask


{-| Send the request.
-}
send : (Result Error a -> msg) -> Request a -> Cmd msg
send tagger request =
    Task.attempt tagger (toTask request)


joinUrlEncoded : List ( String, String ) -> String
joinUrlEncoded args =
    String.join "&" (List.map queryPair args)


queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    queryEscape key ++ "=" ++ queryEscape value


queryEscape : String -> String
queryEscape =
    Http.encodeUri >> replace "%20" "+"


replace : String -> String -> String -> String
replace old new =
    String.split old >> String.join new

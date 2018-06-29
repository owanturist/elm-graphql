module GraphQL
    exposing
        ( Error
        , GraphQL
        , Request
        , get
        , mutation
        , post
        , query
        , select
        , send
        , subscription
        , toHttpRequest
        , toTask
        , withBearerToken
        , withCacheBuster
        , withCredentials
        , withHeader
        , withHeaders
        , withQueryParam
        , withQueryParams
        , withTimeout
        )

{-| Building and sending GraphQL


# Build a GraphQL

@docs GraphQL, query, mutation, subscription, select


# Build a Request

@docs Request, get, post

@docs withHeader, withHeaders, withBearerToken, withQueryParam
@docs withQueryParams, withTimeout, withCredentials, withCacheBuster


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


{-| -}
query : String -> Selector a -> GraphQL a
query =
    GraphQL "query"


{-| -}
mutation : String -> Selector a -> GraphQL a
mutation =
    GraphQL "mutation"


{-| -}
subscription : String -> Selector a -> GraphQL a
subscription =
    GraphQL "subscription"


{-| -}
select : GraphQL a -> ( Maybe String, Decoder a )
select (GraphQL operation name selector) =
    case Selector.select selector of
        ( Nothing, decoder ) ->
            ( Nothing, decoder )

        ( Just selector, decoder ) ->
            ( Just (operation ++ " " ++ name ++ Internal.wrap "{" "}" selector)
            , decoder
            )


type Method
    = Get
    | Post


{-| -}
type alias Request a =
    { method : String
    , url : String
    , headers : List Http.Header
    , body : Http.Body
    , expect : Http.Expect a
    , timeout : Maybe Time
    , withCredentials : Bool
    , queryParams : List ( String, String )
    , cacheBuster : Maybe String
    }


requestBuilder : Method -> String -> GraphQL a -> Request a
requestBuilder method url graphql =
    let
        ( query, decoder ) =
            select graphql

        ( methodStr, queryParams, body ) =
            case method of
                Get ->
                    ( "GET"
                    , [ Maybe.map ((,) "query") query ]
                        |> List.filterMap identity
                    , Http.emptyBody
                    )

                Post ->
                    ( "POST"
                    , []
                    , [ Maybe.map ((,) "query" << Encode.string) query
                      ]
                        |> List.filterMap identity
                        |> Encode.object
                        |> Http.jsonBody
                    )
    in
    { method = methodStr
    , url = url
    , headers = []
    , body = body
    , expect = Http.expectJson (Decode.field "data" decoder)
    , timeout = Nothing
    , withCredentials = False
    , queryParams = queryParams
    , cacheBuster = Nothing
    }


{-| -}
get : String -> GraphQL a -> Request a
get =
    requestBuilder Get


{-| -}
post : String -> GraphQL a -> Request a
post =
    requestBuilder Post


{-| -}
withHeader : String -> String -> Request a -> Request a
withHeader key value builder =
    { builder | headers = Http.header key value :: builder.headers }


{-| -}
withHeaders : List ( String, String ) -> Request a -> Request a
withHeaders headerPairs builder =
    { builder | headers = List.map (uncurry Http.header) headerPairs ++ builder.headers }


{-| -}
withBearerToken : String -> Request a -> Request a
withBearerToken value builder =
    { builder | headers = Http.header "Authorization" ("Bearer " ++ value) :: builder.headers }


{-| -}
withQueryParam : String -> String -> Request a -> Request a
withQueryParam key value builder =
    { builder | queryParams = builder.queryParams ++ [ ( key, value ) ] }


{-| -}
withQueryParams : List ( String, String ) -> Request a -> Request a
withQueryParams queryParams builder =
    { builder | queryParams = builder.queryParams ++ queryParams }


{-| -}
withTimeout : Time -> Request a -> Request a
withTimeout timeout builder =
    { builder | timeout = Just timeout }


{-| -}
withCredentials : Bool -> Request a -> Request a
withCredentials with builder =
    { builder | withCredentials = with }


{-| -}
withCacheBuster : String -> Request a -> Request a
withCacheBuster paramName builder =
    { builder | cacheBuster = Just paramName }


{-| -}
type alias Error =
    Http.Error


{-| -}
toHttpRequest : Request a -> Http.Request a
toHttpRequest builder =
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
        , expect = builder.expect
        , timeout = builder.timeout
        , withCredentials = builder.withCredentials
        }


toPlainTask : Request a -> Task Error a
toPlainTask =
    Http.toTask << toHttpRequest


{-| -}
toTask : Request a -> Task Error a
toTask builder =
    case builder.cacheBuster of
        Nothing ->
            toPlainTask builder

        Just buster ->
            Time.now
                |> Task.map (flip (withQueryParam buster) builder << toString)
                |> Task.andThen toPlainTask


{-| -}
send : (Result Error a -> msg) -> Request a -> Cmd msg
send tagger builder =
    Task.attempt tagger (toTask builder)


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

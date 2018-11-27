module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Card exposing (..)
import Html exposing (..)
import Http
import Json.Decode as Decode



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = \model -> Browser.Document "Move In!" [ view model ]
        }



-- MODEL


type alias Model =
    { cards : List Card
    , debug : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] ""
    , getCards
    )


getCards : Cmd Msg
getCards =
    Http.get
        { url = "https://move-in-printer.now.sh/api"
        , expect = Http.expectJson ReceiveData Decode.string
        }



-- UPDATE


type Msg
    = NoOp
    | ReceiveData (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveData result ->
            case result of
                Ok yay ->
                    ( { model | debug = yay }, Cmd.none )

                Err err ->
                    case err of
                        Http.BadUrl string ->
                            ( { model | debug = string }, Cmd.none )

                        Http.Timeout ->
                            ( { model | debug = "TIMEOUT" }, Cmd.none )

                        Http.NetworkError ->
                            ( { model | debug = "NETWORK ERROR" }, Cmd.none )

                        Http.BadStatus int ->
                            ( { model | debug = String.fromInt int }, Cmd.none )

                        Http.BadBody string ->
                            ( { model | debug = string }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Html.text model.debug



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Card exposing (..)
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Icon as Icon
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Styles
import FontAwesome.Transforms as Icon
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import List.Extra as List



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
        { url = "http://localhost:3000"
        , expect = Http.expectJson ReceiveData decoder
        }



-- UPDATE


decoder : Decoder (List Card)
decoder =
    Decode.field "cards" (Decode.list Card.decode)


type Msg
    = NoOp
    | ReceiveData (Result Http.Error (List Card))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveData result ->
            case result of
                Ok cards ->
                    let
                        text =
                            List.map Card.title cards
                                |> String.concat
                                |> Debug.log "Card Titles"
                    in
                    ( { model | cards = cards }, Cmd.none )

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
    div []
        [ FontAwesome.Styles.css
        , viewCards model
        ]


viewCards : Model -> Html Msg
viewCards model =
    model.cards
        |> List.map renderCard
        |> List.groupsOf 12
        |> List.intercalate [ div [ class "page-break" ] [] ]
        |> Html.div [ class "wrapper" ]


renderCard : Card -> Html Msg
renderCard card =
    Html.div [ class "card" ]
        [ Html.div [ class "card-title" ] [ Html.text <| Card.title card ] ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Card exposing (..)
import FontAwesome.Icon as Icon
import FontAwesome.Styles
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import List.Extra as List


numPrintedPerPage =
    9



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
        --"http://localhost:3000"
        { url = "https://move-in-printer.now.sh/"
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
        |> List.map viewCard
        |> List.groupsOf numPrintedPerPage
        |> List.intercalate [ div [ class "page-break" ] [] ]
        |> Html.div [ class "wrapper" ]


viewCard : Card -> Html Msg
viewCard card =
    case card of
        ItemCard name category stats ->
            Html.div [ class "card", class "item" ]
                [ viewCardTitle <| Card.title card
                , viewCategories <| Card.categories card
                , viewCardRooms <| Card.rooms card
                ]

        ActionCard name item ->
            Html.div [ class "card action" ]
                [ viewCardTitle <| Card.title card ]


viewCardRooms : List Card.Room -> Html Msg
viewCardRooms rooms =
    rooms
        |> List.map viewRoom
        |> div [ class "card-rooms" ]


viewRoom : Card.Room -> Html Msg
viewRoom room =
    Html.div [ class (Card.roomClass room), class "card-room" ]
        [ Html.text <| Card.roomName room ]


viewCategories : List Card.Category -> Html Msg
viewCategories categories =
    categories
        |> List.map viewCategory
        |> div [ class "card-categories" ]


viewCategory : Card.Category -> Html Msg
viewCategory category =
    Html.div [ class "card-category" ] <|
        case Card.categoryIcon category of
            Just ( emoji, label ) ->
                [ Html.div [ class "category-emoji" ] [ Html.text emoji ]
                , Html.div [ class "category-label" ] [ Html.text label ]
                ]

            Nothing ->
                [ nothing ]


viewCardTitle : String -> Html Msg
viewCardTitle string =
    Html.div
        [ class "card-title" ]
        [ Html.text <| string ]


nothing : Html Msg
nothing =
    Html.text ""



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

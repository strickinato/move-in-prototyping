module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Card exposing (..)
import Card.Room as Room exposing (..)
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
    , debug : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] Nothing
    , getCards
    )


getCards : Cmd Msg
getCards =
    Http.get
        -- { url = "https://move-in-printer.now.sh/api"
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
                    in
                    ( { model | cards = cards }, Cmd.none )

                Err err ->
                    case err of
                        Http.BadUrl string ->
                            ( { model | debug = Just string }, Cmd.none )

                        Http.Timeout ->
                            ( { model | debug = Just "TIMEOUT" }, Cmd.none )

                        Http.NetworkError ->
                            ( { model | debug = Just "NETWORK ERROR" }, Cmd.none )

                        Http.BadStatus int ->
                            ( { model | debug = Just <| String.fromInt int }, Cmd.none )

                        Http.BadBody string ->
                            ( { model | debug = Just string }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ FontAwesome.Styles.css
        , viewDebug model.debug
        , viewCards model
        ]


viewDebug : Maybe String -> Html Msg
viewDebug maybeString =
    case maybeString of
        Just string ->
            Html.text string

        Nothing ->
            nothing


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
        ItemCard (Name name) (Space space) category maybeNotes stats ->
            Html.div [ class "card", class "item" ]
                [ Html.div [ class "card-header" ]
                    [ viewCardRooms <| Card.roomsWithPoints card ]
                , Html.div [ class "card-content" ]
                    [ Html.div [ class "card-category-border" ]
                        (Card.categories card
                            |> List.repeat 8
                            |> List.concat
                            |> List.take 8
                            |> List.map viewCategory
                        )
                    , Html.div [ class "card-main" ]
                        [ viewCardTitle name
                        , viewCardSpace space
                        , viewCardDescription maybeNotes
                        ]
                    ]
                ]

        ActionCard (Name name) (Action notes) ->
            Html.div [ class "card", class "action" ]
                [ viewCardTitle name
                , viewCardDescription (Just notes)
                ]


viewCardSpace : Int -> Html Msg
viewCardSpace numSpaces =
    let
        boxes =
            if numSpaces > 0 then
                List.repeat numSpaces <|
                    Html.div [ class "card-space-square" ] [ Html.text "+" ]

            else if numSpaces < 0 then
                List.repeat (abs numSpaces) <|
                    Html.div [ class "card-space-square" ] [ Html.text "-" ]

            else
                [ Html.text "NO SPACE!" ]
    in
    Html.div [ class "card-space-points" ] boxes


viewCardDescription : Maybe String -> Html Msg
viewCardDescription maybeString =
    let
        render string =
            Html.div [ class "card action-description" ]
                [ Html.text string ]
    in
    Maybe.map render maybeString
        |> Maybe.withDefault nothing


viewCardRooms : List ( Room, Int ) -> Html Msg
viewCardRooms rooms =
    rooms
        |> List.map viewRoom
        |> div [ class "card-rooms" ]


viewRoom : ( Room, Int ) -> Html Msg
viewRoom ( room, qualityPoints ) =
    Html.div [ class <| Room.className room, class "card-room" ]
        [ div [ class "card-room-name" ] [ Html.text <| Room.displayName room ]
        , div [ class "card-room-points" ] [ Html.text <| String.fromInt qualityPoints ]
        ]


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

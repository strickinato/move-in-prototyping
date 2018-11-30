module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Card exposing (..)
import Card.Room as Room exposing (..)
import Dict.Any as AnyDict exposing (AnyDict)
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


type alias CardData =
    { cards : List Card
    , categoriesByRoom : AnyDict String Room (List Category)
    }


type Model
    = Fetching
    | Error String
    | Success CardData


init : () -> ( Model, Cmd Msg )
init _ =
    ( Fetching
    , getCards
    )


getCards : Cmd Msg
getCards =
    Http.get
        -- { url = "http://localhost:3000"
        { url = "https://move-in-printer.now.sh/api"
        , expect = Http.expectJson ReceiveData decoder
        }



-- UPDATE


decoder : Decoder CardData
decoder =
    Decode.succeed CardData
        |> Pipeline.required "cards" (Decode.list Card.decode)
        |> Pipeline.required "categoriesByRoom" Card.categoriesByRoomDecoder


type Msg
    = NoOp
    | ReceiveData (Result Http.Error CardData)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveData result ->
            case result of
                Ok cardData ->
                    ( Success cardData, Cmd.none )

                Err err ->
                    case err of
                        Http.BadUrl string ->
                            ( Error string, Cmd.none )

                        Http.Timeout ->
                            ( Error "TIMEOUT", Cmd.none )

                        Http.NetworkError ->
                            ( Error "NETWORK ERROR", Cmd.none )

                        Http.BadStatus int ->
                            ( Error <| String.fromInt int, Cmd.none )

                        Http.BadBody string ->
                            ( Error string, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Fetching ->
            Html.text "GETTING THE Card Data..."

        Error str ->
            Html.div [ style "color" "red" ] [ Html.text str ]

        Success cardData ->
            div []
                [ FontAwesome.Styles.css
                , viewCards cardData
                ]


viewCards : CardData -> Html Msg
viewCards cardData =
    cardData.cards
        |> List.concatMap Card.expandForNum
        |> List.map viewCard
        |> List.append (viewCheatSheets 5 cardData.categoriesByRoom)
        |> List.groupsOf numPrintedPerPage
        |> List.intercalate [ div [ class "page-break" ] [] ]
        |> Html.div [ class "wrapper" ]


viewCard : Card -> Html Msg
viewCard card =
    case card of
        ItemCard (Name name) num (Space space) category maybeNotes stats vibe ->
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
                        , viewVibe vibe

                        -- , viewCardDescription maybeNotes
                        ]
                    ]
                ]

        ActionCard (Name name) num (Action notes) ->
            Html.div [ class "card", class "action" ]
                [ viewCardTitle name
                , viewCardDescription (Just notes)
                ]


viewVibe : Vibe -> Html Msg
viewVibe vibe =
    case vibe of
        Fancy ->
            Html.div [ class "vibe-text" ] [ Html.text "Fancy" ]

        WhiteTrash ->
            Html.div [ class "vibe-text" ] [ Html.text "White Trash" ]

        NoVibe ->
            nothing


viewCardSpace : Int -> Html Msg
viewCardSpace numSpaces =
    let
        boxes =
            if numSpaces > 0 then
                List.repeat numSpaces <|
                    Html.div
                        [ class "card-space-square"
                        , class "positive"
                        ]
                        [ Html.text "+" ]

            else if numSpaces < 0 then
                List.repeat (abs numSpaces) <|
                    Html.div
                        [ class "card-space-square"
                        , class "negative"
                        ]
                        [ Html.text "-" ]

            else
                [ Html.text "NO SPACE!" ]

        spaceText =
            if numSpaces > 0 then
                String.concat
                    [ "+"
                    , String.fromInt numSpaces
                    , " "
                    , pluralize "space" "spaces" numSpaces
                    , ":"
                    ]

            else if numSpaces < 0 then
                String.concat
                    [ String.fromInt numSpaces
                    , " "
                    , pluralize "space" "spaces" numSpaces
                    , ":"
                    ]

            else
                ""
    in
    Html.div [ class "card-space-points" ]
        [ Html.div [ class "card-space-point-label" ]
            [ Html.text spaceText ]
        , Html.div [ class "card-space-point-boxes" ] boxes
        ]


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
            Just ( emoji, _ ) ->
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


viewCheatSheets : Int -> AnyDict String Room (List Category) -> List (Html Msg)
viewCheatSheets numCheatSheets categoriesByRoom =
    let
        renderCat ( emoji, label ) =
            Html.li []
                [ Html.text emoji
                , Html.text label
                ]

        renderRoom ( room, cats ) =
            Html.li []
                [ Html.span [ class "room-name" ] [ Html.text <| Room.displayName room ]
                , Html.ul []
                    (cats
                        |> List.filterMap Card.categoryIcon
                        |> List.sortBy Tuple.second
                        |> List.map renderCat
                    )
                ]
    in
    categoriesByRoom
        |> AnyDict.toList
        |> List.map renderRoom
        |> Html.div [ class "card", class "cheat-sheet" ]
        |> List.repeat numCheatSheets


pluralize : String -> String -> Int -> String
pluralize singular plural num =
    if abs num == 1 then
        singular

    else
        plural

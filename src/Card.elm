module Card exposing (Action(..), Card(..), Category(..), Item(..), Name(..), Quality(..), Room(..), Rules, Space(..), Stats(..), Vibe(..), decode, decodeRoom, roomDecoder, title)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline as Pipeline


type Card
    = ItemCard Name Item
    | ActionCard Name Action


type Item
    = Any Stats
    | Single Room Stats
    | Multiple (List ( Room, Stats ))


type Name
    = Name String


type Action
    = Action Rules


type alias Rules =
    String


type Stats
    = Stats Quality Space Category Vibe Rules


type Quality
    = Quality Int


type Space
    = Space Int


type Room
    = Bathroom
    | Bedroom
    | LivingRoom
    | Kitchen


type Category
    = NoCategory
    | Toilet
    | Bathing
    | Sink
    | Bed
    | TableSpace
    | Dresser
    | CookingDevice
    | ColdFoodStorage
    | Seating
    | Decor


type Vibe
    = NoVibe
    | WhiteTrash
    | Fancy


title : Card -> String
title card =
    case card of
        ItemCard (Name name) _ ->
            name

        ActionCard (Name name) _ ->
            name


decode : Decoder Card
decode =
    Decode.field "_type" Decode.string
        |> Decode.andThen typeDecode


typeDecode : String -> Decoder Card
typeDecode string =
    case string of
        "item" ->
            itemCardDecoder

        "action" ->
            actionCardDecoder

        _ ->
            ("Not a correct type of card: " ++ string)
                |> Decode.fail


itemCardDecoder : Decoder Card
itemCardDecoder =
    Decode.succeed (\a -> ItemCard a (Any defaultStats))
        |> Pipeline.required "Card Title" nameDecoder


actionCardDecoder : Decoder Card
actionCardDecoder =
    Decode.succeed ActionCard
        |> Pipeline.required "Name" nameDecoder
        |> Pipeline.required "Description" actionDecoder


actionDecoder : Decoder Action
actionDecoder =
    Decode.map Action Decode.string


nameDecoder : Decoder Name
nameDecoder =
    Decode.map Name Decode.string


defaultStats : Stats
defaultStats =
    Stats (Quality 1) (Space -1) NoCategory NoVibe "Hi"


decodeRoom : Decoder Room
decodeRoom =
    Decode.string
        |> Decode.andThen roomDecoder


roomDecoder : String -> Decoder Room
roomDecoder string =
    case string of
        "Bathroom" ->
            Decode.succeed Bathroom

        "Bedroom" ->
            Decode.succeed Bedroom

        "Kitchen" ->
            Decode.succeed Kitchen

        "Living Room" ->
            Decode.succeed LivingRoom

        _ ->
            ("String for room doesn't match: " ++ string)
                |> Decode.fail

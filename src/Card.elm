module Card exposing
    ( Action(..)
    , Card(..)
    , Category(..)
    , Item(..)
    , Name(..)
    , Quality(..)
    , Room(..)
    , Rules
    , Space(..)
    , Stats(..)
    , Vibe(..)
    , actionCardDecoder
    , actionDecoder
    , anyDecoder
    , decode
    , decodeRoom
    , defaultStats
    , itemCardDecoder
    , itemDecoder
    , multipleDecoder
    , nameDecoder
    , roomClass
    , roomDecoder
    , roomName
    , rooms
    , roomsDecoder
    , singleDecoder
    , title
    , toRoom
    , typeDecode
    )

import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Transforms as Icon
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline as Pipeline
import Result.Extra as Result


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
    | AnyRoom


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


roomClass : Room -> String
roomClass room =
    case room of
        Bathroom ->
            "bathroom"

        Bedroom ->
            "bedroom"

        LivingRoom ->
            "living-room"

        Kitchen ->
            "kitchen"

        AnyRoom ->
            "any-room"


roomName : Room -> String
roomName room =
    case room of
        Bathroom ->
            "Bathroom"

        Bedroom ->
            "Bedroom"

        LivingRoom ->
            "Living Room"

        Kitchen ->
            "Kitchen"

        AnyRoom ->
            "Any"


rooms : Card -> List Room
rooms card =
    case card of
        ItemCard _ item ->
            case item of
                Any _ ->
                    [ AnyRoom ]

                Single room _ ->
                    [ room ]

                Multiple roomStuff ->
                    roomStuff
                        |> List.map Tuple.first

        ActionCard name item ->
            []


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
    Decode.succeed ItemCard
        |> Pipeline.required "Card Title" nameDecoder
        |> Pipeline.custom itemDecoder


itemDecoder : Decoder Item
itemDecoder =
    Decode.field "rooms" (Decode.list Decode.string)
        |> Decode.andThen roomsDecoder


roomsDecoder : List String -> Decoder Item
roomsDecoder roomStrings =
    case roomStrings of
        [] ->
            anyDecoder

        [ single ] ->
            singleDecoder single

        multiple ->
            multipleDecoder multiple


anyDecoder : Decoder Item
anyDecoder =
    Decode.succeed (Any defaultStats)


singleDecoder : String -> Decoder Item
singleDecoder single =
    Decode.map (\r -> Single r defaultStats) (roomDecoder single)


multipleDecoder : List String -> Decoder Item
multipleDecoder multiple =
    let
        intoMultiple : List Room -> Item
        intoMultiple listRooms =
            List.map (\room -> ( room, defaultStats )) listRooms
                |> Multiple

        decodeResult : Result String Item -> Decoder Item
        decodeResult result =
            case result of
                Ok item ->
                    Decode.succeed item

                Err err ->
                    Decode.fail err
    in
    multiple
        |> List.map toRoom
        |> Result.combine
        |> Result.map intoMultiple
        |> decodeResult



-- Result x (List Room)
-- let
--     fn : List String -> Decoder (List Room)
--     fn a =
--         Decode.succeed a
--             |> Decode.map
-- in
-- Decode.andThen fn (Decode.list decodeRoom)
--     |> Decode.map intoMultiple
-- let
-- in
-- Decode.map intoMultiple (Decode.list decodeRoom)


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


toRoom : String -> Result String Room
toRoom string =
    case string of
        "Bathroom" ->
            Ok Bathroom

        "Bedroom" ->
            Ok Bedroom

        "Kitchen" ->
            Ok Kitchen

        "Living Room" ->
            Ok LivingRoom

        "Any" ->
            Ok AnyRoom

        _ ->
            Err ("String for room doesn't match: " ++ string)


roomDecoder : String -> Decoder Room
roomDecoder string =
    case toRoom string of
        Ok room ->
            Decode.succeed room

        Err msg ->
            Decode.fail msg

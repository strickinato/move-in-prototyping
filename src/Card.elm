module Card exposing
    ( Action(..)
    , Card(..)
    , Category(..)
    , Item(..)
    , Name(..)
    , Quality(..)
    , Rules
    , Space(..)
    , Stats(..)
    , Vibe(..)
    , actionCardDecoder
    , actionDecoder
    , anyDecoder
    , categories
    , categoryDecoder
    , categoryIcon
    , decode
    , decodeCategory
    , defaultStats
    , description
    , itemCardDecoder
    , itemDecoder
    , multipleDecoder
    , nameDecoder
    , roomsDecoder
    , roomsWithPoints
    , singleDecoder
    , title
    , toCategory
    , typeDecode
    )

import Card.Room as Room exposing (Room(..))
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Icon exposing (Icon)
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Transforms as Icon
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline as Pipeline
import Result.Extra as Result


type Card
    = ItemCard Name (List Category) (Maybe Notes) Item
    | ActionCard Name Action


type Item
    = Any Stats
    | Single Room Stats
    | Multiple (List ( Room, Stats ))


type Name
    = Name String


type Action
    = Action Rules


type alias Notes =
    String


type alias Rules =
    String


type Stats
    = Stats Quality Space


type Quality
    = Quality Int


type Space
    = Space Int


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
        ItemCard (Name name) _ _ _ ->
            name

        ActionCard (Name name) _ ->
            name


roomsWithPoints : Card -> List ( Room, Int )
roomsWithPoints card =
    case card of
        ItemCard _ _ _ item ->
            case item of
                Any (Stats (Quality quality) _) ->
                    [ ( AnyRoom, quality ) ]

                Single room (Stats (Quality quality) _) ->
                    [ ( room, quality ) ]

                Multiple roomStuff ->
                    let
                        deconstruct ( room, Stats (Quality quality) _ ) =
                            ( room, quality )
                    in
                    List.map deconstruct roomStuff

        ActionCard name item ->
            []


description : Card -> Maybe String
description card =
    case card of
        ItemCard _ _ maybeNotes _ ->
            maybeNotes

        ActionCard _ (Action string) ->
            Just string


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
        |> Pipeline.required "categories" (Decode.list categoryDecoder)
        |> Pipeline.optional "Rules" (Decode.nullable notesDecoder) Nothing
        |> Pipeline.custom itemDecoder


notesDecoder : Decoder Notes
notesDecoder =
    Decode.string


itemDecoder : Decoder Item
itemDecoder =
    Decode.field "rooms" (Decode.list Decode.string)
        |> Decode.andThen roomsDecoder


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
    Stats (Quality 1) (Space -1)


categoryDecoder : Decoder Category
categoryDecoder =
    Decode.string
        |> Decode.andThen decodeCategory


decodeCategory : String -> Decoder Category
decodeCategory string =
    case toCategory string of
        Ok category ->
            Decode.succeed category

        Err msg ->
            Decode.fail msg


toCategory : String -> Result String Category
toCategory string =
    case string of
        "Cooking Device" ->
            Ok CookingDevice

        "Cold Food Storage" ->
            Ok ColdFoodStorage

        "Toilet" ->
            Ok Toilet

        "Bathing" ->
            Ok Bathing

        "Sink" ->
            Ok Sink

        "Decor" ->
            Ok Decor

        "Seating" ->
            Ok Seating

        "Table Space" ->
            Ok TableSpace

        "Bed" ->
            Ok Bed

        "Dresser" ->
            Ok Dresser

        "None" ->
            Ok NoCategory

        _ ->
            Err ("String for category doesn't match: " ++ string)


categories : Card -> List Category
categories card =
    case card of
        ItemCard _ categoryList _ _ ->
            categoryList

        ActionCard _ _ ->
            []


categoryIcon : Category -> Maybe ( String, String )
categoryIcon category =
    case category of
        NoCategory ->
            Nothing

        Toilet ->
            Just ( "🚽", "toilet" )

        Bathing ->
            Just ( "🛀", "bathing" )

        Sink ->
            Just ( "🚰", "sink" )

        Bed ->
            Just ( "🛌", "bed" )

        TableSpace ->
            Just ( "🛄", "table" )

        Dresser ->
            Just ( "🗄️", "dresser" )

        CookingDevice ->
            Just ( "🍳", "cooking device" )

        ColdFoodStorage ->
            Just ( "❄", "cold food storage" )

        Seating ->
            Just ( "💺", "seating" )

        Decor ->
            Just ( "🖼️", "decor" )


statsDecoder : Decoder Stats
statsDecoder =
    Decode.succeed Stats
        |> Pipeline.required "Quality Points" qualityDecoder
        |> Pipeline.required "Space Points" spaceDecoder


qualityDecoder : Decoder Quality
qualityDecoder =
    Decode.map Quality Decode.int


spaceDecoder : Decoder Space
spaceDecoder =
    Decode.map Space Decode.int


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
    Decode.map (\r -> Single r defaultStats) (Room.decode single)


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
        |> List.map Room.fromString
        |> Result.combine
        |> Result.map intoMultiple
        |> decodeResult

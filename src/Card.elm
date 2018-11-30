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
    , categoriesByRoomDecoder
    , categoryDecoder
    , categoryIcon
    , decode
    , decodeCategory
    , defaultStats
    , expandForNum
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
import Dict
import Dict.Any as AnyDict exposing (AnyDict)
import FontAwesome.Attributes as Icon
import FontAwesome.Brands as Icon
import FontAwesome.Icon exposing (Icon)
import FontAwesome.Layering as Icon
import FontAwesome.Solid as Icon
import FontAwesome.Transforms as Icon
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline as Pipeline
import Maybe.Extra as Maybe
import Result.Extra as Result


type Card
    = ItemCard Name Number Space (List Category) (Maybe Notes) Item Vibe
    | ActionCard Name Number Action


type Item
    = Any Stats
    | Single Room Stats
    | Multiple (List ( Room, Stats ))


type alias Number =
    Int


type Name
    = Name String


type Action
    = Action Rules


type alias Notes =
    String


type alias Rules =
    String


type Stats
    = Stats Quality


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
        ItemCard (Name name) _ _ _ _ _ _ ->
            name

        ActionCard (Name name) _ _ ->
            name


number : Card -> Int
number card =
    case card of
        ItemCard _ num _ _ _ _ _ ->
            num

        ActionCard _ num _ ->
            num


categoriesByRoomDecoder : Decoder (AnyDict String Room (List Category))
categoriesByRoomDecoder =
    let
        resolve result =
            case result of
                Ok anyDict ->
                    Decode.succeed anyDict

                Err str ->
                    Decode.fail str

        toAnyDict dict =
            Dict.keys dict
                |> List.map Room.fromString
                |> Result.combine
                |> Result.map
                    (\x ->
                        List.map2 (\a b -> ( b, a )) (Dict.values dict) x
                    )
                |> Result.map (\x -> AnyDict.fromList Room.asKey x)
                |> resolve
    in
    Decode.dict (Decode.list categoryDecoder)
        |> Decode.andThen toAnyDict


expandForNum : Card -> List Card
expandForNum card =
    List.repeat (number card) card


roomsWithPoints : Card -> List ( Room, Int )
roomsWithPoints card =
    case card of
        ItemCard _ _ _ _ _ item _ ->
            case item of
                Any (Stats (Quality quality)) ->
                    [ ( AnyRoom, quality ) ]

                Single room (Stats (Quality quality)) ->
                    [ ( room, quality ) ]

                Multiple roomStuff ->
                    let
                        deconstruct ( room, Stats (Quality quality) ) =
                            ( room, quality )
                    in
                    List.map deconstruct roomStuff

        ActionCard _ _ _ ->
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
        |> Pipeline.required "Number" Decode.int
        |> Pipeline.required "Space Points" spaceDecoder
        |> Pipeline.required "categories" (Decode.list categoryDecoder)
        |> Pipeline.optional "Rules" (Decode.nullable notesDecoder) Nothing
        |> Pipeline.custom itemDecoder
        |> Pipeline.optional "Vibe" vibeDecoder NoVibe


vibeDecoder : Decoder Vibe
vibeDecoder =
    let
        resolve result =
            case result of
                Ok vibe ->
                    Decode.succeed vibe

                Err error ->
                    Decode.fail error
    in
    Decode.string
        |> Decode.andThen (resolve << vibeFromString)


vibeFromString : String -> Result String Vibe
vibeFromString string =
    case string of
        "Fancy" ->
            Ok Fancy

        "White Trash" ->
            Ok WhiteTrash

        str ->
            Err "Not a legit vibe!"


notesDecoder : Decoder Notes
notesDecoder =
    Decode.string


itemDecoder : Decoder Item
itemDecoder =
    let
        resolve : Result String Item -> Decoder Item
        resolve result =
            case result of
                Ok item ->
                    Decode.succeed item

                Err error ->
                    Decode.fail error
    in
    Decode.map2 toItem listRoomDecoder listIntDecoder
        |> Decode.andThen resolve



-- Decoder (List Int)


listIntDecoder : Decoder (List Int)
listIntDecoder =
    Decode.field "Quality Points" Decode.string
        |> Decode.andThen toIntList


toIntList : String -> Decoder (List Int)
toIntList string =
    let
        resolve maybeInts =
            case maybeInts of
                Just ints ->
                    Decode.succeed ints

                Nothing ->
                    Decode.fail "Something weird went down with the quality points"
    in
    string
        |> String.split ","
        |> List.map String.toInt
        |> Maybe.combine
        |> resolve


listRoomDecoder : Decoder (List Room)
listRoomDecoder =
    Decode.field "rooms" (Decode.list Decode.string)
        |> Decode.andThen roomsDecoder


roomsDecoder : List String -> Decoder (List Room)
roomsDecoder strings =
    let
        resolve list =
            case list of
                Ok rooms ->
                    Decode.succeed rooms

                Err error ->
                    Decode.fail error
    in
    strings
        |> List.map Room.fromString
        |> Result.combine
        |> resolve


actionCardDecoder : Decoder Card
actionCardDecoder =
    Decode.succeed ActionCard
        |> Pipeline.required "Name" nameDecoder
        |> Pipeline.required "Number" Decode.int
        |> Pipeline.required "Description" actionDecoder


actionDecoder : Decoder Action
actionDecoder =
    Decode.map Action Decode.string


nameDecoder : Decoder Name
nameDecoder =
    Decode.map Name Decode.string


defaultStats : Stats
defaultStats =
    Stats (Quality 1)


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
        ItemCard _ _ _ categoryList _ _ _ ->
            categoryList

        ActionCard _ _ _ ->
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
            Just ( "🛄", "table space" )

        Dresser ->
            Just ( "🗄️", "dresser" )

        CookingDevice ->
            Just ( "🍳", "cooking device" )

        ColdFoodStorage ->
            Just ( "❄ ", "cold food storage" )

        Seating ->
            Just ( "💺", "seating" )

        Decor ->
            Just ( "🖼️", "decor" )


statsDecoder : Decoder Stats
statsDecoder =
    Decode.succeed Stats
        |> Pipeline.required "Quality Points" qualityDecoder


qualityDecoder : Decoder Quality
qualityDecoder =
    Decode.map Quality Decode.int


spaceDecoder : Decoder Space
spaceDecoder =
    let
        convert str =
            case String.toInt str of
                Just int ->
                    Decode.map Space <| Decode.succeed int

                Nothing ->
                    Decode.fail
                        ("What kind of space points are these?" ++ str)
    in
    Decode.string
        |> Decode.andThen convert


toItem : List Room -> List Int -> Result String Item
toItem rooms ints =
    case ( rooms, ints ) of
        ( [], [ int ] ) ->
            Ok <| Any (Stats (Quality int))

        ( [ room ], [ int ] ) ->
            Ok <| Single room (Stats (Quality int))

        ( rooms_, [ int ] ) ->
            rooms_
                |> List.map (\r -> ( r, Stats (Quality int) ))
                |> Multiple
                |> Ok

        ( rooms_, ints_ ) ->
            let
                stats =
                    List.map (\i -> Stats (Quality i)) ints_
            in
            List.map2 (\a b -> ( a, b )) rooms_ stats
                |> Multiple
                |> Ok


oldRoomsDecoder : List String -> Decoder Item
oldRoomsDecoder roomStrings =
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

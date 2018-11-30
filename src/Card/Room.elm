module Card.Room exposing
    ( Room(..)
    , asKey
    , className
    , decode
    , displayName
    , fromString
    )

import Json.Decode as Decode exposing (Decoder)


type Room
    = Bathroom
    | Bedroom
    | LivingRoom
    | Kitchen
    | AnyRoom


decode : String -> Decoder Room
decode string =
    case fromString string of
        Ok room ->
            Decode.succeed room

        Err msg ->
            Decode.fail msg


fromString : String -> Result String Room
fromString string =
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


className : Room -> String
className room =
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


displayName : Room -> String
displayName room =
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


asKey : Room -> String
asKey room =
    displayName room
        |> String.toLower
        |> String.replace " " ""

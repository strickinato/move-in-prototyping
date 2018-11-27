module Card exposing (Action(..), Card(..), Category(..), Item(..), Quality(..), Room(..), Rules, Space(..), Stats(..), Vibe(..), decode)

import Json.Decode as Decode exposing (..)


type Card
    = ItemCard Item
    | ActionCard Action


type Item
    = Any Stats
    | Single Room Stats
    | Multiple (List ( Room, Stats ))


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


decode : Decoder Card
decode =
    succeed <| ActionCard <| Action "Hi"


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

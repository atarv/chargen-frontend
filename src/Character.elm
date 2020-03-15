module Character exposing
    ( Attributes
    , Character
    , SavingThrows
    , allClasses
    , allRaces
    , characterDecoder
    , getAttribute
    , possibleCharacterClasses
    )

import Dict exposing (Dict, get)
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)
import Maybe exposing (withDefault)
import Set as Set exposing (Set)



-- TYPES


type alias Attributes =
    Dict String Int


type alias SavingThrows =
    Dict String Int


type alias Character =
    { race : String
    , cClass : String
    , level : Int
    , alignment : String
    , attributes : Attributes
    , savingThrows : SavingThrows
    }



-- FUNCTIONS


allRaces : Set String
allRaces =
    Set.fromList [ "Dwarf", "Elf", "Gnome" ]


allClasses : Set String
allClasses =
    Set.fromList [ "Assassin", "Cleric", "Druid", "Fighter" ]


getAttribute : Attributes -> String -> Int
getAttribute attributes name =
    withDefault -1 <| get name attributes


raceAllowedClasses : String -> Set String
raceAllowedClasses race =
    withDefault Set.empty <|
        Dict.get race <|
            Dict.fromList
                [ ( "Dwarf", Set.fromList [ "Assassin", "Cleric", "Fighter" ] )
                , ( "Elf", Set.fromList [ "Assassin", "Cleric", "Fighter" ] )
                , ( "Gnome", Set.fromList [ "Assassin", "Cleric", "Fighter" ] )
                ]


possibleCharacterClasses : Set String -> Set String
possibleCharacterClasses selectedRaces =
    Set.foldl
        (\r cls -> Set.union cls <| raceAllowedClasses r)
        Set.empty
        selectedRaces



-- JSON


characterDecoder : Decoder Character
characterDecoder =
    Decode.succeed Character
        |> required "race" string
        |> required "cClass" string
        |> required "level" int
        |> required "alignment" string
        |> required "attributes" (Decode.dict int)
        |> required "savingThrows" (Decode.dict int)

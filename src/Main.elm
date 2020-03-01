module Chargen exposing (main)

import Browser
import Dict exposing (Dict, get)
import Html exposing (Html, div, fieldset, h2, input, label, legend, pre, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Maybe exposing (withDefault)
import MultiSelect exposing (Item, multiSelect)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure
    | Success ModelContent


type alias ModelContent =
    { form : FormData
    , characters : List Character
    }


type alias FormData =
    { minLevel : Int
    , maxLevel : Int
    , count : Int
    , selectedRaces : List String
    , selectedClasses : List String
    }

type alias Attributes = Dict String Int

type alias Character =
    { race : String
    , cClass : String
    , level : Int
    , alignment : String
    , attributes : Attributes
    }



showFormData : FormData -> String
showFormData data =
    "FormData\n{ minLevel = "
        ++ String.fromInt data.minLevel
        ++ "\n, maxLevel = "
        ++ String.fromInt data.maxLevel
        ++ "\n, count = "
        ++ String.fromInt data.count
        ++ "\n, selectedRaces = "
        ++ String.concat (List.intersperse ", " data.selectedRaces)
        ++ "\n, selectedClasses = "
        ++ String.concat (List.intersperse ", " data.selectedClasses)
        ++ "\n}\n"


allRaces : List String
allRaces =
    [ "Dwarf", "Elf", "Gnome" ]


allClasses : List String
allClasses =
    [ "Assassin", "Cleric", "Druid", "Fighter" ]


defaultFormData : FormData
defaultFormData =
    { minLevel = 1, maxLevel = 20, selectedRaces = allRaces, selectedClasses = allClasses, count = 10 }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Success { form = defaultFormData, characters = [] }, getRandomCharacters defaultFormData )



-- UPDATE


type Msg
    = Reset
    | RaceSelectionChanged (List String)
    | ClassSelectionChanged (List String)
    | ChangeMaxLevel String
    | ChangeMinLevel String
    | ChangeCount String
    | GenerateCharacters FormData
    | GotCharacters (Result Http.Error (List Character))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Success content ->
            case msg of
                Reset ->
                    init ()

                GenerateCharacters form ->
                    ( model, getRandomCharacters form )

                GotCharacters httpResult ->
                    case httpResult of
                        Ok newChars ->
                            let
                                changeChars =
                                    \mod -> { mod | characters = newChars }
                            in
                            ( Success (changeChars content), Cmd.none )

                        Err _ ->
                            ( Failure, Cmd.none )

                RaceSelectionChanged newRaces ->
                    let
                        changeRaces =
                            \form -> { form | selectedRaces = newRaces }
                    in
                    ( Success { content | form = changeRaces content.form }, Cmd.none )

                ClassSelectionChanged newClasses ->
                    let
                        changeClasses =
                            \form -> { form | selectedClasses = newClasses }
                    in
                    ( Success { content | form = changeClasses content.form }, Cmd.none )

                ChangeMinLevel lvl ->
                    let
                        changeLevel =
                            \form -> { form | minLevel = Maybe.withDefault 1 (String.toInt lvl) }
                    in
                    ( Success { content | form = changeLevel content.form }, Cmd.none )

                ChangeMaxLevel lvl ->
                    let
                        changeLevel =
                            \form -> { form | maxLevel = Maybe.withDefault 1 (String.toInt lvl) }
                    in
                    ( Success { content | form = changeLevel content.form }, Cmd.none )

                ChangeCount c ->
                    let
                        changeCount =
                            \form -> { form | count = Maybe.withDefault 1 (String.toInt c) }
                    in
                    ( Success { content | form = changeCount content.form }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Success { form, characters } ->
            div [ class "content" ]
                [ h2 [ id "app-header" ] [ text "OSRIC Random Character Generator" ]
                , formView form
                , pre [] [ text (showFormData form) ]
                , div [] [ characterListView characters ]
                ]

        _ ->
            div [] [ text "unhandled" ]


formView : FormData -> Html Msg
formView form =
    Html.form [ class "pure-form pure-form-stacked" ]
        [ fieldset []
            [ legend [] [ text "Character generation constraints" ]
            , div [ class "pure-g" ]
                -- Add grid class for every form field
                (List.map (\x -> div [ class "pure-u-1 pure-u-md-1-4" ] [ x ])
                    [ levelNumber "Min level" form.minLevel 1 form.maxLevel ChangeMinLevel
                    , levelNumber "Max level" form.maxLevel form.minLevel 100 ChangeMaxLevel
                    , label []
                        [ text "Races"
                        , multiSelect
                            { items = optionsFromList allRaces
                            , onChange = RaceSelectionChanged
                            }
                            [ class "pure-u-4-5", Html.Attributes.required True ]
                            form.selectedRaces
                        ]
                    , label []
                        [ text "Classes"
                        , multiSelect
                            { items = optionsFromList allClasses
                            , onChange = ClassSelectionChanged
                            }
                            [ class "pure-u-4-5", Html.Attributes.required True ]
                            form.selectedClasses
                        ]
                    , levelNumber "Character count" form.count 1 100 ChangeCount
                    ]
                )
            , Html.button
                [ class "pure-button"
                , Html.Events.onClick (GenerateCharacters form)
                , type_ "button"
                ]
                [ text "Generate" ]
            ]
        ]


optionsFromList : List String -> List Item
optionsFromList races =
    List.map (\s -> { value = s, text = s, enabled = True }) races


levelNumber : String -> Int -> Int -> Int -> (String -> Msg) -> Html Msg
levelNumber txt val min max action =
    label []
        [ text txt
        , input
            [ class "pure-u-5-24 level-number"
            , type_ "number"
            , value (String.fromInt val)
            , onInput action
            , Html.Attributes.min (String.fromInt min)
            , Html.Attributes.max (String.fromInt max)
            , size 2
            ]
            []
        ]


characterListView : List Character -> Html Msg
characterListView characters =
    table [ class "pure-table pure-table-horizontal pure-table-striped" ]
        [ thead []
            [ tr []
                (List.map
                    (\header -> th [] [ text header ])
                    [ "Race"
                    , "Class"
                    , "Level"
                    , "Alignment"
                    , "STR"
                    , "DEX"
                    , "CON"
                    , "INT"
                    , "WIS"
                    , "CHA"
                    ]
                )
            ]
        , tbody []
            (List.map characterView characters)
        ]


characterView : Character -> Html Msg
characterView chr =
    tr []
        ([ td [] [ text chr.race ]
         , td [] [ text chr.cClass ]
         , td [] [ text <| String.fromInt chr.level ]
         , td [] [ text chr.alignment ]
         ]
            ++ characterAttributes chr.attributes
        )


characterAttributes : Attributes -> List (Html Msg)
characterAttributes attributes =
    List.map
        (\att ->
            td []
                [ text <| String.fromInt <| getAttribute attributes att ]
        )
        [ "str", "dex", "con", "int", "wis", "cha" ]


getAttribute : Attributes -> String -> Int
getAttribute attributes name =
    withDefault -1 <| get name attributes



-- HTTP


getRandomCharacters : FormData -> Cmd Msg
getRandomCharacters form =
    Http.post
        { url = "http://localhost:8080/character"
        , body = Http.jsonBody <| formDataEncode form
        , expect = Http.expectJson GotCharacters (Decode.list characterDecoder)
        }



-- JSON Decoders


characterDecoder : Decoder Character
characterDecoder =
    Decode.succeed Character
        |> required "race" string
        |> required "cClass" string
        |> required "level" int
        |> required "alignment" string
        |> required "attributes" (Decode.dict int)


formDataEncode : FormData -> Encode.Value
formDataEncode form =
    Encode.object
        [ ( "minLevel", Encode.int form.minLevel )
        , ( "maxLevel", Encode.int form.maxLevel )
        , ( "count", Encode.int form.count )
        , ( "selectedRaces", Encode.list Encode.string form.selectedRaces )
        , ( "selectedClasses", Encode.list Encode.string form.selectedClasses )
        ]

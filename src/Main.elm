module Chargen exposing (main)

import Browser
import Html exposing (Html, div, fieldset, h2, input, label, legend, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
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
    }


showFormData : FormData -> String
showFormData data =
    "FormData { minLevel = "
        ++ String.fromInt data.minLevel
        ++ ", maxLevel = "
        ++ String.fromInt data.maxLevel
        ++ ", count = "
        ++ String.fromInt data.count
        ++ ", selectedRaces = "
        ++ String.concat (List.intersperse ", " data.selectedRaces)
        ++ " }\n"


defaultSelectedRaces : List String
defaultSelectedRaces =
    [ "Dwarf", "Elf", "Gnome" ]


defaultFormData : FormData
defaultFormData =
    { minLevel = 1, maxLevel = 20, selectedRaces = defaultSelectedRaces, count = 10 }


type alias Character =
    { race : String
    , cClass : String
    , level : Int
    , alignment : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Success { form = defaultFormData, characters = [] }, getRandomCharacters defaultFormData )



-- UPDATE


type Msg
    = Reset
    | RaceSelectionChanged (List String)
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
                , text (showFormData form)
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
                    [ levelNumber "Min level" (String.fromInt form.minLevel) ChangeMinLevel
                    , levelNumber "Max level" (String.fromInt form.maxLevel) ChangeMaxLevel
                    , label []
                        [ text "Races"
                        , multiSelect
                            { items = raceOptions defaultSelectedRaces
                            , onChange = RaceSelectionChanged
                            }
                            [ class "pure-u-4-5", Html.Attributes.required True ]
                            form.selectedRaces
                        ]
                    , levelNumber "Character count" (String.fromInt form.count) ChangeCount
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


raceOptions : List String -> List Item
raceOptions races =
    List.map (\s -> { value = s, text = s, enabled = True }) races


levelNumber : String -> String -> (String -> Msg) -> Html Msg
levelNumber txt val action =
    label []
        [ text txt
        , input
            [ class "pure-u-5-24 level-number"
            , type_ "number"
            , value val
            , onInput action
            , Html.Attributes.min "1"
            , Html.Attributes.max "100"
            , size 2
            ]
            []
        ]


characterListView : List Character -> Html Msg
characterListView characters =
    table [ class "pure-table pure-table-horizontal pure-table-striped" ]
        [ thead []
            [ tr []
                [ th [] [ text "Class" ]
                , th [] [ text "Race" ]
                , th [] [ text "Level" ]
                , th [] [ text "Alignment" ]
                ]
            ]
        , tbody []
            (List.map characterView characters)
        ]


characterView : Character -> Html Msg
characterView chr =
    tr []
        [ td [] [ text chr.cClass ]
        , td [] [ text chr.race ]
        , td [] [ text <| String.fromInt chr.level ]
        , td [] [ text chr.alignment ]
        ]



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


formDataEncode : FormData -> Encode.Value
formDataEncode form =
    Encode.object
        [ ( "minLevel", Encode.int form.minLevel )
        , ( "maxLevel", Encode.int form.maxLevel )
        , ( "count", Encode.int form.count )
        ]

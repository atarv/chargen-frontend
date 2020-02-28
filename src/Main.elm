module Chargen exposing (main)

import Browser
import Html exposing (Html, div, fieldset, h2, input, label, legend, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required)
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
    | Loading ModelContent
    | Success ModelContent


type alias ModelContent =
    { form : FormData
    , characters : List Character
    }


type alias FormData =
    { minLevel : Int
    , maxLevel : Int
    , selectedRaces : List String
    }


showFormData : FormData -> String
showFormData data =
    "FormData { minLevel = "
        ++ String.fromInt data.minLevel
        ++ ", maxLevel = "
        ++ String.fromInt data.maxLevel
        ++ ", selectedRaces = "
        ++ String.concat (List.intersperse ", " data.selectedRaces)
        ++ " }\n"


defaultSelectedRaces : List String
defaultSelectedRaces =
    [ "Dwarf", "Elf", "Gnome" ]


defaultFormData : FormData
defaultFormData =
    { minLevel = 1, maxLevel = 20, selectedRaces = defaultSelectedRaces }


type alias Character =
    -- TODO: add Attributes and SavingThrows, maybe decoders needed
    { race : String
    , cClass : String
    , level : Int
    , alignment : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Success { form = defaultFormData, characters = [] }, Cmd.none )



-- UPDATE


type Msg
    = Reset
    | RaceSelectionChanged (List String)
    | ChangeMaxLevel String
    | ChangeMinLevel String
    | GotCharacters (Result Http.Error (List Character))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init ()

        GotCharacters httpResult ->
            case httpResult of
                -- FIXME: Toteuta oikea käsittely
                Ok newChars ->
                    let
                        changeChars =
                            \chars mod -> { mod | characters = chars }
                    in
                    case model of
                        Success content ->
                            ( Success (changeChars newChars content), Cmd.none )

                        Loading content ->
                            ( Loading (changeChars newChars content), Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        RaceSelectionChanged newRaces ->
            let
                changeRaces =
                    \form -> { form | selectedRaces = newRaces }
            in
            case model of
                Success content ->
                    ( Success { content | form = changeRaces content.form }, Cmd.none )

                Loading content ->
                    ( Loading { content | form = changeRaces content.form }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeMinLevel lvl ->
            let
                changeLevel =
                    \form -> { form | minLevel = Maybe.withDefault 1 (String.toInt lvl) }
            in
            case model of
                Success content ->
                    ( Success { content | form = changeLevel content.form }, Cmd.none )

                Loading content ->
                    ( Loading { content | form = changeLevel content.form }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ChangeMaxLevel lvl ->
            let
                changeLevel =
                    \form -> { form | maxLevel = Maybe.withDefault 1 (String.toInt lvl) }
            in
            case model of
                Success content ->
                    ( Success { content | form = changeLevel content.form }, Cmd.none )

                Loading content ->
                    ( Loading { content | form = changeLevel content.form }, Cmd.none )

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
                , Html.form [ class "pure-form pure-form-stacked" ]
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
                                ]
                            )
                        ]
                    , text (showFormData form)
                    ]
                ]

        _ ->
            div [] [ text (Debug.todo "unhandled") ]


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



-- HTTP


getRandomCharacters : Int -> Cmd Msg
getRandomCharacters count =
    Http.get
        { url = "http://localhost:8080/characters" ++ String.fromInt count
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

module Chargen exposing (main)

import Browser
import Character exposing (..)
import Html exposing (Html, div, fieldset, h2, input, label, pre, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (int, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Maybe exposing (withDefault)
import MultiSelect exposing (Item, multiSelect)
import Set as Set exposing (Set)
import Tuple exposing (second)



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
    = MainModel ModelContent


type alias ModelContent =
    { form : FormData
    , characters : List Character
    , errorMessage : Maybe String
    }


type alias FormData =
    { minLevel : Int
    , maxLevel : Int
    , count : Int
    , selectedRaces : Set String
    , selectedClasses : Set String
    , attributeGen : String
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
        ++ String.concat
            (List.intersperse ", " <| Set.toList data.selectedRaces)
        ++ "\n, selectedClasses = "
        ++ String.concat
            (List.intersperse ", " <| Set.toList data.selectedClasses)
        ++ "\n, attributeGen = "
        ++ data.attributeGen
        ++ "\n}\n"


defaultFormData : FormData
defaultFormData =
    { minLevel = 1
    , maxLevel = 20
    , selectedRaces = allRaces
    , selectedClasses = allowedClassesForRaces allRaces
    , count = 10
    , attributeGen = "Method4D6BestOf3"
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( MainModel { form = defaultFormData, characters = [], errorMessage = Nothing }
    , getRandomCharacters defaultFormData
    )



-- UPDATE


type Msg
    = Reset
    | RaceSelectionChanged (List String)
    | ClassSelectionChanged (List String)
    | ChangeAttributeGen String
    | ChangeMaxLevel String
    | ChangeMinLevel String
    | ChangeCount String
    | GenerateCharacters
    | GenerateMoreCharacters
    | GotCharacters (Result Http.Error (List Character))
    | GotMoreCharacters (Result Http.Error (List Character))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        MainModel ({ characters, form } as content) ->
            case msg of
                Reset ->
                    init ()

                GenerateCharacters ->
                    ( model, getRandomCharacters form )

                GenerateMoreCharacters ->
                    ( model, getMoreRandomCharacters form )

                GotCharacters httpResult ->
                    case httpResult of
                        Ok newChars ->
                            ( MainModel { content | characters = newChars, errorMessage = Nothing }, Cmd.none )

                        Err e ->
                            ( MainModel { content | errorMessage = Just <| errorMessageFrom e }, Cmd.none )

                GotMoreCharacters httpResult ->
                    case httpResult of
                        Ok newChars ->
                            ( MainModel { content | characters = characters ++ newChars, errorMessage = Nothing }, Cmd.none )

                        Err e ->
                            ( MainModel { content | errorMessage = Just <| errorMessageFrom e }, Cmd.none )

                RaceSelectionChanged newRaces ->
                    ( MainModel
                        { content
                            | form = { form | selectedRaces = Set.fromList newRaces }
                        }
                    , Cmd.none
                    )

                ClassSelectionChanged newClasses ->
                    ( MainModel { content | form = { form | selectedClasses = Set.fromList newClasses } }, Cmd.none )

                ChangeAttributeGen method ->
                    ( MainModel { content | form = { form | attributeGen = method } }, Cmd.none )

                ChangeMinLevel lvl ->
                    ( MainModel { content | form = { form | minLevel = Maybe.withDefault 1 (String.toInt lvl) } }, Cmd.none )

                ChangeMaxLevel lvl ->
                    ( MainModel { content | form = { form | maxLevel = Maybe.withDefault 1 (String.toInt lvl) } }, Cmd.none )

                ChangeCount c ->
                    ( MainModel { content | form = { form | count = Maybe.withDefault 1 (String.toInt c) } }, Cmd.none )


errorMessageFrom : Http.Error -> String
errorMessageFrom err =
    case err of
        Http.BadStatus code ->
            String.fromInt code

        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.NetworkError ->
            "Network error"

        Http.Timeout ->
            "Request timed out"

        _ ->
            "Undefined error"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        MainModel { form, characters, errorMessage } ->
            div [ class "content" ]
                [ h2 [ id "app-header" ] [ text "OSRIC Random Character Generator" ]
                , formView form
                , errorMessageView (isJust errorMessage) ("Well, that was a bad roll: " ++ Maybe.withDefault "" errorMessage)
                , pre [] [ text (showFormData form) ] -- DEBUG:
                , div [] [ characterListView characters ]
                , div [ class "centered" ]
                    [ Html.button
                        [ class "pure-button"
                        , onClick <| GenerateMoreCharacters
                        , (disabled << not << List.isEmpty << validateForm) form
                        ]
                        [ text "Generate more"
                        ]
                    ]
                ]


isJust : Maybe a -> Bool
isJust a =
    case a of
        Just _ ->
            True

        Nothing ->
            False


errorMessageView : Bool -> String -> Html msg
errorMessageView isVisible message =
    div [ class "error-message", (not >> hidden) isVisible ] [ text message ]


formView : FormData -> Html Msg
formView form =
    let
        isInError =
            (not << List.isEmpty << validateForm) form

        errorMessage =
            withDefault "" <| List.head (validateForm form)
    in
    Html.form [ class "pure-form pure-form-stacked" ]
        [ fieldset []
            [ div [ class "pure-g" ]
                -- Add grid class for every form field
                (List.map (\x -> div [ class "pure-u-1 pure-u-md-1-5" ] [ x ])
                    [ levelNumber "Min level" form.minLevel 1 form.maxLevel ChangeMinLevel
                    , levelNumber "Max level" form.maxLevel form.minLevel 100 ChangeMaxLevel
                    , label []
                        [ text "Races"
                        , multiSelect
                            { items = optionsFromSet allRaces allRaces
                            , onChange = RaceSelectionChanged
                            }
                            [ class "pure-u-4-5", Html.Attributes.required True ]
                          <|
                            Set.toList form.selectedRaces
                        ]
                    , label []
                        [ text "Classes"
                        , multiSelect
                            { items =
                                optionsFromSet allClasses <|
                                    allowedClassesForRaces form.selectedRaces
                            , onChange = ClassSelectionChanged
                            }
                            [ class "pure-u-4-5", Html.Attributes.required True ]
                          <|
                            Set.toList form.selectedClasses
                        ]
                    , levelNumber "Character count" form.count 1 100 ChangeCount
                    , attributeGenChooser form.attributeGen
                    ]
                )
            , div [ class "centered" ]
                [ errorMessageView isInError errorMessage
                , Html.button
                    [ class "pure-button"
                    , onClick GenerateCharacters
                    , type_ "button"
                    , disabled isInError
                    ]
                    [ text "Generate" ]
                ]
            ]
        ]


optionsFromSet : Set String -> Set String -> List Item
optionsFromSet allOptions allowedValues =
    List.map
        (\s -> { value = s, text = s, enabled = Set.member s allowedValues })
    <|
        Set.toList allOptions


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
                    , "Magic Items"
                    , "Breath"
                    , "Death"
                    , "Petrification"
                    , "Spells"
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
            ++ attributesView chr.attributes
            ++ savingThrowsView chr.savingThrows
        )


attributesView : Attributes -> List (Html Msg)
attributesView attributes =
    List.map
        (\att ->
            td []
                [ text <| String.fromInt <| getAttribute attributes att ]
        )
        [ "str", "dex", "con", "int", "wis", "cha" ]


savingThrowsView : SavingThrows -> List (Html Msg)
savingThrowsView sthrs =
    List.map
        (\st ->
            td []
                [ text <| String.fromInt <| getAttribute sthrs st ]
        )
        [ "magicItems", "breath", "death", "petrify", "spells" ]


attributeGenChooser : String -> Html Msg
attributeGenChooser current =
    label []
        [ text "Attribute generation method"
        , div []
            [ label [ for "3d6", class "pure-radio" ]
                [ input
                    [ type_ "radio"
                    , id "3d6"
                    , name "attributeGen"
                    , value "Method3D6"
                    , checked (current == "Method3D6")
                    , onInput ChangeAttributeGen
                    ]
                    []
                , text "3D6"
                ]
            , label [ for "4d6bof3", class "pure-radio" ]
                [ input
                    [ type_ "radio"
                    , id "4d6bof3"
                    , name "attributeGen"
                    , value "Method4D6BestOf3"
                    , checked (current == "Method4D6BestOf3")
                    , onInput ChangeAttributeGen
                    ]
                    []
                , text "4D6 best of 3"
                ]
            ]
        ]


{-| Returns `Nothing`, if form is valid, otherwise an error message is returned.
-}
validateForm : FormData -> List String
validateForm data =
    List.map second <|
        List.filter (\( b, _ ) -> b)
            [ ( Set.isEmpty data.selectedRaces
              , "Select at least one race"
              )
            , ( Set.isEmpty data.selectedClasses
              , "Select at least one class"
              )
            , ( not <| data.minLevel <= data.maxLevel
              , "Min level must be lesser than or equal to max level"
              )
            , ( List.any (\i -> i < 1) [ data.minLevel, data.maxLevel, data.count ]
              , "All form fields must be positive"
              )
            ]



-- HTTP


getRandomCharacters : FormData -> Cmd Msg
getRandomCharacters form =
    Http.post
        { url = "http://localhost:8080/character"
        , body = Http.jsonBody <| formDataEncode form
        , expect = Http.expectJson GotCharacters (Decode.list characterDecoder)
        }


getMoreRandomCharacters : FormData -> Cmd Msg
getMoreRandomCharacters form =
    Http.post
        { url = "http://localhost:8080/character"
        , body = Http.jsonBody <| formDataEncode form
        , expect = Http.expectJson GotMoreCharacters (Decode.list characterDecoder)
        }



-- JSON Decoders


formDataEncode : FormData -> Encode.Value
formDataEncode form =
    Encode.object
        [ ( "minLevel", Encode.int form.minLevel )
        , ( "maxLevel", Encode.int form.maxLevel )
        , ( "count", Encode.int form.count )
        , ( "selectedRaces", Encode.set Encode.string form.selectedRaces )
        , ( "selectedClasses", Encode.set Encode.string form.selectedClasses )
        , ( "attributeGen", Encode.string form.attributeGen )
        ]

module Chargen exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline exposing (required, optional)


-- MAIN
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

type Model
  = Form FormData
  | Failure
  | Loading FormData
  | Success FormData (List Character)

type alias FormData = { minLevel : Int, maxLevel : Int }

defaultFormData = { minLevel = 1, maxLevel = 20 }

-- TODO: add Attributes and SavingThrows
type alias Character =  { race : String
                        , cClass : String
                        , level : Int
                        , alignment : String }


init : () -> (Model, Cmd Msg)
init _ =
  (Form defaultFormData, Cmd.none)

-- UPDATE

type Msg
  = GotoForm
  | GotCharacters (Result Http.Error (List Character))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotoForm -> (Form defaultFormData, Cmd.none)
    GotCharacters httpResult ->
        case httpResult of
            Ok chars -> (Form defaultFormData, Cmd.none) -- FIXME: Toteuta oikea käsittely
            Err _ -> (Failure, Cmd.none)

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
    case model of 
        Form data -> div [class "content"] 
            [ Html.form [ class "pure-form pure-form-stacked" ] 
                [ fieldset []
                    [ legend [] [ text "Character generation constraints"]
                    , div [ class "pure-g" ]
                        (List.map (\x -> div [class "pure-u-1 pure-u-md-1-2" ] [x]) [ label []  
                            [ text "Min level"
                            , input [class "pure-u-2-5", type_ "number", value (String.fromInt (data.minLevel)), Html.Attributes.min "1", Html.Attributes.max "100", size 2] []
                                ]
                            , label []  [ text "Max level"
                                , input [ class "pure-u-2-5", type_ "number", value (String.fromInt (data.maxLevel)), size 2] []
                                ]
                            , label [] [ text "Races"
                                , select [ class "pure-u-4-5", multiple True, Html.Attributes.required True] 
                                (List.map raceOption ["Dwarf", "Elf", "Gnome"])
                                ]
                            ]
                        )
                    ]
                ]
            ]
        _ -> div [] [text "unhandled"]

raceOption race = option [ value race, selected True ] [ text race ]

-- HTTP

-- getRandomCatGif : Cmd Msg
-- getRandomCatGif =
--   Http.get
--     { url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=cat"
--     , expect = Http.expectJson GotGif gifDecoder
--     }

getRandomCharacters : Int -> Cmd Msg
getRandomCharacters count =
    Http.get
        { url = "http://localhost:8080/characters" ++ (String.fromInt count)
        , expect = Http.expectJson GotCharacters (Decode.list characterDecoder)
        }


-- JSON Decoders
characterDecoder : Decoder Character
characterDecoder = Decode.succeed Character
    |> required "race" string
    |> required "cClass" string
    |> required "level" int
    |> required "alignment" string
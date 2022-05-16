module Views.Form exposing
    ( Form
    , NumberInput
    , TextInput
    , form
    , numberInput
    , textInput
    )

import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Json


type alias Input value msg =
    { id : String
    , label : String
    , value : value
    , error : String
    , onInput : value -> msg
    , onBlur : msg
    , type_ : String
    , toString : value -> String
    , fromString : String -> value
    }


input : Input value msg -> Html msg
input config =
    H.div []
        [ H.label [ A.for config.id ] [ H.text config.label ]
        , H.input
            [ A.type_ config.type_
            , A.id config.id
            , A.name config.id
            , A.value (config.toString config.value)
            , E.onInput (config.onInput << config.fromString)
            , E.onBlur config.onBlur
            ]
            []
        , if String.isEmpty config.error then
            H.text ""

          else
            H.p [] [ H.text config.error ]
        ]


type alias TextInput msg =
    { id : String
    , label : String
    , value : String
    , error : String
    , onInput : String -> msg
    , onBlur : msg
    }


textInput : TextInput msg -> Html msg
textInput config =
    input
        { id = config.id
        , label = config.label
        , value = config.value
        , error = config.error
        , onInput = config.onInput
        , onBlur = config.onBlur
        , type_ = "text"
        , toString = identity
        , fromString = identity
        }


type alias NumberInput msg =
    { id : String
    , label : String
    , value : Maybe Int
    , error : String
    , onInput : Maybe Int -> msg
    , onBlur : msg
    }


numberInput : NumberInput msg -> Html msg
numberInput config =
    input
        { id = config.id
        , label = config.label
        , value = config.value
        , error = config.error
        , onInput = config.onInput
        , onBlur = config.onBlur
        , type_ = "number"
        , toString = Maybe.withDefault "" << Maybe.map String.fromInt
        , fromString = String.toInt
        }


type alias Form msg =
    { id : String
    , enableSubmit : Bool
    , onSubmit : msg
    , onReset : msg
    }


{-| preventDefault() the form submission so that it doesn't refresh the page

    This function is actually just taken from the docs here:
    https://package.elm-lang.org/packages/elm/html/latest/Html-Events#preventDefaultOn

-}
onSubmit : msg -> H.Attribute msg
onSubmit msg =
    E.preventDefaultOn "submit" (Json.map (\_ -> ( msg, True )) (Json.succeed msg))


form : Form msg -> List (Html msg) -> Html msg
form config contents =
    let
        buttons =
            H.div []
                [ H.button [ A.type_ "reset", E.onClick config.onReset ] [ H.text "Reset" ]
                , H.button [ A.type_ "submit", A.disabled (not config.enableSubmit) ] [ H.text "Submit" ]
                ]
    in
    H.form
        [ A.id config.id, onSubmit config.onSubmit ]
        (contents ++ [ buttons ])

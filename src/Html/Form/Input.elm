module Html.Form.Input
    ( Element, FormElement, formGroup
    , textInput, passwordInput, intInput, floatInput
    , dateInput, TimeOfDay, timeInput
    , checkBox
    , textArea
    , SelectElement, selectBox
    , InputElement, basicInput
    ) where
{-| This module will help generating good looking forms using the twitter bootstrap framework

# Default input element structure
@docs Element, FormElement

# Popular form groups
@docs textInput, passwordInput, intInput, floatInput, dateInput, TimeOfDay, timeInput, checkBox, SelectElement, selectBox, textArea

# Custom input elements
@docs InputElement, basicInput

# Custom form groups
@docs formGroup
-}

import Date
import String
import Time
import Html as H
import Html.Attributes as A
import Html.Events as E
import Html exposing (text)

{-| All inputs will be defined by this basic structure -}
type alias Element v e a =
    { a | id: String
        , label: String
        , helpBlock: Maybe String
        , value: v
        , hasError: Bool
        , onValue: v -> Signal.Message
        , onError: e -> Signal.Message
    }

{-| An element with de and encoder -}
type alias FormElement v e a =
    Element v e
    {  a | decoder: String -> Result e v
         , encoder: v -> String
    }

{-| Build your own input element -}
formGroup : (FormElement v e a -> H.Html) -> FormElement v e a -> H.Html
formGroup makeInput el =
    H.div [ A.class ("form-group" ++ if el.hasError then " has-error" else "") ]
    [ H.label [ A.for el.id ] [ text el.label ]
    , makeInput el
    , case el.helpBlock of
        Just val -> H.span [ A.class "help-block" ] [ text val ]
        Nothing -> H.span [] []
    ]

{-| Spec for an input with a type -}
type alias InputElement v e a =
    FormElement v e
    { type': String
    }

{-| A simple text input -}
basicInput : InputElement v e a -> H.Html
basicInput =
    formGroup <| \el ->
    H.input
    [ A.type' el.type'
    , A.id el.id
    , A.class "form-control"
    , A.placeholder el.label
    , A.value (el.encoder el.value)
    , E.on "change" E.targetValue <| \str ->
        case el.decoder str of
            Err err -> el.onError err
            Ok v -> el.onValue v
    ] []

{-| A textarea -}
textArea : Element String e {} -> H.Html
textArea el =
    let el1 = { el | decoder = Ok }
        el2 = { el1 | encoder = identity}
        handle e =
            H.textarea
           [ A.id e.id
           , A.class "form-control"
           , A.placeholder e.label
           , E.on "change" E.targetValue e.onValue
           ] [text (e.encoder e.value)]
    in formGroup handle el2

{-| A simple text input -}
textInput : Element String e {} -> H.Html
textInput el =
    basicInput <|
    let el1 = { el | decoder = Ok }
        el2 = { el1 | encoder = identity}
        el3 = { el2 | type' = "text" }
    in el3

{-| A simple password input -}
passwordInput : Element String e {} -> H.Html
passwordInput el =
    basicInput <|
    let el1 = { el | decoder = Ok }
        el2 = { el1 | encoder = identity}
        el3 = { el2 | type' = "password" }
    in el3

{-| A simple int input -}
intInput : Element Int String {} -> H.Html
intInput el =
    basicInput <|
    let el1 = { el | decoder = String.toInt }
        el2 = { el1 | encoder = toString }
        el3 = { el2 | type' = "number" }
    in el3

{-| A simple float input -}
floatInput : Element Float String {} -> H.Html
floatInput el =
    basicInput <|
    let el1 = { el | decoder = String.toFloat }
        el2 = { el1 | encoder = toString }
        el3 = { el2 | type' = "number" }
    in el3

{-| A simple date input -}
dateInput : Element Date.Date String {} -> H.Html
dateInput el =
    basicInput <|
    let encode d =
            (String.padLeft 4 '0' <| toString <| Date.year d) ++ "-"
            ++ (String.padLeft 2 '0' <| toString <| dateMonthToInt d) ++ "-"
            ++ (String.padLeft 2 '0' <| toString <| Date.day d)
        el1 = { el | decoder = Date.fromString }
        el2 = { el1 | encoder = encode }
        el3 = { el2 | type' = "date" }
    in el3

dateMonthToInt : Date.Date -> Int
dateMonthToInt d =
    case Date.month d of
        Date.Jan -> 1
        Date.Feb -> 2
        Date.Mar -> 3
        Date.Apr -> 4
        Date.May -> 5
        Date.Jun -> 6
        Date.Jul -> 7
        Date.Aug -> 8
        Date.Sep -> 9
        Date.Oct -> 10
        Date.Nov -> 11
        Date.Dec -> 12

{-| Time of day -}
type alias TimeOfDay =
    { hour: Int
    , minute: Int
    }

{-| A simple time input -}
timeInput : Element TimeOfDay String {} -> H.Html
timeInput el =
    basicInput <|
    let encode t =
            (String.padLeft 2 '0' <| toString <| t.hour) ++ ":"
            ++ (String.padLeft 2 '0' <| toString <| t.minute)
        decode str =
            case String.split ":" str of
                [hourStr, minStr] ->
                    String.toInt hourStr `Result.andThen` \hour ->
                    String.toInt minStr `Result.andThen` \min ->
                    Ok { hour = hour, minute = min }
                _ -> Err <| "Invalid date: " ++ str
        el1 = { el | decoder = decode }
        el2 = { el1 | encoder = encode }
        el3 = { el2 | type' = "time" }
    in el3

{-| A simple checkbox input -}
checkBox : Element Bool e {} -> H.Html
checkBox el =
    H.div [ A.class ("checkbox" ++ if el.hasError then " has-error" else "") ]
    [ H.label []
        [ H.input
            [ A.type' "checkbox"
            , A.id el.id
            , A.checked el.value
            , E.on "change" E.targetChecked el.onValue
            ]
            []
        , text (" " ++ el.label)
        ]
    , case el.helpBlock of
        Just val -> H.span [ A.class "help-block" ] [ text val ]
        Nothing -> H.span [] []
    ]

{-| Spec for selectBox -}
type alias SelectElement v e a =
    FormElement v e
    { choices: List v
    , displayChoice: v -> String
    }

{-| A simple dropdown -}
selectBox : SelectElement v e {} -> H.Html
selectBox =
    formGroup <| \el ->
    let opts =
            flip List.map el.choices <| \ch ->
            let valAttr =
                    A.value (el.encoder ch)
                rest =
                    if ch == el.value
                    then [A.selected True]
                    else []
            in H.option (valAttr :: rest) [ text <| el.displayChoice ch ]
    in H.select
        [ A.id el.id
        , A.class "form-control"
        ] opts
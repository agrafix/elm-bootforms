module Html.Form.Input
    ( FormValue
    , Element, FormElement, formGroup
    , textInput, passwordInput, intInput, floatInput
    , dateInput, TimeOfDay, timeInput
    , checkBox
    , textArea
    , SelectElement, selectBox
    , InputElement, basicInput
    , stringFormVal, mayStringFormVal
    ) where
{-| This module will help generating good looking forms using the twitter bootstrap framework

# Default input element structure
@docs FormValue, Element, FormElement

# Popular form groups
@docs textInput, passwordInput, intInput, floatInput, dateInput, TimeOfDay, timeInput, checkBox, SelectElement, selectBox, textArea

# Helper functions
@docs stringFormVal, mayStringFormVal

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

{-| The input string of the user and the parsed value -}
type alias FormValue e v =
    { userInput : String
    , value : Result e v
    }

{-| Generate a FormValue for textual input boxes w/o validation -}
stringFormVal : String -> FormValue e String
stringFormVal str =
    { userInput = str
    , value = Ok str
    }

{-| Generate a FormValue for textual input boxes w/o validation -}
mayStringFormVal : Maybe String -> FormValue e String
mayStringFormVal = stringFormVal << Maybe.withDefault ""

{-| All inputs will be defined by this basic structure -}
type alias Element v e =
    { id: String
    , label: String
    , helpBlock: Maybe String
    , value: FormValue e v
    , onValue: FormValue e v -> Signal.Message
    }

{-| An element with a decoder -}
type alias FormElement v e a =
    { element: Element v e
    , props: a
    , decoder: String -> Result e v
    }

isError : Result a b -> Bool
isError r =
    case r of
        Err _ -> True
        Ok _ -> False

{-| Build your own input element -}
formGroup : Element v e -> H.Html -> H.Html
formGroup el view =
    H.div [ A.class ("form-group" ++ if isError el.value.value then " has-error" else "") ]
    [ H.label [ A.for el.id ] [ text el.label ]
    , view
    , case el.helpBlock of
        Just val -> H.span [ A.class "help-block" ] [ text val ]
        Nothing -> H.span [] []
    ]

{-| Spec for an input with a type -}
type alias InputElement v e =
    FormElement v e
    { type' : String
    , extraClasses : List String
    }

{-| A simple input -}
basicInput : InputElement v e -> H.Html
basicInput iel =
    formGroup iel.element <|
        let el = iel.element
        in H.input
            [ A.type' iel.props.type'
            , A.id el.id
            , A.class ("form-control " ++ String.join " " iel.props.extraClasses)
            , A.placeholder el.label
            , A.value el.value.userInput
            , E.on "input" E.targetValue <| \str ->
                el.onValue
                { userInput = str
                , value = iel.decoder str
                }
            ] []

{-| A textarea -}
textArea : Element String e -> H.Html
textArea el =
    let handle =
            H.textarea
           [ A.id el.id
           , A.class "form-control"
           , A.placeholder el.label
           , E.on "input" E.targetValue <| \str ->
                el.onValue { userInput = str, value = Ok str }
           ]
           [ text el.value.userInput
           ]
    in formGroup el handle

{-| A simple text input -}
textInput : Element String e -> H.Html
textInput el =
    basicInput
    { element = el
    , props = { type' = "text", extraClasses = [] }
    , decoder = Ok
    }

{-| A simple password input -}
passwordInput : Element String e -> H.Html
passwordInput el =
    basicInput
    { element = el
    , props = { type' = "password", extraClasses = [] }
    , decoder = Ok
    }

{-| A simple int input -}
intInput : Element Int String -> H.Html
intInput el =
    basicInput
    { element = el
    , props = { type' = "number", extraClasses = ["bootforms-int"] }
    , decoder = String.toInt
    }

{-| A simple float input -}
floatInput : Element Float String -> H.Html
floatInput el =
    basicInput
    { element = el
    , props = { type' = "number", extraClasses = ["bootforms-float"] }
    , decoder = String.toFloat
    }

{-| A simple date input -}
dateInput : Element Date.Date String -> H.Html
dateInput el =
    basicInput
    { element = el
    , props = { type' = "text", extraClasses = ["bootforms-date"] }
    , decoder = Date.fromString
    }

{-| Time of day -}
type alias TimeOfDay =
    { hour: Int
    , minute: Int
    }

{-| A simple time input -}
timeInput : Element TimeOfDay String -> H.Html
timeInput el =
    basicInput <|
    let decode str =
            case String.split ":" str of
                [hourStr, minStr] ->
                    String.toInt hourStr `Result.andThen` \hour ->
                    String.toInt minStr `Result.andThen` \min ->
                    if hour < 0 || hour > 23 || min < 0 || min > 59
                    then Err <| "Invalid time: " ++ str
                    else Ok { hour = hour, minute = min }
                _ -> Err <| "Ill formatted time: " ++ str
    in { element = el
       , props = { type' = "text", extraClasses = ["bootforms-time"] }
       , decoder = decode
       }

{-| A simple checkbox input -}
checkBox : Element Bool e -> H.Html
checkBox el =
    H.div [ A.class ("checkbox" ++ if isError el.value.value then " has-error" else "") ]
    [ H.label []
        [ H.input
            [ A.type' "checkbox"
            , A.id el.id
            , A.checked (Result.withDefault False el.value.value)
            , E.on "change" E.targetChecked <| \cb ->
                el.onValue { userInput = if cb then "True" else "False", value = Ok cb }
            ]
            []
        , text (" " ++ el.label)
        ]
    , case el.helpBlock of
        Just val -> H.span [ A.class "help-block" ] [ text val ]
        Nothing -> H.span [] []
    ]

{-| Spec for selectBox -}
type alias SelectElement v e =
    FormElement v e
    { choices: List v
    , displayChoice: v -> String
    , choiceValue: v -> String
    }

{-| A simple dropdown -}
selectBox : SelectElement v e -> H.Html
selectBox sel =
    formGroup sel.element <|
    let el = sel.element
        opts =
            flip List.map sel.props.choices <| \ch ->
            let valAttr =
                    A.value (sel.props.choiceValue ch)
                rest =
                    if Ok ch == el.value.value
                    then [A.selected True]
                    else []
            in H.option (valAttr :: rest) [ text <| sel.props.displayChoice ch ]
    in H.select
        [ A.id el.id
        , A.class "form-control"
        , E.on "change" E.targetValue <| \str ->
            el.onValue
            { userInput = str
            , value = sel.decoder str
            }
        ] opts
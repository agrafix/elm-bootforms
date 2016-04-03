module Html.Form.Input
    ( FormValue, FormValueSt, FormValueAction, FormValueActionSt(..)
    , Element, FormElement, formGroup
    , textInput, passwordInput, intInput, floatInput
    , dateInput, TimeOfDay, timeInput
    , checkBox
    , textArea
    , SelectElement, selectBox
    , InputElement, basicInput, basicInputRaw, basicInputRawEff
    , stringFormVal, mayStringFormVal, emptyFormVal
    , getFormValue, getFormValueDef, validFormValue
    , noFx, apply, effect, mappedEffect
    ) where
{-| This module will help generating good looking forms using the twitter bootstrap framework.
It also provides automatic conversion of textual input to more useful types

# Form input
@docs FormValue, FormValueSt, FormValueAction, FormValueActionSt, apply, effect, mappedEffect, noFx, getFormValue, getFormValueDef, validFormValue, stringFormVal, mayStringFormVal, emptyFormVal

# Default input element structure
@docs Element, FormElement

# Popular form groups
@docs textInput, passwordInput, intInput, floatInput, dateInput, TimeOfDay, timeInput, checkBox, SelectElement, selectBox, textArea

# Custom input elements
@docs InputElement, basicInput, basicInputRaw, basicInputRawEff

# Custom form groups
@docs formGroup
-}
import Date
import Effects
import Html as H
import Html exposing (text)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Json
import String
import Time

{-| An action to be applied to a form value -}
type alias FormValueAction e v = FormValueActionSt e v ()

{-| An action to be applied to a form value for components with internal state -}
type FormValueActionSt e v st =
    FormValueActionSt
    { action : FormValueSt e v st -> FormValueSt e v st
    , effect : Effects.Effects (FormValueActionSt e v st)
    }

{-| Apply an action to a form value -}
apply : FormValueActionSt e v st -> FormValueSt e v st -> FormValueSt e v st
apply (FormValueActionSt st) = st.action

{-| Return all side effects of an action to a form value -}
effect : FormValueActionSt e v st -> Effects.Effects (FormValueActionSt e v st)
effect (FormValueActionSt st) = st.effect

{-| Return all side effects of an action to a form value and apply a function to wrap them -}
mappedEffect : (FormValueActionSt e v st -> a) -> FormValueActionSt e v st -> Effects.Effects a
mappedEffect f = Effects.map f << effect

{-| An effect free form action -}
noFx : (FormValueSt e v st -> FormValueSt e v st) -> FormValueActionSt e v st
noFx f = FormValueActionSt { action = f, effect = Effects.none }

{-| The input string of the user and the parsed value -}
type alias FormValue e v = FormValueSt e v ()

{-| The input string of the user, the parsed value and internal state of the component -}
type alias FormValueSt e v st =
    { userInput : String
    , value : Result e v
    , focused : Bool
    , internalState : st
    }

{-| An empty form value -}
emptyFormVal : FormValueSt String v ()
emptyFormVal = { userInput = "", value = Err "No input", focused = False, internalState = () }

{-| Generate a FormValueSt for textual input boxes w/o validation -}
stringFormVal : String -> FormValueSt e String ()
stringFormVal str =
    { userInput = str
    , value = Ok str
    , focused = False
    , internalState = ()
    }

{-| Generate a FormValueSt for textual input boxes w/o validation -}
mayStringFormVal : Maybe String -> FormValueSt e String ()
mayStringFormVal = stringFormVal << Maybe.withDefault ""

{-| All inputs will be defined by this basic structure -}
type alias Element v e st =
    { id: String
    , label: String
    , helpBlock: Maybe String
    , value: FormValueSt e v st
    , onValue: FormValueActionSt e v st -> Signal.Message
    }

{-| An element with a decoder -}
type alias FormElement v e st a =
    { element: Element v e st
    , props: a
    , decoder: String -> Result e v
    , autoBlur: Bool
    }

isError : Result a b -> Bool
isError r =
    case r of
        Err _ -> True
        Ok _ -> False

{-| Build your own input element -}
formGroup : Element v e st -> H.Html -> H.Html
formGroup el view =
    H.div [ A.class ("form-group" ++ if isError el.value.value then " has-error" else "") ]
    [ H.label [ A.for el.id ] [ text el.label ]
    , view
    , case el.helpBlock of
        Just val -> H.span [ A.class "help-block" ] [ text val ]
        Nothing -> H.span [] []
    ]

{-| Spec for an input with a type -}
type alias InputElement v e st =
    FormElement v e st
    { type' : String
    , extraClasses : List String
    }

focusHandlers : Bool -> (FormValueActionSt e v st -> Signal.Message) -> List H.Attribute
focusHandlers autoBlur onValue =
    [ E.on "focus" Json.value <| \_ ->
        onValue <| noFx <| \val -> { val | focused = True }
    , E.on "blur" Json.value <| \_ ->
        onValue <| noFx <| \val -> { val | focused = not autoBlur }
    ]

{-| A simple input not grouped yet emitting an effect -}
basicInputRawEff :
    (FormValueSt e v st -> Effects.Effects (FormValueActionSt e v st))
    -> InputElement v e st
    -> H.Html
basicInputRawEff effs iel =
    let el = iel.element
        val = el.value
    in flip H.input [] <|
           [ A.type' iel.props.type'
           , A.id el.id
           , A.class ("form-control " ++ String.join " " iel.props.extraClasses)
           , A.placeholder el.label
           , A.value val.userInput
           , E.on "input" E.targetValue <| \str ->
               el.onValue <|
               let action curVal =
                       { curVal
                           | userInput = str
                           , value = iel.decoder str
                       }
               in FormValueActionSt
                  { action = action
                  , effect = effs <| action val
                  }
           ] ++ (focusHandlers iel.autoBlur el.onValue)

{-| A simple input not grouped yet -}
basicInputRaw : InputElement v e st -> H.Html
basicInputRaw = basicInputRawEff (\_ -> Effects.none)

{-| A simple input -}
basicInput : InputElement v e st -> H.Html
basicInput iel =
    formGroup iel.element (basicInputRaw iel)

{-| A textarea -}
textArea : Element String e () -> H.Html
textArea el =
    let val = el.value
        handle =
            flip H.textarea [ text val.userInput ] <|
            [ A.id el.id
            , A.class "form-control"
            , A.placeholder el.label
            , E.on "input" E.targetValue <| \str ->
                el.onValue <| noFx <| \curVal ->
                { curVal
                    | userInput = str
                    , value = Ok str
                }
            ] ++ (focusHandlers True el.onValue)
    in formGroup el handle

{-| A simple text input -}
textInput : Element String e () -> H.Html
textInput el =
    basicInput
    { element = el
    , props = { type' = "text", extraClasses = [] }
    , decoder = Ok
    , autoBlur = True
    }

{-| A simple password input -}
passwordInput : Element String e () -> H.Html
passwordInput el =
    basicInput
    { element = el
    , props = { type' = "password", extraClasses = [] }
    , decoder = Ok
    , autoBlur = True
    }

{-| A simple int input -}
intInput : Element Int String () -> H.Html
intInput el =
    basicInput
    { element = el
    , props = { type' = "number", extraClasses = ["bootforms-int"] }
    , decoder = String.toInt
    , autoBlur = True
    }

{-| A simple float input -}
floatInput : Element Float String () -> H.Html
floatInput el =
    basicInput
    { element = el
    , props = { type' = "number", extraClasses = ["bootforms-float"] }
    , decoder = String.toFloat
    , autoBlur = True
    }

{-| A simple date input -}
dateInput : Element Date.Date String () -> H.Html
dateInput el =
    basicInput
    { element = el
    , props = { type' = "text", extraClasses = ["bootforms-date"] }
    , decoder = Date.fromString
    , autoBlur = True
    }

{-| Time of day -}
type alias TimeOfDay =
    { hour: Int
    , minute: Int
    }

{-| A simple time input -}
timeInput : Element TimeOfDay String () -> H.Html
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
       , autoBlur = True
       }

{-| A simple checkbox input -}
checkBox : Element Bool e st -> H.Html
checkBox el =
    H.div [ A.class ("checkbox" ++ if isError el.value.value then " has-error" else "") ]
    [ H.label []
        [ flip H.input [] <|
            [ A.type' "checkbox"
            , A.id el.id
            , A.checked (Result.withDefault False el.value.value)
            , E.on "change" E.targetChecked <| \cb ->
                el.onValue <| noFx <| \curVal -> { curVal | userInput = if cb then "True" else "False", value = Ok cb }
            ] ++ (focusHandlers True el.onValue)
        , text (" " ++ el.label)
        ]
    , case el.helpBlock of
        Just val -> H.span [ A.class "help-block" ] [ text val ]
        Nothing -> H.span [] []
    ]

{-| Spec for selectBox -}
type alias SelectElement v e =
    FormElement v e ()
    { choices: List v
    , displayChoice: v -> String
    , choiceValue: v -> String
    }

{-| A simple dropdown -}
selectBox : SelectElement v e -> H.Html
selectBox sel =
    formGroup sel.element <|
    let el = sel.element
        val = el.value
        opts =
            flip List.map sel.props.choices <| \ch ->
            let valAttr =
                    A.value (sel.props.choiceValue ch)
                rest =
                    if Ok ch == val.value
                    then [A.selected True]
                    else []
            in H.option (valAttr :: rest) [ text <| sel.props.displayChoice ch ]
    in flip H.select opts <|
        [ A.id el.id
        , A.class "form-control"
        , E.on "change" E.targetValue <| \str ->
            el.onValue <| noFx <| \curVal ->
            { curVal
                | userInput = str
                , value = sel.decoder str
            }
        ] ++ (focusHandlers sel.autoBlur el.onValue)


{-| Check if the given FormValueSt contains a valid value -}
validFormValue : FormValueSt e t st -> Bool
validFormValue fv =
    case fv.value of
      Ok _ -> True
      Err _ -> False

{-| Read the current form value if available -}
getFormValue : FormValueSt e t st -> Maybe t
getFormValue fv =
    case fv.value of
      Ok val -> Just val
      Err _ -> Nothing

{-| Read the current form value or fallback to a default -}
getFormValueDef : t -> FormValueSt e t st -> t
getFormValueDef def = Maybe.withDefault def << getFormValue

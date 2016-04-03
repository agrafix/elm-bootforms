module Html.Form.Input.Suggest.External
  ( Suggestions, ExtSuggestiveInput, extSuggestiveTextInput
  , extSuggFormVal
  ) where
{-| This module implements an input that includes input suggestions coming from an external source,
such as an HTTP request.

# The input field
@docs Suggestions, extSuggFormVal, ExtSuggestiveInput, extSuggestiveTextInput
-}
import Char
import Html as H
import Html exposing (text)
import Html.Attributes as A
import Html.Events as E
import Html.Form.Input exposing (..)
import String
import Task
import Effects
import Json.Decode as Json

{-| A list of suggestions -}
type alias Suggestions = List String

{-| Spec for suggestive input with external data source.
-}
type alias ExtSuggestiveInput err e =
    FormElement String e Suggestions
    { maxSuggest: Int
    , minTyped: Int
    , loadSuggestions: String -> Task.Task err Suggestions
    }

{-| Generate a FormValue for a suggestive input boxes -}
extSuggFormVal : String -> FormValueSt e String Suggestions
extSuggFormVal str =
    { userInput = str
    , value = Ok str
    , focused = False
    , internalState = []
    }

{-| A suggestive input field. Note that suggestions require at least 3 typed characters -}
extSuggestiveTextInput : ExtSuggestiveInput err e -> H.Html
extSuggestiveTextInput sel =
    formGroup sel.element <|
    let el = sel.element
        val = el.value
        inLen = String.length el.value.userInput
        suggestions =
            if val.focused && inLen >= sel.props.minTyped
            then List.take (sel.props.maxSuggest) <| val.internalState
            else []
        mkTask oldFv t =
            FormValueActionSt
            { effect = Effects.none
            , action =
                case t of
                  Nothing -> identity
                  Just suggs ->
                      \fv ->
                      if oldFv.userInput == fv.userInput
                      then { fv | internalState = suggs }
                      else fv
            }
        mkEff fv =
            Effects.task <|
            Task.map (mkTask fv) <|
            Task.toMaybe <|
            sel.props.loadSuggestions fv.userInput
    in H.div []
        [ basicInputRawEff mkEff
            { element = el
            , props = { type' = "text", extraClasses = [] }
            , decoder = sel.decoder
            , autoBlur = False
            }
        , if List.length suggestions > 0 && not (List.member el.value.userInput suggestions)
          then H.ul [ A.class "form-suggest" ] <|
               flip List.map suggestions <| \sug ->
                  H.li []
                  [ H.button
                        [ A.class "btn btn-default btn-xs"
                        , E.onWithOptions "click" { stopPropagation = True, preventDefault = True } Json.value <| \_ ->
                            el.onValue <| noFx <| \curVal ->
                            { curVal
                                | userInput = sug
                                , value = sel.decoder sug
                                , focused = False
                            }
                        ] [text sug]
                  ]
          else H.span [] []
        ]

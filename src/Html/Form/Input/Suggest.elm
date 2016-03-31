module Html.Form.Input.Suggest
  ( SuggestiveInput, suggestiveTextInput
  ) where
{-| This module implements an input that includes input suggestions

# The input field
@docs SuggestiveInput, suggestiveTextInput
-}

import Date
import Fuzzy as F
import Html as H
import Html exposing (text)
import Html.Attributes as A
import Html.Events as E
import Html.Form.Input as I
import Json.Decode as Json
import String

{-| Spec for suggestive input -}
type alias SuggestiveInput e =
    I.FormElement String e
    { choices: List String
    , maxSuggest: Int
    , minTyped: Int
    }

{-| A suggestive input field -}
suggestiveTextInput : SuggestiveInput e -> H.Html
suggestiveTextInput sel =
    I.formGroup sel.element <|
    let el = sel.element
        val = el.value
        inLen = String.length el.value.userInput
        suggestions =
            if val.focused && inLen >= sel.props.minTyped
            then List.take (sel.props.maxSuggest) <|
                 List.sortBy (\x -> F.match [] [] el.value.userInput x |> .score) sel.props.choices
            else []
    in H.div []
        [ I.textInput sel.element
        , if List.length suggestions > 0 && not (List.member el.value.userInput suggestions)
          then H.ul [ A.class "form-suggest" ] <|
               flip List.map suggestions <| \sug ->
                  H.li []
                  [ H.button
                        [ A.class "btn btn-default btn-xs"
                        , E.on "click" Json.value <| \_ ->
                            el.onValue
                            { val
                                | userInput = sug
                                , value = sel.decoder sug
                            }
                        ] [text sug]
                  ]
          else H.span [] []
        ]

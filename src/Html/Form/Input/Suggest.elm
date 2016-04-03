module Html.Form.Input.Suggest
  ( SuggestiveInput, suggestiveTextInput
  , SearchIndex, makeSearchIndex
  ) where
{-| This module implements an input that includes input suggestions

# The search index
@docs SearchIndex, makeSearchIndex

# The input field
@docs SuggestiveInput, suggestiveTextInput
-}
import Char
import Dict
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
    { choices: SearchIndex
    , maxSuggest: Int
    , minTyped: Int
    }

{-| An abstract search index -}
type SearchIndex
    = SearchIndex
    { firstCharMap : Dict.Dict Char (List String)
    }

{-| Build a search index for a given list of searchable items -}
makeSearchIndex : List String -> SearchIndex
makeSearchIndex choices =
    let loop zs =
            case zs of
              [] -> Dict.empty
              (x :: xs) ->
                  case String.uncons x of
                    Nothing -> loop xs
                    Just (ch, _) ->
                        let action =
                                Dict.update (Char.toLower ch) <| \oldVal ->
                                case oldVal of
                                  Nothing -> Just [x]
                                  Just ys -> Just (x :: ys)
                        in action (loop xs)
    in SearchIndex
       { firstCharMap = loop choices
       }

fuzzySearch : String -> SearchIndex -> List String
fuzzySearch needle (SearchIndex idx) =
    case String.uncons needle of
      Nothing -> []
      Just (ch, _) ->
          case Dict.get (Char.toLower ch) idx.firstCharMap of
            Nothing -> []
            Just possible ->
              List.sortBy (\x -> F.match [] [] needle x |> .score) possible

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
                 fuzzySearch el.value.userInput sel.props.choices
            else []
    in H.div []
        [ I.basicInputRaw
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
                            el.onValue <| \curVal ->
                            { curVal
                                | userInput = sug
                                , value = sel.decoder sug
                                , focused = False
                            }
                        ] [text sug]
                  ]
          else H.span [] []
        ]

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
import IntDict
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
    I.FormElement String e ()
    { choices: SearchIndex
    , maxSuggest: Int
    , minTyped: Int
    }

type alias SearchEntry =
    { value : String
    , indexed : String
    }

{-| An abstract search index -}
type SearchIndex
    = SearchIndex
    { firstCharMap : IntDict.IntDict (List SearchEntry)
    }

{-| Compute the search index key for a search string. Note that the resulting int can not have more then 32 bits!
 -}
getIdxKey : String -> Maybe Int
getIdxKey str =
    case String.toList str of
      [] -> Nothing
      [a] -> Just (Char.toCode a)
      [a, b] -> Just (Char.toCode a * Char.toCode b * 100)
      (a :: b :: c :: _) -> Just (Char.toCode a + Char.toCode b * 100 + Char.toCode c * 10000)

{-| Build a search index for a given list of searchable items -}
makeSearchIndex : List String -> SearchIndex
makeSearchIndex choices =
    let loop zs =
            case zs of
              [] -> IntDict.empty
              (xRaw :: xs) ->
                  let x = String.toLower xRaw
                      entry = { value = xRaw, indexed = x }
                  in case getIdxKey x of
                       Nothing -> loop xs
                       Just k ->
                           let action =
                                   IntDict.update k <| \oldVal ->
                                   case oldVal of
                                     Nothing -> Just [entry]
                                     Just ys -> Just (entry :: ys)
                           in action (loop xs)
    in SearchIndex
       { firstCharMap = loop choices
       }

fuzzySearch : String -> SearchIndex -> List SearchEntry
fuzzySearch needleRaw (SearchIndex idx) =
    let needle = String.toLower needleRaw
    in case getIdxKey needle of
         Nothing -> []
         Just k ->
             case IntDict.get k idx.firstCharMap of
               Nothing -> []
               Just possible ->
                   List.sortBy (\x -> F.match [] [] needle x.indexed |> .score) possible

{-| A suggestive input field. Note that suggestions require at least 3 typed characters -}
suggestiveTextInput : SuggestiveInput e -> H.Html
suggestiveTextInput sel =
    I.formGroup sel.element <|
    let el = sel.element
        val = el.value
        inLen = String.length el.value.userInput
        suggestions =
            if val.focused && inLen >= sel.props.minTyped
            then List.map (.value) <|
                 List.take (sel.props.maxSuggest) <|
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
                            el.onValue <| I.noFx <| \curVal ->
                            { curVal
                                | userInput = sug
                                , value = sel.decoder sug
                                , focused = False
                            }
                        ] [text sug]
                  ]
          else H.span [] []
        ]

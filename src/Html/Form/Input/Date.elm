module Html.Form.Input.Date
  ( DatePickerInput, datePickerInput
  , displayDayGerman, displayMonthGerman
  ) where
{-| This module implements an input that includes a date picker

# The input field
@docs DatePickerInput, datePickerInput

# Configuration helpers
@docs displayDayGerman, displayMonthGerman
-}

import Date as D
import Date.Core as D
import Date.Field as D
import Html.Form.Input exposing (..)
import Html as H
import Html exposing (text)
import Html.Attributes as A
import Html.Events as E
import List as L
import List.Split as L
import Json.Decode as Json

{-| Spec for date picker input -}
type alias DatePickerInput e =
    FormElement D.Date e
    { displayMonth : D.Month -> String
    , displayDay : D.Day -> String
    , defaultDate : D.Date
    , encoder : D.Date -> String -- must match decoder!
    }

{-| Short german week days -}
displayDayGerman : D.Day -> String
displayDayGerman day =
    case day of
      D.Mon -> "Mo"
      D.Tue -> "Di"
      D.Wed -> "Mi"
      D.Thu -> "Do"
      D.Fri -> "Fr"
      D.Sat -> "Sa"
      D.Sun -> "So"

{-| German month names -}
displayMonthGerman : D.Month -> String
displayMonthGerman m =
    case m of
      D.Jan -> "Januar"
      D.Feb -> "Februar"
      D.Mar -> "März"
      D.Apr -> "April"
      D.May -> "Mai"
      D.Jun -> "Juni"
      D.Jul -> "Juli"
      D.Aug -> "August"
      D.Sep -> "September"
      D.Oct -> "Oktober"
      D.Nov -> "November"
      D.Dec -> "Dezember"

{-| An input field that shows a date picker below -}
datePickerInput : DatePickerInput e -> H.Html
datePickerInput dpi =
    formGroup dpi.element <|
    let el = dpi.element
        val = el.value
        apply d =
            el.onValue <| \curVal ->
            { curVal
                | userInput = dpi.props.encoder d
                , value = Ok d
            }
    in H.div []
         [ basicInputRaw
             { element = el
             , props = { type' = "text", extraClasses = ["ag-datepicker"] }
             , decoder = dpi.decoder
             , autoBlur = False
             }
         , monthTable apply dpi.props.displayDay dpi.props.displayMonth <|
             getFormValueDef dpi.props.defaultDate val
         ]

monthGrid : D.Date -> List (List (Maybe Int))
monthGrid now =
    let first = D.toFirstOfMonth now
        dayOfWeekFirst = D.isoDayOfWeek (D.dayOfWeek first) - 1
        last = D.lastOfMonthDate now
        lastDay = D.day last
        dayOfWeekLast = D.isoDayOfWeek (D.dayOfWeek last) - 1
        lastRowTil = 6 - dayOfWeekLast
        firstRowTil = dayOfWeekFirst
        fullList =
            L.repeat firstRowTil Nothing
            ++ L.map Just [1..lastDay]
            ++ L.repeat lastRowTil Nothing
    in L.chunksOfLeft 7 fullList

monthTable : (D.Date -> Signal.Message) -> (D.Day -> String) -> (D.Month -> String) -> D.Date -> H.Html
monthTable changeDate displayDay displayMonth now =
    let month = displayMonth (D.month now)
        day = D.day now
        totalDays = D.daysInMonthDate now
        dayOfWeak = D.isoDayOfWeek (D.dayOfWeek now) - 1
        dayHeaders =
          L.map (\x -> H.td [] [text x])
          [ displayDay D.Mon, displayDay D.Tue, displayDay D.Wed
          , displayDay D.Thu, displayDay D.Fri, displayDay D.Sat, displayDay D.Sun
          ]
        grid = monthGrid now
        calRows =
          flip L.map grid <| \dayRow ->
          H.tr [] (L.map dayCell dayRow)
        dayCell d =
          case d of
            Nothing -> H.td [] []
            Just i ->
              H.td
              [ A.class ("ag-cal-cell " ++ (if i == day then "info ag-cal-cell-active" else ""))
              , E.on "click" Json.value <| \_ -> changeDate (D.fieldToDateClamp (D.DayOfMonth i) now)
              ]
              [ text (toString i)
              ]
        navButton side f =
           H.button
              [ A.class ("btn btn-default btn-xs ag-cal-nav-btn ag-cal-nav-btn-" ++ side)
              , E.on "click" Json.value <| \_ ->
                changeDate (f now)
              ]
              [ H.span [ A.class ("glyphicon glyphicon-chevron-" ++ side) ] []
              ]
    in H.div [ A.class "ag-datepicker-table" ]
       [ H.div [ A.class "row" ]
           [ H.div [ A.class "col-xs-3" ]
               [ navButton "left" D.lastOfPrevMonthDate ]
           , H.div [ A.class "col-xs-6" ]
               [ H.h3 [] [ text month, text " ", text (toString <| D.year now) ] ]
           , H.div [ A.class "col-xs-3" ]
               [ navButton "right" D.firstOfNextMonthDate ]
           ]
       , H.table [ A.class "table table-bordered table-condensed"]
          [ H.tbody [] ( H.tr [] dayHeaders :: calRows ) ]
       ]

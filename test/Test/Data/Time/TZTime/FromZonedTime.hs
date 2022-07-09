-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Test.Data.Time.TZTime.FromZonedTime where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Time (LocalTime(..), TimeOfDay(..), TimeZone(..), ZonedTime(..), minutesToTimeZone)
import Data.Time.Calendar.Compat (pattern YearMonthDay)
import Data.Time.TZInfo as TZI
import Data.Time.TZTime as TZT
import Data.Time.TZTime.Internal (TZTime(UnsafeTZTime))
import Test.Tasty.HUnit
import Test.Utils

unit_fromZonedTimeStrict_with_correct_offset :: Assertion
unit_fromZonedTimeStrict_with_correct_offset = do
  let local = LocalTime (YearMonthDay 2022 3 4) (TimeOfDay 10 15 40)
  let tz = TZI.fromLabel TZI.Europe__Rome

  fromZonedTimeStrict tz (ZonedTime local (minutesToTimeZone 60)) @?=
    Right (UnsafeTZTime local tz (TimeZone 60 False "CET"))

unit_fromZonedTimeStrict_with_incorrect_offset :: Assertion
unit_fromZonedTimeStrict_with_incorrect_offset = do
  let local = LocalTime (YearMonthDay 2022 3 4) (TimeOfDay 10 15 40)
  let tz = TZI.fromLabel TZI.Europe__Rome

  fromZonedTimeStrict tz (ZonedTime local (minutesToTimeZone 0)) @?= Left
    (TZOInvalidOffset
      local
      0
      (UnsafeTZTime local tz (TimeZone 60 False "CET") :| [])
    )

unit_fromZonedTimeStrict_at_overlap_with_correct_offset :: Assertion
unit_fromZonedTimeStrict_at_overlap_with_correct_offset = do
  let local = LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 15 0)
  let tz = TZI.fromLabel TZI.America__Winnipeg

  fromZonedTimeStrict tz (ZonedTime local cst) @?= Right (UnsafeTZTime local tz cst)
  fromZonedTimeStrict tz (ZonedTime local cdt) @?= Right (UnsafeTZTime local tz cdt)

unit_fromZonedTimeStrict_at_overlap_with_incorrect_offset :: Assertion
unit_fromZonedTimeStrict_at_overlap_with_incorrect_offset = do
  let local = LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 15 0)
  let tz = TZI.fromLabel TZI.America__Winnipeg

  fromZonedTimeStrict tz (ZonedTime local (minutesToTimeZone 0)) @?= Left
    (TZOInvalidOffset
      local
      0
      (UnsafeTZTime local tz cdt :| [UnsafeTZTime local tz cst])
    )

unit_fromZonedTimeStrict_at_gaps :: Assertion
unit_fromZonedTimeStrict_at_gaps = do
  let local = LocalTime (YearMonthDay 2022 3 13) (TimeOfDay 2 15 0)
  let tz = TZI.fromLabel TZI.America__Winnipeg

  case fromZonedTimeStrict tz (ZonedTime local cst) of
    Left (TZOGap lt before after) ->
      fromLocalTimeStrict tz (local) @?= Left (TZGap lt before after)
    result ->
      assertFailure $ unlines
        [ "Expected 'fromZonedTimeStrict' to land on a gap."
        , "Actual result: "
        , show result
        ]

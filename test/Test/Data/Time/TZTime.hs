-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Data.Time.TZTime where

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Time
import Data.Time.Calendar.Compat (pattern YearMonthDay)
import Data.Time.TZInfo as TZI
import Data.Time.TZTime as TZT
import Data.Time.TZTime.Internal (TZTime(UnsafeTZTime))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit

cst :: TimeZone
cst = read "CST"

cdt :: TimeZone
cdt = read "CDT"

unit_fromLocalTime_at_gaps :: Assertion
unit_fromLocalTime_at_gaps = do
  -- At 01:59:59 on 2022-03-13, the America/Winnipeg time zone switches
  -- from CST (UTC-6) to CDT (UTC-5).
  -- In other words, the clocks “skip” 1 hour, straight to 03:00:00.
  --
  -- See: https://www.timeanddate.com/time/zone/canada/winnipeg?year=2022
  --
  -- Therefore, the local time "2:15" is invalid.
  -- `fromLocalTime` should return the local time shifted backwards / forwards
  -- by the duration of the gap (+/- 1 hour).
  let local = LocalTime (YearMonthDay 2022 3 13) (TimeOfDay 2 15 0)
  let tz = TZI.fromLabel TZI.America__Winnipeg

  TZT.fromLocalTime tz local @?= Left
    (TZGap local
      (UnsafeTZTime
        (LocalTime (YearMonthDay 2022 3 13) (TimeOfDay 1 15 0))
        tz cst
      )
      (UnsafeTZTime
        (LocalTime (YearMonthDay 2022 3 13) (TimeOfDay 3 15 0))
        tz cdt
      )
    )

test_fromLocalTime_at_odd_gaps :: [TestTree]
test_fromLocalTime_at_odd_gaps =
  [ testCase "Australia/Lord_Howe" do
      -- Australia/Lord_Howe turns the clocks forward 30 mins.
      --
      -- See: https://www.timeanddate.com/time/zone/australia/lord-howe-island?year=2022
      let local = LocalTime (YearMonthDay 2022 10 2) (TimeOfDay 2 7 0)
      let tz = TZI.fromLabel TZI.Australia__Lord_Howe

      TZT.fromLocalTime tz local @?= Left
        (TZGap local
          (UnsafeTZTime
            (LocalTime (YearMonthDay 2022 10 2) (TimeOfDay 1 37 0))
            tz
            (TimeZone (10 * 60 + 30) False "+1030")
          )
          (UnsafeTZTime
            (LocalTime (YearMonthDay 2022 10 2) (TimeOfDay 2 37 0))
            tz
            (TimeZone (11 * 60) True "+11")
          )
        )

  , testCase "Pacific/Apia" do
      -- On 2011-12-29, at 23:59:59, the island of Samoa switched from UTC-10 to UTC+14.
      -- In other words, the clocks were turned forward 24 hours,
      -- effectively skipping a whole calendar day (from 29 Dec to 31 Dec).
      --
      -- See: https://www.timeanddate.com/time/zone/samoa/apia?year=2011
      let local = LocalTime (YearMonthDay 2011 12 30) (TimeOfDay 10 7 0)
      let tz = TZI.fromLabel TZI.Pacific__Apia

      TZT.fromLocalTime tz local @?= Left
        (TZGap local
          (UnsafeTZTime
            (LocalTime (YearMonthDay 2011 12 29) (TimeOfDay 10 7 0))
            tz
            (TimeZone (-10 * 60) True "-10")
          )
          (UnsafeTZTime
            (LocalTime (YearMonthDay 2011 12 31) (TimeOfDay 10 7 0))
            tz
            (TimeZone (14 * 60) True "+14")
          )
        )
  ]

unit_fromLocalTime_at_overlaps :: Assertion
unit_fromLocalTime_at_overlaps = do
  -- At 01:59:59 on 2022-11-06, the America/Winnipeg time zone switches
  -- from CDT (UTC-5) to CST (UTC-6).
  -- In other words, the clocks are turned back 1 hour, back to 01:00:00.
  --
  -- See: https://www.timeanddate.com/time/zone/canada/winnipeg?year=2022
  --
  -- Therefore, the local time "01:15" happens twice: once at the -5 offset
  -- and again at the -6 offset.
  -- `fromLocalTime` should return both instants.
  let local = LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 15 0)
  let tz = TZI.fromLabel TZI.America__Winnipeg

  TZT.fromLocalTime tz local @?= Left
    (TZOverlap local
      (UnsafeTZTime
        (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 15 0))
        tz cdt
      )
      (UnsafeTZTime
        (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 15 0))
        tz cst
      )
    )

unit_fromLocalTime_at_odd_overlaps :: Assertion
unit_fromLocalTime_at_odd_overlaps = do
  -- Australia/Lord_Howe turns the clocks backwards 30 mins.
  --
  -- See: https://www.timeanddate.com/time/zone/australia/lord-howe-island?year=2022
  let local = LocalTime (YearMonthDay 2022 4 3) (TimeOfDay 1 45 0)
  let tz = TZI.fromLabel TZI.Australia__Lord_Howe

  TZT.fromLocalTime tz local @?= Left
    (TZOverlap local
      (UnsafeTZTime
        (LocalTime (YearMonthDay 2022 4 3) (TimeOfDay 1 45 0))
        tz
        (TimeZone (11 * 60) True "+11")
      )
      (UnsafeTZTime
        (LocalTime (YearMonthDay 2022 4 3) (TimeOfDay 1 45 0))
        tz
        (TimeZone (10 * 60 + 30) False "+1030")
      )
    )

unit_atStartOfDay_at_gaps :: Assertion
unit_atStartOfDay_at_gaps = do
  -- On this date in Havana, at 23:59, the clocks are turned forward 1 hour,
  -- which means midnight never happens.
  --
  -- `atStartOfDay` should return the 01:00, the earliest time possible for that day.
  --
  -- See: https://www.timeanddate.com/time/zone/cuba/havana?year=2022
  let tz = TZI.fromLabel TZI.America__Havana
  let tzt = unsafeFromLocalTime tz (LocalTime (YearMonthDay 2022 3 13) (TimeOfDay 10 45 0))
  let result = atStartOfDay tzt

  result @?=
    UnsafeTZTime
      (LocalTime (YearMonthDay 2022 3 13) (TimeOfDay 1 0 0))
      tz
      (TimeZone (-4 * 60) True "CDT")

test_atStartOfDay_at_overlaps :: [TestTree]
test_atStartOfDay_at_overlaps =
  [ testCase "America/Havana" do
      -- On this date in Havana, at 01:00, the clocks are turned backwards 1 hour,
      -- which means midnight happens twice.
      --
      -- `atStartOfDay` should return the earliest of the two midnights.
      --
      -- See: https://www.timeanddate.com/time/zone/cuba/havana?year=2022
      let tz = TZI.fromLabel TZI.America__Havana
      let tzt = unsafeFromLocalTime tz (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 10 45 0))
      let result = atStartOfDay tzt

      result @?=
        UnsafeTZTime
          (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 0 0 0))
          tz
          (TimeZone (-4 * 60) True "CDT")

      -- Sanity checks
      -- Make sure this really is an overlap
      assertBool "Expected `result` to land on an overlap" $
        (result & toUTC) < (result & atLatestOffset & toUTC)
      -- This really is the earliest offset
      result @?= atEarliestOffset result

  , testCase "America/St_Johns" do
      -- Up until 2010, in America/St_Johns, the clocks were turned backwards
      -- 1 hour in autumn at 00:01 back to 23:01 the previous day, meaning the **date**
      -- would be moved backwards.
      --
      -- See:
      --   * https://davecturner.github.io/2017/12/27/timezone-curiosities.html
      --   * https://www.timeanddate.com/time/zone/canada/st-johns?year=2009
      let tz = TZI.fromLabel TZI.America__St_Johns
      let tzt = unsafeFromLocalTime tz (LocalTime (YearMonthDay 2009 11 1) (TimeOfDay 10 45 0))
      let result = atStartOfDay tzt

      result @?=
        UnsafeTZTime
          (LocalTime (YearMonthDay 2009 11 1) (TimeOfDay 0 0 0))
          tz
          (TimeZone (-2 * 60 - 30) True "NDT")

      -- Sanity checks
      -- Make sure this really is an overlap
      assertBool "Expected `result` to land on an overlap" $
        (result & toUTC) < (result & atLatestOffset & toUTC)
      -- This really is the earliest offset
      result @?= atEarliestOffset result
  ]

unit_addTime_and_diffTZTime_operate_on_the_universal_time_line :: Assertion
unit_addTime_and_diffTZTime_operate_on_the_universal_time_line = do
  -- At 01:59:59 on 2022-11-06, the America/Winnipeg time zone switches
  -- from CDT (UTC-5) to CST (UTC-6).
  -- In other words, the clocks are turned back 1 hour, back to 01:00:00.
  let local = LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 0 30 0)
  let tz = TZI.fromLabel TZI.America__Winnipeg
  let tzt = unsafeFromLocalTime tz local

  addTime (standardHours 1) tzt @?=
    UnsafeTZTime (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 30 0)) tz cdt

  addTime (standardHours 2) tzt @?=
    UnsafeTZTime (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 1 30 0)) tz cst

  addTime (standardHours 3) tzt @?=
    UnsafeTZTime (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 2 30 0)) tz cst

  diffTZTime (addTime (standardHours 1) tzt) tzt @?= standardHours 1
  diffTZTime (addTime (standardHours 2) tzt) tzt @?= standardHours 2
  diffTZTime (addTime (standardHours 3) tzt) tzt @?= standardHours 3

unit_composing_modifiers :: Assertion
unit_composing_modifiers = do
  let tz = TZI.fromLabel TZI.America__Havana
  let tzt = unsafeFromLocalTime tz (LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 10 45 0))

  let result =
        tzt
          & modifyLocalLenient (
              addCalendarClip (calendarMonths 2 <> calendarDays 3)
              >>> atFirstDayOfWeekOnAfter Wednesday
              >>> atMidnight
            )
          & atEarliestOffset
          & addTime (standardMinutes 1 + standardSeconds 20)

  result @?= unsafeFromLocalTime tz
    (LocalTime (YearMonthDay 2023 1 11) (TimeOfDay 0 1 20))

unit_addCalendar_zero_equals_id :: Assertion
unit_addCalendar_zero_equals_id = do
  -- When using "lenient" behavior (i.e. `modifyLocalLenient`),
  -- adding 0 days should be equivalent to the identity function.
  let tz = TZI.fromLabel TZI.America__Winnipeg
  let mkLocalTime hh mm ss = LocalTime (YearMonthDay 2022 11 6) (TimeOfDay hh mm ss)

  check $ unsafeFromLocalTime tz (mkLocalTime 0 0 0)
  check $ unsafeFromLocalTime tz (mkLocalTime 2 0 0)
  check $ fromLocalTime tz (mkLocalTime 1 0 0) & \(Left (TZOverlap _ earliest _)) -> earliest
  check $ fromLocalTime tz (mkLocalTime 1 0 0) & \(Left (TZOverlap _ _ latest)) -> latest
  check $ fromLocalTime tz (mkLocalTime 1 30 0) & \(Left (TZOverlap _ earliest _)) -> earliest
  check $ fromLocalTime tz (mkLocalTime 1 30 0) & \(Left (TZOverlap _ _ latest)) -> latest
  check $ fromLocalTime tz (mkLocalTime 1 59 0) & \(Left (TZOverlap _ earliest _)) -> earliest
  check $ fromLocalTime tz (mkLocalTime 1 59 0) & \(Left (TZOverlap _ _ latest)) -> latest

  where
    check :: TZTime -> Assertion
    check tzt = do
      modifyLocalLenient (addCalendarClip (calendarDays 0)) tzt @?= tzt

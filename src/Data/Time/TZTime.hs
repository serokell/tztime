-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Data.Time.TZTime
  (
  -- * TZTime
    Internal.TZTime
  , Internal.tzTimeLocalTime
  , Internal.tzTimeTZ
  , Internal.tzTimeOffset
  -- * Constructors
  , Internal.fromUTC
  , Internal.fromLocalTime
  , Internal.TZResult(..)
  , Internal.TZError(..)
  , Internal.fromLocalTimeError
  , Internal.fromLocalTimeThrow
  , Internal.unsafeFromLocalTime
  -- * Conversions
  , Internal.toUTC
  , Internal.toZonedTime
  , Internal.inTZ
  -- * Modifying a TZTime
  , atEarliestOffset
  , atLatestOffset
  , atStartOfDay
  -- * Universal time-line
  -- ** Adding seconds/minutes/hours
  , addTime
  , standardHours
  , standardMinutes
  , standardSeconds
  -- * Local time-line
  , modifyLocalStrict
  , modifyLocalLenient
  , modifyLocalStrictError
  , modifyLocalStrictThrow
  -- ** Adding days/weeks/months/years.
  , addCalendarClip
  , addCalendarRollOver
  , calendarDays
  , calendarWeeks
  , calendarMonths
  , calendarYears
  -- * Setting date/time components.
  , atYear
  , atMonthOfYear
  , atDayOfMonth
  , atDay
  , atHour
  , atMinute
  , atSecond
  , atTimeOfDay
  , atMidnight
  , atFirstDayOfWeekOnAfter
  -- * Other
  , diffTZTime
  ) where

import Control.Exception.Safe (MonadThrow, throwM)
import Control.Monad.Except (MonadError, liftEither)
import Data.Fixed (Pico)
import Data.Time
  (CalendarDiffDays(..), Day, DayOfWeek(..), LocalTime(..), NominalDiffTime, TimeOfDay(..),
  addUTCTime, diffUTCTime, midnight, secondsToNominalDiffTime)
import Data.Time qualified as Time
import Data.Time.Calendar.Compat
  (DayOfMonth, MonthOfYear, Year, firstDayOfWeekOnAfter, pattern YearMonthDay)
import Data.Time.TZTime.Internal as Internal

-- $setup
-- >>> import Data.Function ((&))

----------------------------------------------------------------------------
-- Modifying a TZTime
----------------------------------------------------------------------------

-- | If this local time happens to be on an overlap,
-- switch to the earliest of the two offsets.
atEarliestOffset :: TZTime -> TZTime
atEarliestOffset tzt =
  case fromLocalTime (tzTimeTZ tzt) (tzTimeLocalTime tzt) of
    Overlap _ earliest _ -> earliest
    _ -> tzt

-- | If this local time happens to be on an overlap,
-- switch to the latest of the two offsets.
atLatestOffset :: TZTime -> TZTime
atLatestOffset tzt =
  case fromLocalTime (tzTimeTZ tzt) (tzTimeLocalTime tzt) of
    Overlap _ _ latest -> latest
    _ -> tzt

-- | Changes the time to the earliest time possible on that day.
--
-- This is usually 00:00, but, if, on that day:
--
-- * the clocks are turned, for example, from 23:59 to 01:00 and midnight is skipped,
--   this will return 01:00.
-- * the clocks are turned, for example, from 01:00 to 00:00 and midnight happens twice,
--   this will return the first occurrence (i.e. midnight at the earliest offset).
atStartOfDay :: TZTime -> TZTime
atStartOfDay tzt =
  case Internal.modifyLocalTimeLine atMidnight tzt of
    Unique unique -> unique
    Gap _ _ after -> after
    Overlap _ atEarliestOffset _ -> atEarliestOffset

----------------------------------------------------------------------------
-- Adding seconds/minutes/hours.
----------------------------------------------------------------------------

-- | Adds the given amount of seconds
addTime :: NominalDiffTime -> TZTime -> TZTime
addTime = Internal.modifyUniversalTimeLine . addUTCTime

-- | A standard hour of 3600 seconds.
standardHours :: Pico -> NominalDiffTime
standardHours h = standardMinutes (h * 60)

-- | A standard minute of 60 seconds.
standardMinutes :: Pico -> NominalDiffTime
standardMinutes m = standardSeconds (m * 60)

standardSeconds :: Pico -> NominalDiffTime
standardSeconds = secondsToNominalDiffTime

----------------------------------------------------------------------------
-- Local time-line.
----------------------------------------------------------------------------

-- | Modifies the date/time on the local time-line.
--
-- The result of the modification may be:
--
-- * A valid `TZTime`.
-- * Ambiguous: this usually happens when the clocks are set back in
--   autumn and a local time happens twice.
-- * Invalid: this usually happens when the clocks are set forward in
--   spring and a local time is skipped.
modifyLocalStrict :: (LocalTime -> LocalTime) -> TZTime -> TZResult
modifyLocalStrict = Internal.modifyLocalTimeLine

-- | Similar to `modifyLocalStrict`, except:
--
-- If the result lands on a gap, shift the time forward by
-- the duration of the gap.
--
-- If it lands on an overlap, attempt to preserve the offset of the
-- original `LocalTime`.
-- This ensures that @addCalendarClip (calendarDays 0) == id@.
-- If this is not possible, use the earliest offset.
modifyLocalLenient :: (LocalTime -> LocalTime) -> TZTime -> TZTime
modifyLocalLenient f tzt =
  case Internal.modifyLocalTimeLine f tzt of
    Unique unique -> unique
    Gap _ _ after -> after
    Overlap _ atEarliestOffset atLatestOffset
      | tzTimeOffset atLatestOffset == tzTimeOffset tzt -> atLatestOffset
      | otherwise -> atEarliestOffset

-- | Similar to `modifyLocalStrict`, but throws a `TZError` in `MonadError`
-- if the result lands in a gap/overlap.
modifyLocalStrictError :: MonadError TZError m => (LocalTime -> LocalTime) -> TZTime -> m TZTime
modifyLocalStrictError f =
  liftEither . Internal.tzResultToError . Internal.modifyLocalTimeLine f

-- | Similar to `modifyLocalStrict`, but throws a `TZError` in `MonadThrow`
-- if the result lands in a gap/overlap.
modifyLocalStrictThrow :: MonadThrow m => (LocalTime -> LocalTime) -> TZTime -> m TZTime
modifyLocalStrictThrow f =
  either throwM pure . Internal.tzResultToError . Internal.modifyLocalTimeLine f

----------------------------------------------------------------------------
-- Adding days/weeks/months/years.
----------------------------------------------------------------------------

-- | Add the given number of months first and then the given number of days,
-- using the proleptic Gregorian calendar.
--
-- When adding months, days past the last day of the month are clipped to the last day.
-- For instance, 2005-01-30 + 1 month = 2005-02-28.
addCalendarClip :: CalendarDiffDays -> LocalTime -> LocalTime
addCalendarClip cdd lt = lt
  { localDay = Time.addGregorianDurationClip cdd $ localDay lt
  }

-- | Add the given number of months first and then the given number of days.
-- using the proleptic Gregorian calendar.
--
-- When adding months, days past the last day of the month roll over to the next month.
-- For instance, 2005-01-30 + 1 month = 2005-03-02.
addCalendarRollOver :: CalendarDiffDays -> LocalTime -> LocalTime
addCalendarRollOver cdd lt = lt
  { localDay = Time.addGregorianDurationRollOver cdd $ localDay lt
  }

calendarDays :: Integer -> CalendarDiffDays
calendarDays n = CalendarDiffDays 0 n

calendarWeeks :: Integer -> CalendarDiffDays
calendarWeeks n = CalendarDiffDays 0 (n * 7)

calendarMonths :: Integer -> CalendarDiffDays
calendarMonths n = CalendarDiffDays n 0

calendarYears :: Integer -> CalendarDiffDays
calendarYears n = CalendarDiffDays (n * 12) 0

----------------------------------------------------------------------------
-- Setting date/time components.
----------------------------------------------------------------------------

-- | Sets the year using the proleptic Gregorian calendar.
atYear :: Year -> LocalTime -> LocalTime
atYear y lt =
  let YearMonthDay _ moy dom = localDay lt
  in  lt { localDay = YearMonthDay y moy dom }

-- | Sets the month using the proleptic Gregorian calendar.
-- Invalid values will be clipped to the correct range.
atMonthOfYear :: MonthOfYear -> LocalTime -> LocalTime
atMonthOfYear moy lt =
  let YearMonthDay y _ dom = localDay lt
  in  lt { localDay = YearMonthDay y moy dom }

-- | Sets the day of month using the proleptic Gregorian calendar.
-- Invalid values will be clipped to the correct range.
atDayOfMonth :: DayOfMonth -> LocalTime -> LocalTime
atDayOfMonth dom lt =
  let YearMonthDay y m _ = localDay lt
  in  lt { localDay = YearMonthDay y m dom }

-- | Sets the day.
atDay :: Day -> LocalTime -> LocalTime
atDay day lt = lt { localDay = day }

atHour :: Int -> LocalTime -> LocalTime
atHour h lt = lt
  { localTimeOfDay = (localTimeOfDay lt) { todHour = h }
  }

atMinute :: Int -> LocalTime -> LocalTime
atMinute m lt = lt
  { localTimeOfDay = (localTimeOfDay lt) { todMin = m }
  }

atSecond :: Pico -> LocalTime -> LocalTime
atSecond s lt = lt
  { localTimeOfDay = (localTimeOfDay lt) { todSec = s }
  }

atTimeOfDay :: TimeOfDay -> LocalTime -> LocalTime
atTimeOfDay tod lt = lt { localTimeOfDay = tod }

-- | Sets the time to 00:00.
atMidnight :: LocalTime -> LocalTime
atMidnight = atTimeOfDay midnight

-- | Moves the date to the next given `DayOfWeek`.
-- If the current date is already a match, then the current date is returned unmodified.
--
-- >>> import Data.Time.Zones.All as TZ
-- >>> tz = TZ.tzByLabel TZ.Europe__London
-- >>> tzt = unsafeFromLocalTime tz (LocalTime (YearMonthDay 2022 2 24) (TimeOfDay 10 0 0))
-- >>> tzt
-- 2022-02-24 10:00:00 GMT
--
-- >>> tzt & modifyLocalLenient (atFirstDayOfWeekOnAfter Thursday)
-- 2022-02-24 10:00:00 GMT
--
-- >>> tzt & modifyLocalLenient (atFirstDayOfWeekOnAfter Wednesday)
-- 2022-03-02 10:00:00 GMT
atFirstDayOfWeekOnAfter :: DayOfWeek -> LocalTime -> LocalTime
atFirstDayOfWeekOnAfter dow lt = lt
  { localDay = firstDayOfWeekOnAfter dow $ localDay lt
  }

----------------------------------------------------------------------------
-- Other
----------------------------------------------------------------------------

-- | Calculate the difference in seconds between two points in time.
diffTZTime :: TZTime -> TZTime -> NominalDiffTime
diffTZTime tzt1 tzt2 =
  diffUTCTime (toUTC tzt1) (toUTC tzt2)

<!--
-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0
-->

# tztime

This package introduces:
* The `TZTime` data type, a valid and unambiguous point in time in some time zone.
* Functions for safely manipulating a `TZTime`.

## Motivation

Note: We'll use the packages `time`, `time-compat` and `tz` for the examples below.

```hs
λ> :m +Data.Time.LocalTime Data.Time.Clock Data.Time.Calendar.Compat
λ> import qualified Data.Time.Zones as TZ
λ> import qualified Data.Time.Zones.All as TZ
```

Manipulating time in a given time zone is not trivial.

Depending on what you need to do, it may make sense to modify the local time-line
(i.e. using `time`'s [`LocalTime`](https://hackage.haskell.org/package/time/docs/Data-Time-LocalTime.html#t:LocalTime)),
or the universal time-line (i.e. convert the time to `UTCTime`, modify it, convert it back
to `LocalTime`).

For example, if you want to add a certain number of hours to the current time,
then modifying the local time-line may end up adding more time than you intended:

```hs
λ> -- It's 00:30 on 2022-11-06 in the America/Winnipeg time zone.
λ> tz = TZ.tzByLabel TZ.America__Winnipeg
λ> t1 = LocalTime (YearMonthDay 2022 11 6) (TimeOfDay 0 30 0)
λ> t1
2022-11-06 00:30:00

λ> -- We naively add 4 hours to the local time.
λ> t2 = addLocalTime (secondsToNominalDiffTime 4 * 60 * 60) t1
λ> t2
2022-11-06 04:30:00

λ> -- Let's use the `tz` package to convert these times to UTC and
λ> -- see how many hours have actually passed between t1 and t2.
λ> TZ.LTUUnique t1utc _ = TZ.localTimeToUTCFull tz t1
λ> TZ.LTUUnique t2utc _ = TZ.localTimeToUTCFull tz t2
λ> nominalDiffTimeToSeconds (diffUTCTime t2utc t1utc) / 60 / 60
5.000000000000
```

We've accidentally landed 5 hours ahead, not 4 as we wanted.
This happened because on that day, at 01:59,
[the America/Winnipeg time zone switched](https://www.timeanddate.com/time/zone/canada/winnipeg?year=2022)
from the CDT offset (UTC-5) to the CST offset (UTC-6).
In other words, the clocks were turned back 1 hour, back to 01:00:00.

If the clocks had been turned forward, as is done in many time zones in spring, then we would have
added only 3 hours instead of 4.

If we add just 1 hour, we'll run into yet another issue:

```hs
λ> -- We naively add 1 hour to the local time.
λ> t2 = addLocalTime (secondsToNominalDiffTime 1 * 60 * 60) t1
λ> t2
2022-11-06 01:30:00

λ> -- Let's try converting t2 to UTC
λ> TZ.localTimeToUTCFull tz t2
LTUAmbiguous
  { _ltuFirst = 2022-11-06 06:30:00 UTC
  , _ltuSecond = 2022-11-06 07:30:00 UTC
  , _ltuFirstZone = CDT
  , _ltuSecondZone = CST
  }
```

We landed on an _ambiguous_ local time.
Since the clocks were turned back, there's an _overlap_: the time 01:30 happened _twice_ on that day.
Once at UTC-5 and again at UTC-6.

On the other hand, when the clock are turned forward, there is a _gap_ in the local time-line and we risk
accidentally constructing an _invalid_ `LocalTime` that never occurred in that time zone.

For these reasons, when adding a certain number of hours/minutes/seconds, you _probably_ want to do it
in the universal time-line instead.

----

Now say you want to add a certain number of days instead.
Doing that on the universal time-line may end up working not quite as expected:

```hs
λ> -- It's 23:30 on 2022-03-12 in the America/Winnipeg time zone.
λ> tz = TZ.tzByLabel TZ.America__Winnipeg
λ> t1 = LocalTime (YearMonthDay 2022 3 12) (TimeOfDay 23 30 0)

λ> -- Convert to UTC, add 1 day, convert back to our time zone.
λ> TZ.LTUUnique t1utc _ = TZ.localTimeToUTCFull tz t1
λ> t2utc = addUTCTime nominalDay t1utc
λ> TZ.utcToLocalTimeTZ tz t2utc
2022-03-14 00:30:00
```

We've accidentally landed _two_ days ahead instead of just one.
This happened because, on 2022-03-13, the clocks were turned forward 1 hour,
so that day only had 23 hours on that time zone.
Adding 24 hours on the universal time-line ended up being too much.

In the local time-line, some days may have 23 hours, 25 hours,
23 hours and 30 minutes (e.g. in the Australia/Lord_Howe time zone), etc,
depending on the offset transitions defined by each time zone.

For this reason, when you want to add days/weeks/months/years, you
_probably_ want to do it in the local time-line and then check if you landed
on a gap or an overlap and correct accordingly.

----

This package aims to:
1. make it easier to do "the right thing" and harder to do "the wrong thing".
2. ensure you don't accidentally end up with an invalid or ambiguous local time.

Here's how you'd do the above using `tztime`:

```hs
λ> import Data.Time.TZTime
λ> import Data.Time.TZTime.QQ (tz)
```

```hs
λ> t1 = [tz|2022-11-06 00:30:00 [America/Winnipeg]|]
λ> t2 = addTime (standardHours 4) t1
λ> t2
2022-11-06 03:30:00 -06:00 [America/Winnipeg]

λ> nominalDiffTimeToSeconds (diffTZTime t2 t1) / 60 / 60
4.000000000000
```

```hs
λ> t1 = [tz|2022-03-12 23:30:00 [America/Winnipeg]|]
λ> modifyLocalLenient (addCalendarClip (calendarDays 1)) t1
2022-03-13 23:30:00 -05:00 [America/Winnipeg]
```

-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Data.Time.TZTime.QQ
  ( tz
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Time.TZTime.Internal qualified as I
import Language.Haskell.TH (Exp, Q, unTypeCode)
import Language.Haskell.TH.Quote

{- |
Quasiquoter for parsing a `TZTime` at compile-time in the format:
@yyyy-mm-dd hh:mm:ss[.sss] [±hh:mm] [time zone]@.

The offset is optional, except when the local time is ambiguous
(i.e. when the clocks are set back around that time in that time zone).

The offset can also be expressed using [military time zone abbreviations](https://www.timeanddate.com/time/zones/military),
and these time zones abbreviations as per RFC 822 section 5:
\"UTC\", \"UT\", \"GMT\", \"EST\", \"EDT\", \"CST\", \"CDT\", \"MST\", \"MDT\", \"PST\", \"PDT\".

Note: the time zone's rules are loaded from the embedded database using `fromIdentifier`.

>>> [tz|2022-03-04 10:15:40.123 [Europe/Rome]|]
2022-03-04 10:15:40.123 +01:00 [Europe/Rome]

>>> [tz|2022-11-06 01:30:00 [America/Winnipeg]|]
Ambiguous time: please specify an offset.
Did you mean any of the following?
  - 2022-11-06 01:30:00 -05:00 [America/Winnipeg]
  - 2022-11-06 01:30:00 -06:00 [America/Winnipeg]

>>> [tz|2022-03-13 02:30:00 [America/Winnipeg]|]
Invalid time: the clocks are set forward around this time.
Did you mean any of the following?
  - 2022-03-13 01:30:00 -06:00 [America/Winnipeg]
  - 2022-03-13 03:30:00 -05:00 [America/Winnipeg]

>>> [tz|2022-03-04 02:02:01 +12:34 [Europe/Rome]|]
Invalid offset: +12:34
Did you mean any of the following?
  - 2022-03-04 02:02:01 +01:00 [Europe/Rome]

-}
tz :: QuasiQuoter
tz = QuasiQuoter
  { quoteExp = qexp
  , quotePat = \_ -> fail "Cannot use 'tz' as a pattern."
  , quoteType = \_ -> fail "Cannot use 'tz' as a type."
  , quoteDec = \_ ->fail "Cannot use 'tz' as a declaration."
  }
  where
    qexp :: String -> Q Exp
    qexp input = do
      (lt, offsetMaybe, ident) <- I.readP_to_Q input I.readComponentsP
      I.getValidTZTimes lt ident >>= I.checkOffset offsetMaybe >>= \case
        tzt :| [] -> unTypeCode $ I.liftTZTime tzt
        tzts -> fail $ "Ambiguous time: please specify an offset.\n" <> I.mkSuggestions tzts

-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Data.Time.TZTime.Internal where

import Control.Applicative (optional)
import Control.DeepSeq (NFData)
import Control.Exception.Safe (Exception(..), MonadThrow, throwM)
import Control.Monad.Except (MonadError, liftEither)
import Data.Data (Data)
import Data.Fixed (Fixed(..), Pico)
import Data.Functor (void, (<&>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, secondsToNominalDiffTime)
import Data.Time qualified as Time
import Data.Time.Compat (pattern YearMonthDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime
import Data.Time.TZInfo (TZIdentifier(..), TZInfo(..), fromIdentifier)
import Data.Time.Zones (LocalToUTCResult(..))
import Data.Time.Zones qualified as TZ
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Syntax (Code, Q, Quote)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as P

----------------------------------------------------------------------------
-- TZTime
----------------------------------------------------------------------------

-- | A valid and unambiguous point in time in some time zone.
data TZTime = UnsafeTZTime
  { tztLocalTime :: LocalTime
  , tztTZInfo :: TZInfo
  , tztOffset :: Time.TimeZone
  }
  deriving stock (Eq, Data, Generic)
  deriving anyclass NFData

instance Show TZTime where
  show (UnsafeTZTime lt tzi offset) =
    show lt <> " " <> iso8601Show offset <> " [" <> tzIdent <> "]"
    where
      tzIdent = T.unpack $ unTZIdentifier $ tziIdentifier tzi

-- | The local time of this `TZTime`.
tzTimeLocalTime :: TZTime -> LocalTime
tzTimeLocalTime = tztLocalTime

-- | The time zone of this `TZTime`.
tzTimeTZInfo :: TZTime -> TZInfo
tzTimeTZInfo = tztTZInfo

-- | The offset observed in this time zone at this moment in time.
tzTimeOffset :: TZTime -> TimeZone
tzTimeOffset = tztOffset

----------------------------------------------------------------------------
-- Constructors
----------------------------------------------------------------------------

-- | Converts a `UTCTime` to the given time zone.
fromUTC :: TZInfo -> UTCTime -> TZTime
fromUTC tzi utct =
  UnsafeTZTime
    { tztLocalTime = TZ.utcToLocalTimeTZ (tziRules tzi) utct
    , tztTZInfo = tzi
    , tztOffset = TZ.timeZoneForUTCTime (tziRules tzi) utct
    }

-- | Constructs a `TZTime` from a local time in the given time zone.
fromLocalTime :: TZInfo -> LocalTime -> Either TZError TZTime
fromLocalTime tzi lt =
  case TZ.localTimeToUTCFull (tziRules tzi) lt of
    LTUUnique _utc namedOffset ->
      Right $ UnsafeTZTime lt tzi namedOffset
    LTUAmbiguous _utc1 _utc2 namedOffset1 namedOffset2 ->
      Left $ TZOverlap lt
        (UnsafeTZTime lt tzi namedOffset1)
        (UnsafeTZTime lt tzi namedOffset2)
    LTUNone utcAfter offsetBefore ->
      let
        offsetAfter = TZ.timeZoneForUTCTime (tziRules tzi) utcAfter
        gap = secondsToNominalDiffTime $ 60 *
          fromIntegral @Int @Pico (timeZoneMinutes offsetAfter - timeZoneMinutes offsetBefore)
        utcBefore = addUTCTime (- gap) utcAfter
      in
        Left $ TZGap lt
          (UnsafeTZTime (TZ.utcToLocalTimeTZ (tziRules tzi) utcBefore) tzi offsetBefore)
          (UnsafeTZTime (TZ.utcToLocalTimeTZ (tziRules tzi) utcAfter) tzi offsetAfter)

-- | Similar to `fromLocalTime`, but throws a `TZError` in `MonadError`
-- if the local time is ambiguous/invalid.
fromLocalTimeError :: MonadError TZError m => TZInfo -> LocalTime -> m TZTime
fromLocalTimeError tzi =
  liftEither . fromLocalTime tzi

-- | Similar to `fromLocalTime`, but throws a `TZError` in `MonadThrow`
-- if the local time is ambiguous/invalid.
fromLocalTimeThrow :: MonadThrow m => TZInfo -> LocalTime -> m TZTime
fromLocalTimeThrow tzi =
  either throwM pure . fromLocalTime tzi

-- | Similar to `fromLocalTime`, but throws an `error`
-- if the local time is ambiguous/invalid.
unsafeFromLocalTime :: HasCallStack => TZInfo -> LocalTime -> TZTime
unsafeFromLocalTime tzi lt =
  case fromLocalTimeError tzi lt of
    Right tzt -> tzt
    Left err -> error $ "unsafeFromLocalTime: " <> displayException err

----------------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------------

-- | Converts this moment in time to the universal time-line.
toUTC :: TZTime -> UTCTime
toUTC tzt =
  localTimeToUTC (tzTimeOffset tzt) (tzTimeLocalTime tzt)

-- | Converts this moment in time to a `ZonedTime` (discarding time zone rules).
toZonedTime :: TZTime -> ZonedTime
toZonedTime tzt = ZonedTime (tzTimeLocalTime tzt) (tzTimeOffset tzt)

-- | Converts this moment in time to some other time zone.
inTZ :: TZInfo -> TZTime -> TZTime
inTZ tzi = fromUTC tzi . toUTC

----------------------------------------------------------------------------
-- Modifying a TZTime
----------------------------------------------------------------------------

-- | Modify this moment in time along the universal time-line.
modifyUniversalTimeLine :: (UTCTime -> UTCTime) -> TZTime -> TZTime
modifyUniversalTimeLine f tzt =
  fromUTC (tzTimeTZInfo tzt) . f . toUTC $ tzt

-- | Modify this moment in time along the local time-line.
modifyLocalTimeLine :: (LocalTime -> LocalTime) -> TZTime -> Either TZError TZTime
modifyLocalTimeLine f tzt =
  fromLocalTime (tzTimeTZInfo tzt) . f . tzTimeLocalTime $ tzt

-- | Attempted to construct a `TZTime` from an invalid or ambiguous `LocalTime`.
data TZError
  = TZOverlap
      LocalTime
      -- ^ The `LocalTime` is ambiguous.
      -- This usually happens when the clocks are set back in
      -- autumn and a local time happens twice.
      ~TZTime -- ^ The first occurrence of the given `LocalTime`, at the earliest offset.
      ~TZTime -- ^ The second occurrence of the given `LocalTime`, at the latest offset.
  | TZGap
      LocalTime
      -- ^ The `LocalTime` is invalid.
      -- This usually happens when the clocks are set forward in
      -- spring and a local time is skipped.
      ~TZTime -- ^ The given `LocalTime` adjusted back by the length of the gap.
      ~TZTime -- ^ The given `LocalTime` adjusted forward by the length of the gap.
  deriving stock (Eq, Generic)
  deriving anyclass (NFData)

instance Show TZError where
  show = displayException

instance Exception TZError where
  displayException = \case
    TZGap lt tzt1 _ ->
      "The local time "
      <> show lt
      <> " is invalid in the time zone "
      <> show (tziIdentifier $ tztTZInfo tzt1)
      <> "."
    TZOverlap lt tzt1 tzt2 ->
      "The local time "
      <> show lt
      <> " is ambiguous in the time zone "
      <> show (tziIdentifier $ tztTZInfo tzt1)
      <> ": it is observed at the offsets '"
      <> iso8601Show (tzTimeOffset tzt1)
      <> "' and '"
      <> iso8601Show (tzTimeOffset tzt2)
      <> "'."

----------------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------------

{- |
@yyyy-mm-dd hh:mm:ss[.sss] [±hh:mm] [time zone]@.
Example: @2022-03-04 02:02:01 +01:00 [Europe/Rome]@.

The offset is optional, except when the local time is ambiguous
(i.e. when the clocks are set forward around that time in that time zone).

The offset can also be expressed using [military time zone abbreviations](https://www.timeanddate.com/time/zones/military),
and these time zones abbreviations as per RFC 822 section 5:
\"UTC\", \"UT\", \"GMT\", \"EST\", \"EDT\", \"CST\", \"CDT\", \"MST\", \"MDT\", \"PST\", \"PDT\".

Note: the time zone's rules are loaded from the embedded database using `fromIdentifier`.
-}
instance Read TZTime where
  readsPrec _ input = do
    (components, input) <- P.readP_to_S readComponentsP input
    case getValidTZTimes components of
      Nothing -> []
      Just (tzt :| []) -> [(tzt, input)]
      Just (tzts) -> NE.toList tzts <&> \tzt -> (tzt, input)

readComponentsP :: ReadP (LocalTime, Maybe Time.TimeZone, TZIdentifier)
readComponentsP =
  (,,)
    <$> (P.readS_to_P $ reads @LocalTime)
    <*> (optional $ P.readS_to_P $ reads @Time.TimeZone)
    <*> readTZIdentP

readTZIdentP :: ReadP TZIdentifier
readTZIdentP = do
  P.skipSpaces
  void $ P.char '['
  fromString @TZIdentifier <$> P.manyTill P.get (P.char ']')

-- | Try to construct a `TZTime` from the given components.
getValidTZTimes :: MonadFail m => (LocalTime, Maybe Time.TimeZone, TZIdentifier) -> m (NonEmpty TZTime)
getValidTZTimes (lt, offsetMaybe, ident) = do
  tzi <- case fromIdentifier ident of
    Nothing -> fail $ "Unknown time zone: '" <> T.unpack (unTZIdentifier ident) <> "'"
    Just tzi -> pure tzi
  case fromLocalTime tzi lt of
    Right tzt
      | Just offset <- offsetMaybe, timeZoneMinutes offset /= timeZoneMinutes (tzTimeOffset tzt) ->
          fail $ "Invalid offset: " <> iso8601Show offset <> "\n" <> mkSuggestions (tzt :| [])
      | otherwise -> pure $ tzt :| []
    Left (TZOverlap _ tzt1 tzt2) ->
      case offsetMaybe of
        Nothing -> pure $ tzt1 :| [tzt2]
        Just offset
          | timeZoneMinutes offset == timeZoneMinutes (tzTimeOffset tzt1) -> pure $ tzt1 :| []
          | timeZoneMinutes offset == timeZoneMinutes (tzTimeOffset tzt2) -> pure $ tzt2 :| []
          | otherwise ->
              fail $ "Invalid offset: " <> iso8601Show offset <> "\n" <> mkSuggestions (tzt1 :| [tzt2])
    Left (TZGap _ tzt1 tzt2) ->
      fail $ "Invalid time: the clocks are set forward around this time.\n" <> mkSuggestions (tzt1 :| [tzt2])

mkSuggestions :: NonEmpty TZTime -> String
mkSuggestions tzts =
  "Did you mean any of the following?" <> foldMap (\tzt -> "\n  - " <> show tzt) tzts

----------------------------------------------------------------------------
-- Template Haskell
----------------------------------------------------------------------------

readP_to_Q :: String -> ReadP a -> Q a
readP_to_Q input parser =
  case P.readP_to_S (parser <* P.eof) input of
    [] -> fail $ "Failed to parse: '" <> input <> "'"
    [(res, _)] -> pure res
    _ -> fail $ "Parsing is ambiguous: '" <> input <> "'"

-- | NOTE: this assumes the time zone identifier used to construct `TZTime` exists in the
-- embedded time zone database, i.e. it can be loaded using `fromIdentifier`.
liftTZTime :: Quote m => TZTime -> Code m TZTime
liftTZTime tzt =
  [e||
    UnsafeTZTime
      $$(liftLocalTime $ tzTimeLocalTime tzt)
      (fromJust $ fromIdentifier ident)
      $$(liftTimeZone $ tzTimeOffset tzt)
  ||]
  where
    ident = tziIdentifier $ tzTimeTZInfo tzt

liftLocalTime :: Quote m => LocalTime -> Code m LocalTime
liftLocalTime (LocalTime (YearMonthDay yy mm dd) (TimeOfDay hh mmm (MkFixed ss))) =
  [e|| (LocalTime (YearMonthDay yy mm dd) (TimeOfDay hh mmm (MkFixed ss))) ||]

liftTimeZone :: Quote m => Time.TimeZone -> Code m Time.TimeZone
liftTimeZone (TimeZone tzMins tzSummer tzName) =
  [e|| TimeZone tzMins tzSummer tzName ||]

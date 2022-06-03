-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Data.Time.TZTime.Internal where

import Control.Exception.Safe (Exception(..), MonadThrow, throwM)
import Control.Monad.Except (MonadError, liftEither)
import Data.Data (Data)
import Data.Fixed (Pico)
import Data.Function ((&))
import Data.Time (UTCTime, addUTCTime, secondsToNominalDiffTime)
import Data.Time.LocalTime
import Data.Time.Zones (LocalToUTCResult(..), TZ)
import Data.Time.Zones qualified as TZ
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

----------------------------------------------------------------------------
-- TZTime
----------------------------------------------------------------------------

-- | A valid and unambiguous point in time in some time zone.
data TZTime = UnsafeTZTime
  { tztLocalTime :: LocalTime
  , tztTZ :: TZ
  , tztOffset :: TimeZone
  }
  deriving stock (Eq, Data, Generic)

instance Show TZTime where
  show = show . toZonedTime

-- | The local time of this `TZTime`.
tzTimeLocalTime :: TZTime -> LocalTime
tzTimeLocalTime = tztLocalTime

-- | The time zone of this `TZTime`.
tzTimeTZ :: TZTime -> TZ
tzTimeTZ = tztTZ

-- | The offset observed in this time zone at this moment in time.
tzTimeOffset :: TZTime -> TimeZone
tzTimeOffset = tztOffset

----------------------------------------------------------------------------
-- Constructors
----------------------------------------------------------------------------

-- | Converts a `UTCTime` to the given time zone.
fromUTC :: TZ -> UTCTime -> TZTime
fromUTC tz utct =
  UnsafeTZTime
    { tztLocalTime = TZ.utcToLocalTimeTZ tz utct
    , tztTZ = tz
    , tztOffset = TZ.timeZoneForUTCTime tz utct
    }

-- | Constructs a `TZTime` from a local time in the given time zone.
fromLocalTime :: TZ -> LocalTime -> Either TZError TZTime
fromLocalTime tz lt =
  case TZ.localTimeToUTCFull tz lt of
    LTUUnique _utc namedOffset ->
      Right $ UnsafeTZTime lt tz namedOffset
    LTUAmbiguous _utc1 _utc2 namedOffset1 namedOffset2 ->
      Left $ TZOverlap lt
        (UnsafeTZTime lt tz namedOffset1)
        (UnsafeTZTime lt tz namedOffset2)
    LTUNone utcAfter offsetBefore ->
      let
        offsetAfter = TZ.timeZoneForUTCTime tz utcAfter
        gap = secondsToNominalDiffTime $ 60 *
          fromIntegral @Int @Pico (timeZoneMinutes offsetAfter - timeZoneMinutes offsetBefore)
        utcBefore = addUTCTime (- gap) utcAfter
      in
        Left $ TZGap lt
          (UnsafeTZTime (TZ.utcToLocalTimeTZ tz utcBefore) tz offsetBefore)
          (UnsafeTZTime (TZ.utcToLocalTimeTZ tz utcAfter) tz offsetAfter)

-- | Similar to `fromLocalTime`, but throws a `TZError` in `MonadError`
-- if the local time is ambiguous/invalid.
fromLocalTimeError :: MonadError TZError m => TZ -> LocalTime -> m TZTime
fromLocalTimeError tz =
  liftEither . fromLocalTime tz

-- | Similar to `fromLocalTime`, but throws a `TZError` in `MonadThrow`
-- if the local time is ambiguous/invalid.
fromLocalTimeThrow :: MonadThrow m => TZ -> LocalTime -> m TZTime
fromLocalTimeThrow tz =
  either throwM pure . fromLocalTime tz

-- | Similar to `fromLocalTime`, but throws an `error`
-- if the local time is ambiguous/invalid.
unsafeFromLocalTime :: HasCallStack => TZ -> LocalTime -> TZTime
unsafeFromLocalTime tz lt =
  case fromLocalTimeError tz lt of
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
inTZ :: TZ -> TZTime -> TZTime
inTZ tz = fromUTC tz . toUTC

----------------------------------------------------------------------------
-- Modifying a TZTime
----------------------------------------------------------------------------

-- | Modify this moment in time along the universal time-line.
modifyUniversalTimeLine :: (UTCTime -> UTCTime) -> TZTime -> TZTime
modifyUniversalTimeLine f tzt =
  fromUTC (tzTimeTZ tzt) . f . toUTC $ tzt

-- | Modify this moment in time along the local time-line.
modifyLocalTimeLine :: (LocalTime -> LocalTime) -> TZTime -> Either TZError TZTime
modifyLocalTimeLine f tzt =
  fromLocalTime (tzTimeTZ tzt) . f . tzTimeLocalTime $ tzt

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
  deriving stock (Eq)

instance Show TZError where
  show = displayException

instance Exception TZError where
  displayException = \case
    TZGap lt _ _ ->
      "The local time " <> show lt <> " is invalid."
    TZOverlap lt tzt1 tzt2 ->
      "The local time "
      <> show lt
      <> " is ambiguous: it is observed at the offsets '"
      <> show (tzt1 & tzTimeOffset & timeZoneMinutes)
      <> "' and '"
      <> show (tzt2 & tzTimeOffset & timeZoneMinutes)
      <> "'."

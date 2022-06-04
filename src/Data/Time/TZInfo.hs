-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

module Data.Time.TZInfo
  ( TZInfo(..)
  , TZIdentifier(..)
  -- * Embeded database
  , fromIdentifier
  , fromLabel
  , TZ.TZLabel(..)
  -- * System
  , loadFromSystem
  , loadFromFile
  , getCurrentTZInfo
  ) where

import Data.Data (Data)
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Zones (TZ)
import Data.Time.Zones qualified as TZ
import Data.Time.Zones.All (TZLabel)
import Data.Time.Zones.All qualified as TZ
import GHC.Generics (Generic)
import System.Directory (getSymbolicLinkTarget)
import System.Environment (lookupEnv)
import System.FilePath (makeRelative)

data TZInfo = TZInfo
  { tziIdentifier :: TZIdentifier
  , tziRules :: TZ
  }
  deriving stock (Eq, Show, Data, Generic)

newtype TZIdentifier = TZIdentifier { unTZIdentifier :: Text }
  deriving newtype (Eq, Show, Ord, IsString)
  deriving stock (Data, Generic)

fromIdentifier :: TZIdentifier -> Maybe TZInfo
fromIdentifier ident =
  TZInfo ident <$> TZ.tzByName (T.encodeUtf8 $ unTZIdentifier ident)

fromLabel :: TZLabel -> TZInfo
fromLabel label =
  TZInfo
    (TZIdentifier (T.decodeUtf8 $ TZ.toTZName label))
    (TZ.tzByLabel label)

loadFromSystem :: TZIdentifier -> IO TZInfo
loadFromSystem ident =
  TZInfo ident <$> TZ.loadSystemTZ (T.unpack $ unTZIdentifier ident)

loadFromFile :: TZIdentifier -> FilePath -> IO TZInfo
loadFromFile ident filepath =
  TZInfo ident <$> TZ.loadTZFromFile filepath

getCurrentTZInfo :: IO TZInfo
getCurrentTZInfo =
  lookupEnv "TZ" >>= \case
    Nothing -> do
      filePath <- getSymbolicLinkTarget "/etc/localtime"
      let ident = fromString @TZIdentifier $ makeRelative "/usr/share/zoneinfo" filePath
      tz <- TZ.loadTZFromFile filePath
      pure $ TZInfo ident tz
    Just "" -> loadFromSystem "UTC"
    Just ident -> TZInfo (fromString ident) <$> TZ.loadSystemTZ ident

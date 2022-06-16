-- | Provides utilities.
--
-- @since 0.1
module Data.Time.Conversion.Utils
  ( timeLocaleAllZones,
    tzLabelToTimeZone,
    catchSync,
  )
where

import Control.Exception
  ( Exception (..),
    SomeAsyncException (..),
    catch,
    throwIO,
  )
import Data.Time.Format (TimeLocale (..))
import Data.Time.Format qualified as Format
import Data.Time.LocalTime (TimeZone)
import Data.Time.Zones qualified as Zones
import Data.Time.Zones.All (TZLabel (..))
import Data.Time.Zones.All qualified as All

-- | 'Format.defaultTimeLocale' with the date format switched to @%d\/%m\/%y@
-- and knowledge of __all__ timezones, per 'TZLabel'. Using this, we can parse
-- non-American labels like @CES@ and @NZST@.
--
-- @since 0.1
timeLocaleAllZones :: TimeLocale
timeLocaleAllZones =
  Format.defaultTimeLocale
    { dateFmt = "%d/%m/%y",
      knownTimeZones = allTimeZones
    }

allTimeZones :: [TimeZone]
allTimeZones = tzLabelToTimeZone <$> [minBound .. maxBound]

-- | Converts the 'TZLabel' into a 'TimeZone'.
--
-- @since 0.1
tzLabelToTimeZone :: TZLabel -> TimeZone
tzLabelToTimeZone = (`Zones.timeZoneForPOSIX` 0) . All.tzByLabel

-- | Catches synchronous exceptions.
--
-- @since 0.1
catchSync :: Exception e => IO a -> (e -> IO a) -> IO a
catchSync io handler =
  io `catch` \ex ->
    case fromException (toException ex) of
      Just (SomeAsyncException _) -> throwIO ex
      Nothing -> handler ex

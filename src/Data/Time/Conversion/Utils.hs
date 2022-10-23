-- | Provides utilities.
--
-- @since 0.1
module Data.Time.Conversion.Utils
  ( timeLocaleAllZones,
    Internal.tzLabelToTimeZone,
    Internal.tzNameToTZLabel,
  )
where

import Data.Time.Conversion.Internal qualified as Internal
import Data.Time.Format (TimeLocale (..))
import Data.Time.Format qualified as Format
import Data.Time.LocalTime (TimeZone)

-- | 'Format.defaultTimeLocale' with the date format switched to @%d\/%m\/%y@
-- and knowledge of __all__ timezones, per 'TZLabel'. Using this, we can parse
-- non-American labels like @CES@ and @NZST@.
--
-- FIXME: This does _not_ include daylight savings time e.g. EDT, NZDT.
-- That is, if this is used to parse a time zone like EDT then it will fail.
--
-- @since 0.1
timeLocaleAllZones :: TimeLocale
timeLocaleAllZones =
  Format.defaultTimeLocale
    { dateFmt = "%d/%m/%y",
      knownTimeZones = allTimeZones
    }

allTimeZones :: [TimeZone]
allTimeZones = Internal.tzLabelToTimeZone <$> [minBound .. maxBound]

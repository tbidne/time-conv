-- | Provides utilities.
--
-- @since 0.1
module Data.Time.Conversion.Utils
  ( timeLocaleAllLabels,
    Internal.tzLabelToTimeZone,
    Internal.tzNameToTZLabel,
  )
where

import Data.Time.Conversion.Internal qualified as Internal
import Data.Time.Format (TimeLocale (..))
import Data.Time.Format qualified as Format
import Data.Time.LocalTime (TimeZone)

-- | 'Format.defaultTimeLocale' with the date format switched to @%d\/%m\/%y@
-- and knowledge of __all__ timezones, per 'Data.Time.Zones.All.TZLabel'.
-- Using this, we can parse non-American labels like @CES@ and @NZST@.
--
-- NB: This does not include zones based on daylight savings e.g. EDT.
--
-- @since 0.1
timeLocaleAllLabels :: TimeLocale
timeLocaleAllLabels =
  Format.defaultTimeLocale
    { dateFmt = "%d/%m/%y",
      knownTimeZones = allTimeZones
    }

allTimeZones :: [TimeZone]
allTimeZones = Internal.tzLabelToTimeZone <$> [minBound .. maxBound]

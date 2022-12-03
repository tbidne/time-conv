-- | Internal module. This module does not necessarily follow PVP.
--
-- @since 0.1
module Data.Time.Conversion.Internal
  ( -- * TZ Database Labels
    tzNameToTZLabel,
    tzLabelToTimeZone,
    tzLabelToTimeZoneAbbrv,
    tzLowerNameLabelMap,
    tzLowerNameLabelMapWith,

    -- * Miscellaneous
    catchSync,
    catchAny,
  )
where

import Control.Exception
  ( Exception (..),
    SomeAsyncException (..),
    SomeException,
    catch,
    throwIO,
  )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Encoding.Error (OnDecodeError)
import Data.Text.Encoding.Error qualified as TError
import Data.Time.LocalTime (TimeZone)
import Data.Time.Zones qualified as Zones
import Data.Time.Zones.All (TZLabel (..))
import Data.Time.Zones.All qualified as All

-- $setup
-- >>> import Data.Time.Zones.All (TZLabel (..))

-- | Catches synchronous exceptions.
--
-- @since 0.1
catchSync :: Exception e => IO a -> (e -> IO a) -> IO a
catchSync io handler =
  io `catch` \ex ->
    case fromException (toException ex) of
      Just (SomeAsyncException _) -> throwIO ex
      Nothing -> handler ex

-- | 'catchSync' specialized to 'SomeException'.
--
-- @since 0.1
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catchSync

-- | Converts the 'TZLabel' into a 'TimeZone'.
--
-- @since 0.1
tzLabelToTimeZone :: TZLabel -> TimeZone
tzLabelToTimeZone = (`Zones.timeZoneForPOSIX` 0) . All.tzByLabel

-- | Transforms a tz label into its given time zone abbreviation.
--
-- ==== __Examples__
--
-- >>> tzLabelToTimeZoneAbbrv America__New_York
-- "EST"
--
-- >>> tzLabelToTimeZoneAbbrv Etc__UTC
-- "UTC"
--
-- @since 0.1
tzLabelToTimeZoneAbbrv :: TZLabel -> Text
tzLabelToTimeZoneAbbrv = T.pack . show . tzLabelToTimeZone

-- NOTE: show instead of Local.timeZoneName since the former reduces to the
-- latter when the timeZoneName is non-empty, but when it is empty includes
-- an offset, so it is strictly more flexible.

-- | Looks up a tz database label by name. Case-insensitive.
--
-- @since 0.1
tzNameToTZLabel :: Text -> Maybe TZLabel
tzNameToTZLabel = (`Map.lookup` tzLowerNameLabelMap) . T.toLower

-- | Like @tz@'s @tzNameLabelMap@ but with lower-case 'Text' keys instead of
-- ByteString for case-insensitive lookup.
--
-- @since 0.1
tzLowerNameLabelMap :: Map Text TZLabel
tzLowerNameLabelMap = tzLowerNameLabelMapWith TError.lenientDecode

-- | tzLowerNameLabelMapOnError with custom decoder, since we need to convert
-- ByteString -> Text.
--
-- @since 0.1
tzLowerNameLabelMapWith :: OnDecodeError -> Map Text TZLabel
tzLowerNameLabelMapWith decoder = Map.mapKeys toLower All.tzNameLabelMap
  where
    toLower = T.toLower . TEnc.decodeUtf8With decoder

-- | This module provides functions for reading time strings. We also provide
-- functions for converting between time zones.
--
-- @since 0.1
module Data.Time.Conversion
  ( -- * High-level parsing/conversion
    readConvertTime,

    -- ** Types
    TimeBuilder (..),
    SrcTZ (..),
    Types._SrcTZConv,
    Types._SrcTZLiteral,
    TZConv (..),
    Types._TZConvLocal,
    Types._TZConvDatabase,
    TZDatabase (..),
    Types._TZDatabaseLabel,
    Types._TZDatabaseText,
    TimeFormat (..),
    Types._TimeFormatManual,
    Types._TimeFormatFull,
    Types.timeFormatStringIso,

    -- ** Formatting
    Types.hm,
    Types.hm12h,
    Types.hmTZ,
    Types.hmTZ12h,

    -- * Low-level functions

    -- ** Parsing time strings
    readInLocalTimeZone,
    readTimeFormat,

    -- ** Converting ZonedTime
    convertZoned,
    convertZonedLabel,

    -- * Errors
    ParseTimeException (..),
    ParseTZDatabaseException (..),
    LocalTimeZoneException (..),
    LocalSystemTimeException (..),

    -- * Miscellaneous
    Utils.timeLocaleAllZones,

    -- * Reexports
    ZonedTime (..),
    TZLabel (..),
    TimeLocale (..),
  )
where

import Control.Exception (SomeException (..), throwIO)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Time.Conversion.Types
  ( LocalSystemTimeException (..),
    LocalTimeZoneException (..),
    ParseTZDatabaseException (..),
    ParseTimeException (..),
    SrcTZ (..),
    TZConv (..),
    TZDatabase (..),
    TimeBuilder (..),
    TimeFormat (..),
  )
import Data.Time.Conversion.Types qualified as Types
import Data.Time.Conversion.Utils qualified as Utils
import Data.Time.Format (TimeLocale (..))
import Data.Time.Format qualified as Format
import Data.Time.LocalTime (ZonedTime)
import Data.Time.LocalTime qualified as Local
import Data.Time.Zones qualified as Zones
import Data.Time.Zones.All (TZLabel (..))
import Data.Time.Zones.All qualified as All
import Optics.Core ((%), (^.), (^?))

-- | Reads the given time string based on the 'TimeBuilder'.
-- The semantics are:
--
-- * If 'srcTZ' is @'SrcTZConv' 'TZConvDatabase'@ then we manually append the
--   timezone data onto the time string and format. That is, only use this if:
--
--      * @timeString@ does __not__ contain a timezone like @EST@.
--      * @format@ does __not__ mention the timezone option @%Z@.
--
-- * If 'timeString' is 'Nothing' then we retrieve the local system time.
--
-- __Throws:__
--
-- * 'ParseTimeException': Error parsing the time string.
-- * 'ParseTZDatabaseException': Error parsing the tz database name.
-- * 'LocalTimeZoneException': Error retrieving local timezone.
-- * 'LocalSystemTimeException': Error retrieving local system time.
--
-- ==== __Examples__
-- >>> import Data.Default (Default (def))
-- >>> let toUtcBuilder = def { destTZ = TZConvDatabase (TZDatabaseLabel Etc__UTC) }
-- >>> let litToUtcBuilder = toUtcBuilder { srcTZ = SrcTZLiteral }
-- >>> -- literal + no src time zone = utc
-- >>> readConvertTime (litToUtcBuilder { timeString = Just "17:24" })
-- 1970-01-01 17:24:00 UTC
--
-- >>> -- literal + convert from est
-- >>> readConvertTime (litToUtcBuilder { format = Types.hmTZ, timeString = Just "17:24 EST" })
-- 1970-01-01 22:24:00 UTC
--
-- >>> let nyToUtcBuilder = toUtcBuilder { srcTZ = SrcTZConv (TZConvDatabase $ TZDatabaseLabel America__New_York) }
-- >>> readConvertTime (nyToUtcBuilder { timeString = Just "08:15" })
-- 1970-01-01 13:15:00 UTC
--
-- @since 0.1
readConvertTime :: TimeBuilder -> IO ZonedTime
readConvertTime builder = do
  inTime <- case builder ^. #timeString of
    Nothing ->
      Local.getZonedTime
        `Utils.catchSync` \(e :: SomeException) -> throwIO $ MkLocalSystemTimeException e
    Just timeStr -> readTimeString builder timeStr

  case builder ^. #destTZ of
    TZConvDatabase tzdb -> do
      lbl <- tzDatabaseToTZLabel tzdb
      pure $ convertZonedLabel inTime lbl
    TZConvLocal -> do
      let inTimeUtc = Local.zonedTimeToUTC inTime
      currTZ <- Local.getCurrentTimeZone
      pure $ Local.utcToZonedTime currTZ inTimeUtc

readTimeString :: TimeBuilder -> Text -> IO ZonedTime
readTimeString builder timeStr = do
  case builder ^. #srcTZ of
    -- read in local time zone
    SrcTZConv TZConvLocal -> do
      -- add system date if specified
      (timeStrDate, formatDate) <- maybeAddDate Nothing
      readInLocalTimeZone locale formatDate timeStrDate
    -- remaining two cases work similarly, though we need to account for a
    -- manually specified timezone
    other -> do
      -- add timezone if specified
      (timeStrDateTZ, formatDateTZ) <- case hasTzdb other of
        -- This is a SrcTZLiteral, leave timeStr and format as-is
        Nothing -> do
          -- add system date if specified
          (timeStrDate, formatDate) <- maybeAddDate Nothing
          pure (timeStrDate, formatDate)
        -- We have a timezone to read in, modify the timeStr and format
        Just tzdb -> do
          lbl <- tzDatabaseToTZLabel tzdb
          -- add src date if specified
          (timeStrDate, formatDate) <- maybeAddDate (Just lbl)

          let name = tzLabelToTimeZoneName lbl
          pure (timeStrDate +-+ name, formatDate +-+ tzString)

      maybe
        (throwParseEx formatDateTZ timeStrDateTZ)
        pure
        (readTimeFormat locale formatDateTZ timeStrDateTZ)
  where
    format = builder ^. #format
    locale = builder ^. #locale
    hasTzdb :: SrcTZ -> Maybe TZDatabase
    hasTzdb x = x ^? Types._SrcTZConv % Types._TZConvDatabase

    throwParseEx f = throwIO . MkParseTimeException f

    maybeAddDate mlabel = do
      if builder ^. #today
        then do
          currDateStr <- currentDate locale mlabel
          pure (T.pack currDateStr +-+ timeStr, dateString +-+ format)
        else pure (timeStr, format)

currentDate :: TimeLocale -> Maybe TZLabel -> IO String
currentDate locale mlabel = do
  currTime <-
    Local.getZonedTime
      `Utils.catchSync` \(e :: SomeException) -> throwIO $ MkLocalSystemTimeException e

  -- Convert into the given label if present. Otherwise keep in system
  -- timezone.
  let currTime' = maybe currTime (convertZonedLabel currTime) mlabel

  pure $ Format.formatTime locale dateString currTime'

dateString :: IsString s => s
dateString = "%Y-%m-%d"

tzString :: IsString s => s
tzString = "%Z"

-- | @readInLocalTimeZone locale format timeStr@ attempts to parse the
-- @timeStr@ given the expected @format@. We parse into the current
-- system time zone, so:
--
-- * @format@ should __not__ mention "%Z"
-- * @timeStr@ should __not__ contain time zone information.
--
-- @
-- λ. readInLocalTimeZone Types.timeLocaleAllZones "%H" "17"
-- Just 1970-01-01 17:00:00 NZST
-- @
--
-- __Throws:__
--
-- * 'ParseTimeException': Error parsing the time string.
-- * 'LocalTimeZoneException': Error retrieving local timezone.
--
-- @since 0.1
readInLocalTimeZone :: TimeLocale -> TimeFormat -> Text -> IO ZonedTime
readInLocalTimeZone locale format timeStr = do
  tzStr <-
    T.pack
      . show
      <$> Local.getCurrentTimeZone
      `Utils.catchSync` (\(e :: SomeException) -> throwIO $ MkLocalTimeZoneException e)
  let timeStr' = timeStr +-+ tzStr
  case readTimeFormat locale format' timeStr' of
    Just zt -> pure zt
    Nothing -> throwIO $ MkParseTimeException format' timeStr'
  where
    format' = format +-+ tzString

-- | @readTimeFormat locale format timeStr@ attempts to parse the @timeStr@ given
-- the expected @format@. No timezone is assumed, so if it is left off then
-- the result is UTC.
--
-- ==== __Examples__
-- >>> readTimeFormat Utils.timeLocaleAllZones Types.hm "17:24"
-- Just 1970-01-01 17:24:00 +0000
--
-- >>> readTimeFormat Utils.timeLocaleAllZones Types.hm12h "07:24 pm"
-- Just 1970-01-01 19:24:00 +0000
--
-- >>> readTimeFormat Utils.timeLocaleAllZones Types.hmTZ "07:24 CET"
-- Just 1970-01-01 07:24:00 CET
--
-- >>> readTimeFormat Utils.timeLocaleAllZones Types.hmTZ12h "07:24 pm EST"
-- Just 1970-01-01 19:24:00 EST
--
-- @since 0.1
readTimeFormat :: TimeLocale -> TimeFormat -> Text -> Maybe ZonedTime
readTimeFormat locale format timeStr = Format.parseTimeM True locale format' timeStr'
  where
    format' = format ^. Types.timeFormatStringIso
    timeStr' = T.unpack timeStr

-- | Converts a zoned time to the given timezone.
--
-- ==== __Examples__
-- >>> let (Just sixPmUtc) = readTimeFormat Utils.timeLocaleAllZones Types.hm "18:00"
-- >>> convertZoned sixPmUtc "America/New_York"
-- Just 1970-01-01 13:00:00 EST
--
-- >>> convertZoned sixPmUtc "bad tz label"
-- Nothing
--
-- @since 0.1
convertZoned :: ZonedTime -> Text -> Maybe ZonedTime
convertZoned zt = fmap (convertZonedLabel zt) . txtToTZLabel

-- | Converts a zoned time to the given timezone.
--
-- ==== __Examples__
-- >>> let (Just sixPmUtc) = readTimeFormat Utils.timeLocaleAllZones Types.hm "18:00"
-- >>> convertZonedLabel sixPmUtc America__New_York
-- 1970-01-01 13:00:00 EST
--
-- @since 0.1
convertZonedLabel :: ZonedTime -> TZLabel -> ZonedTime
convertZonedLabel zt tzLabel =
  let tz = All.tzByLabel tzLabel
      utc = Local.zonedTimeToUTC zt
      timeZone = Zones.timeZoneForUTCTime tz utc
   in Local.utcToZonedTime timeZone utc

tzDatabaseToTZLabel :: TZDatabase -> IO TZLabel
tzDatabaseToTZLabel (TZDatabaseLabel lbl) = pure lbl
tzDatabaseToTZLabel (TZDatabaseText txt) =
  case txtToTZLabel txt of
    Just lbl -> pure lbl
    Nothing -> throwIO $ MkParseTZDatabaseException txt

tzLabelToTimeZoneName :: TZLabel -> Text
tzLabelToTimeZoneName = T.pack . Local.timeZoneName . Utils.tzLabelToTimeZone

txtToTZLabel :: Text -> Maybe TZLabel
txtToTZLabel = All.fromTZName . TEnc.encodeUtf8

-- concat with a space
(+-+) :: (Semigroup a, IsString a) => a -> a -> a
xs +-+ ys = xs <> " " <> ys

infixr 5 +-+

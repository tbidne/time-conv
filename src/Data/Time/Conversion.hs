-- | This module provides functions for reading time strings. We also provide
-- functions for converting between timezones.
--
-- @since 0.1
module Data.Time.Conversion
  ( -- * High-level parsing/conversion
    readConvertTime,
    readTime,
    convertTime,

    -- ** Types
    TimeReader (..),
    defaultTimeReader,
    TZDatabase (..),
    _TZDatabaseLabel,
    _TZDatabaseText,
    TimeFormat (..),

    -- ** Formatting
    Types.hm,
    Types.hm12h,
    Types.hmTZ,
    Types.hmTZ12h,
    Types.rfc822,

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

    -- * Reexports
    ZonedTime (..),
    TZLabel (..),
    TimeLocale (..),
  )
where

import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion.Internal qualified as Internal
import Data.Time.Conversion.Types
  ( LocalSystemTimeException (..),
    LocalTimeZoneException (..),
    ParseTZDatabaseException (..),
    ParseTimeException (..),
    TZDatabase (..),
    TimeFormat (..),
    TimeReader (..),
    defaultTimeReader,
    _TZDatabaseLabel,
    _TZDatabaseText,
  )
import Data.Time.Conversion.Types qualified as Types
import Data.Time.Format (TimeLocale (..))
import Data.Time.Format qualified as Format
import Data.Time.LocalTime (ZonedTime)
import Data.Time.LocalTime qualified as Local
import Data.Time.Zones qualified as Zones
import Data.Time.Zones.All (TZLabel (..))
import Data.Time.Zones.All qualified as All
import Effects.Exception
  ( HasCallStack,
    catchAny,
    throwCS,
  )
import Optics.Core ((^.))

-- $setup
-- >>> import Control.Exception (SomeException)
-- >>> import Data.Functor (void)
-- >>> import Data.Time.Conversion.Types qualified as Types
-- >>> import Effects.Exception (catchCS)
-- >>> let parseTimeEx = \(e :: ParseTimeException) -> putStrLn "parse time exception"
-- >>> let parseTzDbEx = \(e :: ParseTZDatabaseException) -> putStrLn "parse tzdb exception"

-- | Reads the given time string based on the source 'TimeReader' and
-- converts to the destination timezone. This is the composition of
-- 'readTime' and 'convertTime'. If the source is 'Nothing' then we read
-- the local system time. Similarly, if the dest is 'Nothing', we convert
-- to the local system timezone.
--
-- __Throws:__
--
-- * 'ParseTimeException': Error parsing the time string.
-- * 'ParseTZDatabaseException': Error parsing the tz database name.
-- * 'LocalTimeZoneException': Error retrieving local timezone.
-- * 'LocalSystemTimeException': Error retrieving local system time.
--
-- ==== __Examples__
--
-- >>> let toUtc = Just $ TZDatabaseLabel Etc__UTC
-- >>> let reader = defaultTimeReader "17:24"
--
-- >>> -- convert from est
-- >>> let nyReader = reader { srcTZ = Just (TZDatabaseLabel America__New_York) }
-- >>> readConvertTime (Just (nyReader { timeString = "08:15" })) toUtc
-- 1970-01-01 13:15:00 UTC
--
-- >>> let badTimeString = reader { timeString = "bad" }
-- >>> (void $ readConvertTime (Just badTimeString) toUtc) `catchCS` parseTimeEx
-- parse time exception
--
-- >>> let badTZDatabase = reader { srcTZ = Just (TZDatabaseText "America/NewYork")}
-- >>> (void $ readConvertTime (Just badTZDatabase) toUtc) `catchCS` parseTzDbEx
-- parse tzdb exception
--
-- @since 0.1
readConvertTime ::
  (HasCallStack) =>
  -- | Source time.
  Maybe TimeReader ->
  -- | Dest timezone.
  Maybe TZDatabase ->
  -- | Converted time.
  IO ZonedTime
readConvertTime mtimeReader destTZ =
  readTime mtimeReader >>= (`convertTime` destTZ)

-- | Reads a time based on the 'TimeReader'. If given 'Nothing' we read the
-- local system time instead.
--
-- __Throws:__
--
-- * 'ParseTimeException': Error parsing the time string.
-- * 'ParseTZDatabaseException': Error parsing the tz database name.
-- * 'LocalTimeZoneException': Error retrieving local timezone.
-- * 'LocalSystemTimeException': Error retrieving local system time.
--
-- ==== __Examples__
-- >>> let reader = defaultTimeReader "17:24"
--
-- >>> -- convert from est
-- >>> readTime (Just (reader { srcTZ = Just (TZDatabaseLabel America__New_York) }))
-- 1970-01-01 17:24:00 -0500
--
-- >>> let nyReader = reader { srcTZ = Just (TZDatabaseLabel America__New_York) }
-- >>> readTime (Just (nyReader { timeString = "08:15" }))
-- 1970-01-01 08:15:00 -0500
--
-- >>> let badTimeString = reader { timeString = "bad" }
-- >>> (void $ readTime (Just badTimeString)) `catchCS` parseTimeEx
-- parse time exception
--
-- >>> let badTZDatabase = reader { srcTZ = Just (TZDatabaseText "America/NewYork")}
-- >>> (void $ readTime (Just badTZDatabase)) `catchCS` parseTzDbEx
-- parse tzdb exception
--
-- @since 0.1
readTime :: (HasCallStack) => Maybe TimeReader -> IO ZonedTime
readTime (Just timeReader) = readTimeString timeReader
readTime Nothing =
  Local.getZonedTime `catchAny` (throwCS . MkLocalSystemTimeException)

-- | Converts the given time to the destination timezone. If no destination
-- timezone is given then we convert to the local system timezone.
--
-- __Throws:__
--
-- * 'ParseTZDatabaseException': Error parsing the tz database name.
-- * 'LocalTimeZoneException': Error retrieving local timezone.
--
-- ==== __Examples__
--
-- >>> import Data.Time.Clock (UTCTime (..))
-- >>> import Data.Time.LocalTime (utc, utcToZonedTime)
-- >>> let zoned = utcToZonedTime utc (UTCTime (toEnum 50_000) 0)
-- >>> zoned
-- 1995-10-10 00:00:00 UTC
--
-- >>> let toNy = Just $ TZDatabaseText "America/New_York"
-- >>> convertTime zoned toNy
-- 1995-10-09 20:00:00 EDT
--
-- >>> (void $ convertTime zoned (Just $ TZDatabaseText "America/NewYork")) `catchCS` parseTzDbEx
-- parse tzdb exception
--
-- @since 0.1
convertTime :: (HasCallStack) => ZonedTime -> Maybe TZDatabase -> IO ZonedTime
convertTime inTime Nothing = do
  let inTimeUtc = Local.zonedTimeToUTC inTime
  currTZ <-
    Local.getCurrentTimeZone
      `catchAny` (throwCS . MkLocalTimeZoneException)
  pure $ Local.utcToZonedTime currTZ inTimeUtc
convertTime inTime (Just tzdb) = convertZonedLabel inTime <$> tzDatabaseToTZLabel tzdb

readTimeString :: (HasCallStack) => TimeReader -> IO ZonedTime
readTimeString timeReader = do
  case timeReader ^. #srcTZ of
    -- read in local timezone
    Nothing -> do
      -- add system date if specified
      (timeStrDate, formatDate) <- maybeAddDate Nothing
      readInLocalTimeZone formatDate timeStrDate
    Just tzdb -> do
      lbl <- tzDatabaseToTZLabel tzdb
      -- add src date if specified
      (timeStrDate, formatDate) <- maybeAddDate (Just lbl)
      let name = Internal.tzLabelToTimeZoneAbbrv lbl
          timeStrDateTZ = timeStrDate +-+ name
          formatDateTZ = formatDate +-+ tzString

      maybe
        (throwParseEx formatDateTZ timeStrDateTZ)
        pure
        (readTimeFormat Format.defaultTimeLocale formatDateTZ timeStrDateTZ)
  where
    format = timeReader ^. #format
    timeStr = timeReader ^. #timeString

    throwParseEx :: (HasCallStack) => TimeFormat -> Text -> IO void
    throwParseEx f = throwCS . MkParseTimeException f

    maybeAddDate :: (HasCallStack) => Maybe TZLabel -> IO (Text, TimeFormat)
    maybeAddDate mlabel = do
      if timeReader ^. #today
        then do
          currDateStr <- currentDate mlabel
          pure (T.pack currDateStr +-+ timeStr, dateString +-+ format)
        else pure (timeStr, format)

currentDate :: (HasCallStack) => Maybe TZLabel -> IO String
currentDate mlabel = do
  currTime <-
    Local.getZonedTime
      `catchAny` (throwCS . MkLocalSystemTimeException)

  -- Convert into the given label if present. Otherwise keep in system
  -- timezone.
  let currTime' = maybe currTime (convertZonedLabel currTime) mlabel

  pure $ Format.formatTime Format.defaultTimeLocale dateString currTime'

dateString :: (IsString s) => s
dateString = "%Y-%m-%d"

tzString :: (IsString s) => s
tzString = "%z"

-- | @readInLocalTimeZone locale format timeStr@ attempts to parse the
-- @timeStr@ given the expected @format@. We parse into the current
-- system timezone, so:
--
-- * @format@ should __not__ mention "%Z"
-- * @timeStr@ should __not__ contain timezone information.
--
-- @
-- λ. readInLocalTimeZone "%H" "17"
-- Just 1970-01-01 17:00:00 NZST
-- @
--
-- __Throws:__
--
-- * 'ParseTimeException': Error parsing the time string.
-- * 'LocalTimeZoneException': Error retrieving local timezone.
--
-- @since 0.1
readInLocalTimeZone :: (HasCallStack) => TimeFormat -> Text -> IO ZonedTime
readInLocalTimeZone format timeStr = do
  localTz <-
    Local.getCurrentTimeZone
      `catchAny` (throwCS . MkLocalTimeZoneException)
  let tzStr = T.pack $ Local.timeZoneOffsetString localTz

      -- Add the local tz string to the time string, and the tz flag to the format
      timeStr' = timeStr +-+ tzStr
  case readTimeFormat Format.defaultTimeLocale format' timeStr' of
    Just zt -> pure zt
    Nothing -> throwCS $ MkParseTimeException format' timeStr'
  where
    format' = format +-+ tzString

-- | @readTimeFormat locale format timeStr@ attempts to parse the @timeStr@ given
-- the expected @format@. No timezone is assumed, so if it is left off then
-- the result is UTC.
--
-- ==== __Examples__
-- >>> readTimeFormat Format.defaultTimeLocale Types.hm "17:24"
-- Just 1970-01-01 17:24:00 +0000
--
-- >>> readTimeFormat Format.defaultTimeLocale Types.hm12h "07:24 pm"
-- Just 1970-01-01 19:24:00 +0000
--
-- >>> readTimeFormat Format.defaultTimeLocale Types.hmTZ "07:24 +5000"
-- Just 1970-01-01 07:24:00 +5000
--
-- >>> readTimeFormat Format.defaultTimeLocale Types.hmTZ12h "07:24 pm -5000"
-- Just 1970-01-01 19:24:00 -5000
--
-- @since 0.1
readTimeFormat :: TimeLocale -> TimeFormat -> Text -> Maybe ZonedTime
readTimeFormat locale format timeStr = Format.parseTimeM True locale format' timeStr'
  where
    format' = T.unpack $ format ^. #unTimeFormat
    timeStr' = T.unpack timeStr

-- | Converts a zoned time to the given timezone.
--
-- ==== __Examples__
-- >>> let (Just sixPmUtc) = readTimeFormat Format.defaultTimeLocale Types.hm "18:00"
-- >>> convertZoned sixPmUtc "America/New_York"
-- Just 1970-01-01 13:00:00 EST
--
-- >>> convertZoned sixPmUtc "bad tz label"
-- Nothing
--
-- @since 0.1
convertZoned :: ZonedTime -> Text -> Maybe ZonedTime
convertZoned zt = fmap (convertZonedLabel zt) . Internal.tzNameToTZLabel

-- | Converts a zoned time to the given timezone.
--
-- ==== __Examples__
-- >>> let (Just sixPmUtc) = readTimeFormat Format.defaultTimeLocale Types.hm "18:00"
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

tzDatabaseToTZLabel :: (HasCallStack) => TZDatabase -> IO TZLabel
tzDatabaseToTZLabel (TZDatabaseLabel lbl) = pure lbl
tzDatabaseToTZLabel (TZDatabaseText txt) =
  case Internal.tzNameToTZLabel txt of
    Just lbl -> pure lbl
    Nothing -> throwCS $ MkParseTZDatabaseException txt

-- concat with a space
(+-+) :: (Semigroup a, IsString a) => a -> a -> a
xs +-+ ys = xs <> " " <> ys

infixr 5 +-+

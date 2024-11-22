{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This module provides functions for reading time strings. We also provide
-- functions for converting between timezones.
--
-- @since 0.1
module Data.Time.Conversion
  ( -- * High-level parsing/conversion
    readConvertTime,
    readTime,
    convertTime,

    -- * Low-level functions

    -- ** Parsing time strings
    readInLocalTimeZone,
    readTimeFormatLocal,
    readTimeFormatZoned,
    readTimeFormat,

    -- ** Converting ZonedTime
    convertZonedLabel,

    -- * Types
    Date (..),
    TimeFormat (..),
    TimeReader (..),
    TZDatabase (..),

    -- ** Re-exports
    TZLabel (..),
    ZonedTime (..),

    -- ** Exceptions
    ParseTimeException (..),
    ParseTZDatabaseException (..),
    LocalTimeZoneException (..),
    LocalSystemTimeException (..),
  )
where

import Control.Exception.Utils (catchSync)
import Control.Monad.Catch
  ( MonadThrow,
    throwM,
  )
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Time.Conversion.Internal qualified as Internal
import Data.Time.Conversion.Types.Date
  ( Date (DateLiteral, DateToday),
    unDateString,
  )
import Data.Time.Conversion.Types.Exception
  ( LocalSystemTimeException (MkLocalSystemTimeException),
    LocalTimeZoneException (MkLocalTimeZoneException),
    ParseTZDatabaseException (MkParseTZDatabaseException),
    ParseTimeException (MkParseTimeException),
  )
import Data.Time.Conversion.Types.TZDatabase
  ( TZDatabase (TZDatabaseLabel, TZDatabaseText),
  )
import Data.Time.Conversion.Types.TimeFormat
  ( TimeFormat (MkTimeFormat, unTimeFormat),
  )
import Data.Time.Conversion.Types.TimeReader
  ( TimeReader
      ( MkTimeReader,
        date,
        format,
        srcTZ,
        timeString
      ),
  )
import Data.Time.Format (ParseTime, TimeLocale)
import Data.Time.Format qualified as Format
import Data.Time.LocalTime
  ( LocalTime,
    TimeZone,
    ZonedTime
      ( ZonedTime,
        zonedTimeToLocalTime,
        zonedTimeZone
      ),
  )
import Data.Time.LocalTime qualified as Local
import Data.Time.Zones (TZ)
import Data.Time.Zones qualified as Zones
import Data.Time.Zones.All (TZLabel (..))
import Data.Time.Zones.All qualified as All
import Effectful (Eff, (:>))
import Effectful.Time.Dynamic (Time, getSystemZonedTime)
import GHC.Stack (HasCallStack)
import Optics.Core ((^.))

-- $setup
-- >>> import Control.Exception (SomeException)
-- >>> import Data.Functor (void)
-- >>> import Data.Time.Conversion.Types.TimeFormat qualified as TimeFmt
-- >>> import Data.Time.Conversion.Types.TimeReader (defaultTimeReader)
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
  (HasCallStack, Time :> es) =>
  -- | Source time.
  Maybe TimeReader ->
  -- | Dest timezone.
  Maybe TZDatabase ->
  -- | Converted time.
  Eff es ZonedTime
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
-- 1970-01-01 17:24:00 EST
--
-- >>> let nyReader = reader { srcTZ = Just (TZDatabaseLabel America__New_York) }
-- >>> readTime (Just (nyReader { timeString = "08:15" }))
-- 1970-01-01 08:15:00 EST
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
readTime ::
  ( HasCallStack,
    Time :> es
  ) =>
  Maybe TimeReader ->
  Eff es ZonedTime
readTime (Just timeReader) = readTimeString timeReader
readTime Nothing =
  getSystemZonedTime `catchSync` (throwM . MkLocalSystemTimeException)

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
convertTime ::
  ( HasCallStack,
    Time :> es
  ) =>
  ZonedTime ->
  Maybe TZDatabase ->
  Eff es ZonedTime
convertTime inTime Nothing = do
  let inTimeUtc = Local.zonedTimeToUTC inTime
  currTZ <-
    getCurrentTimeZone
      `catchSync` (throwM . MkLocalTimeZoneException)
  pure $ Local.utcToZonedTime currTZ inTimeUtc
convertTime inTime (Just tzdb) = convertZonedLabel inTime <$> tzDatabaseToTZLabel tzdb

readTimeString ::
  forall es.
  ( HasCallStack,
    Time :> es
  ) =>
  TimeReader ->
  Eff es ZonedTime
readTimeString timeReader =
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

      -- Read string as a LocalTime, no TZ info. This allow us to correctly
      -- get the source's timezone, taking the desired date into account.
      localTime <-
        maybe
          (throwParseEx formatDate timeStrDate)
          pure
          (readTimeFormatLocal Format.defaultTimeLocale formatDate timeStrDate)

      pure $ convertLocalLabel localTime lbl
  where
    format = timeReader ^. #format
    timeStr = timeReader ^. #timeString

    throwParseEx :: (HasCallStack, MonadThrow m) => TimeFormat -> Text -> m void
    throwParseEx f = throwM . MkParseTimeException f

    maybeAddDate ::
      ( HasCallStack
      ) =>
      -- Maybe source timezone
      Maybe TZLabel ->
      Eff es (Text, TimeFormat)
    maybeAddDate mlabel = case timeReader ^. #date of
      Nothing -> pure (timeStr, format)
      Just (DateLiteral dateStr) -> do
        let str = unDateString dateStr
        pure (str +-+ timeStr, dateString +-+ format)
      Just DateToday -> do
        -- get the current date in the source timezone
        currDateStr <- currentDate mlabel
        pure (T.pack currDateStr +-+ timeStr, dateString +-+ format)

currentDate ::
  ( HasCallStack,
    Time :> es
  ) =>
  Maybe TZLabel ->
  Eff es String
currentDate mlabel = do
  currTime <-
    getSystemZonedTime
      `catchSync` (throwM . MkLocalSystemTimeException)

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
readInLocalTimeZone ::
  ( HasCallStack,
    Time :> es
  ) =>
  TimeFormat ->
  Text ->
  Eff es ZonedTime
readInLocalTimeZone format timeStr = do
  localTz <-
    getCurrentTimeZone
      `catchSync` (throwM . MkLocalTimeZoneException)
  let tzStr = T.pack $ Local.timeZoneOffsetString localTz

      -- Add the local tz string to the time string, and the tz flag to the format
      timeStr' = timeStr +-+ tzStr
  case readTimeFormatZoned Format.defaultTimeLocale format' timeStr' of
    Just zt -> pure zt
    Nothing -> throwM $ MkParseTimeException format' timeStr'
  where
    format' = format +-+ tzString

-- | 'readTimeFormat' specialized to 'ZonedTime'.
--
-- ==== __Examples__
-- >>> readTimeFormatZoned Format.defaultTimeLocale TimeFmt.hm "17:24"
-- Just 1970-01-01 17:24:00 +0000
--
-- >>> readTimeFormatZoned Format.defaultTimeLocale TimeFmt.hm12h "07:24 pm"
-- Just 1970-01-01 19:24:00 +0000
--
-- >>> readTimeFormatZoned Format.defaultTimeLocale TimeFmt.hmTZ "07:24 +5000"
-- Just 1970-01-01 07:24:00 +5000
--
-- >>> readTimeFormatZoned Format.defaultTimeLocale TimeFmt.hmTZ12h "07:24 pm -5000"
-- Just 1970-01-01 19:24:00 -5000
--
-- @since 0.1
readTimeFormatZoned :: TimeLocale -> TimeFormat -> Text -> Maybe ZonedTime
readTimeFormatZoned = readTimeFormat

-- | 'readTimeFormat' specialized to 'LocalTime'.
--
-- @since 0.1
readTimeFormatLocal :: TimeLocale -> TimeFormat -> Text -> Maybe LocalTime
readTimeFormatLocal = readTimeFormat

-- | @readTimeFormat locale format timeStr@ attempts to parse the @timeStr@ given
-- the expected @format@. No timezone is assumed, so if it is left off then
-- the result is UTC.
--
-- @since 0.1
readTimeFormat :: (ParseTime t) => TimeLocale -> TimeFormat -> Text -> Maybe t
readTimeFormat locale format timeStr = Format.parseTimeM True locale format' timeStr'
  where
    format' = T.unpack $ format ^. #unTimeFormat
    timeStr' = T.unpack timeStr

-- | Converts a zoned time to the given timezone.
--
-- ==== __Examples__
-- >>> let (Just sixPmUtc) = readTimeFormat Format.defaultTimeLocale TimeFmt.hm "18:00"
-- >>> convertZonedLabel sixPmUtc America__New_York
-- 1970-01-01 13:00:00 EST
--
-- @since 0.1
convertZonedLabel :: ZonedTime -> TZLabel -> ZonedTime
convertZonedLabel = convertLabel (const Local.zonedTimeToUTC)

-- | Converts a local time to the given timezone.
--
-- ==== __Examples__
-- >>> let (Just sixPmUtc) = readTimeFormat Format.defaultTimeLocale TimeFmt.hm "18:00"
-- >>> convertLocalLabel sixPmUtc America__New_York
-- 1970-01-01 18:00:00 EST
--
-- @since 0.1
convertLocalLabel :: LocalTime -> TZLabel -> ZonedTime
convertLocalLabel = convertLabel Zones.localTimeToUTCTZ

convertLabel :: (TZ -> a -> UTCTime) -> a -> TZLabel -> ZonedTime
convertLabel toUtcTime t tzLabel = Local.utcToZonedTime timeZone utcTime
  where
    -- This is a TZ i.e. the preliminary timezone corresponding to our
    -- label e.g. America/New_York -> TZ. This type is a stepping stone
    -- to the actual ZonedTime we want.
    tz = All.tzByLabel tzLabel
    -- Convert to UTC. Localtime will use the tz param to derive TimeZone
    -- information. ZonedTime will ignore it as it already carries TimeZone
    -- info.
    utcTime = toUtcTime tz t
    -- Get the final TimeZone from TZ and the UTC time. We need
    -- the time as the TimeZone can vary with the actual time e.g.
    -- America/New_York -> EST / EDT.
    timeZone = Zones.timeZoneForUTCTime tz utcTime

tzDatabaseToTZLabel :: (HasCallStack, MonadThrow m) => TZDatabase -> m TZLabel
tzDatabaseToTZLabel (TZDatabaseLabel lbl) = pure lbl
tzDatabaseToTZLabel (TZDatabaseText txt) =
  case Internal.tzNameToTZLabel txt of
    Just lbl -> pure lbl
    Nothing -> throwM $ MkParseTZDatabaseException txt

-- concat with a space
(+-+) :: (Semigroup a, IsString a) => a -> a -> a
xs +-+ ys = xs <> " " <> ys

infixr 5 +-+

getCurrentTimeZone :: (HasCallStack, Time :> es) => Eff es TimeZone
getCurrentTimeZone = do
  ZonedTime _ tz <- getSystemZonedTime
  pure tz

{-# LANGUAGE OverloadedLabels #-}

-- | This module provides functions for reading time strings. We also provide
-- functions for converting between time zones.
--
-- @since 0.1
module Data.Time.Conversion
  ( -- * General Read/Convert
    readConvertTime,
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

    -- ** Formats
    Types.hm,
    Types.hm12h,
    Types.hmTZ,
    Types.hmTZ12h,

    -- * Reading Time Strings
    readInLocalTimeZone,
    readTimeFormat,

    -- * Converting ZonedTime
    convertZoned,
    convertZonedLabel,

    -- * Errors
    TimeError (..),
    Types._TimeErrorParseTime,
    Types._TimeErrorParseTZDatabase,
    Types._TimeErrorLocalTimeZone,
  )
where

import Control.Exception
  ( Exception (..),
    SomeAsyncException (..),
    SomeException (..),
    catch,
    throwIO,
  )
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Time.Conversion.Types
  ( SrcTZ (..),
    TZConv (..),
    TZDatabase (..),
    TimeBuilder (..),
    TimeError (..),
    TimeFormat (..),
  )
import Data.Time.Conversion.Types qualified as Types
import Data.Time.Format (TimeLocale (..))
import Data.Time.Format qualified as Format
import Data.Time.LocalTime (TimeZone, ZonedTime)
import Data.Time.LocalTime qualified as Local
import Data.Time.Zones qualified as Zones
import Data.Time.Zones.All (TZLabel (..))
import Data.Time.Zones.All qualified as All
import Optics.Core ((^.))

-- | Reads the given time string based on the 'TimeBuilder'. For
-- @readConvertTime timeStr builder@ The semantics are:
--
-- * If 'srcTZ' is 'SrcTZLabel' then we manually append the timezone data
--   onto the time string and format. That is, do not use this option unless:
--    * @timeStr@ does __not__ contain a timezone like @EST@.
--    * @format@ does __not__ mention the timezone option @%Z@.
--
-- __Throws:__
--
-- * 'TimeErrorParseTime': Error parsing the time string.
-- * 'TimeErrorParseTZDatabase': Error parsing the tz database name.
-- * 'TimeErrorLocalTimeZone': Error retrieving local timezone.
--
-- @since 0.1
readConvertTime :: TimeBuilder -> Text -> IO ZonedTime
readConvertTime builder timeStr = do
  inTime <- case builder ^. #srcTZ of
    SrcTZLiteral ->
      readOrThrow (\f t -> pure $ readTimeFormat locale f t) format timeStr
    SrcTZConv TZConvLocal ->
      readOrThrow (readInLocalTimeZone locale) format timeStr
    SrcTZConv (TZConvDatabase tzdb) -> do
      lbl <- tzDatabaseToTZLabel tzdb
      let name = tzLabelToTimeZoneName lbl
          timeStr' = timeStr <> " " <> name
          format' = format <> " %Z"
      readOrThrow (\f t -> pure $ readTimeFormat locale f t) format' timeStr'

  case builder ^. #destTZ of
    TZConvDatabase tzdb -> do
      lbl <- tzDatabaseToTZLabel tzdb
      pure $ convertZonedLabel inTime lbl
    TZConvLocal -> do
      let inTimeUtc = Local.zonedTimeToUTC inTime
      currTZ <- Local.getCurrentTimeZone
      pure $ Local.utcToZonedTime currTZ inTimeUtc
  where
    format = builder ^. #format
    locale = builder ^. #locale
    readOrThrow reader f t = do
      mresult <- reader f t
      case mresult of
        Just result -> pure result
        Nothing -> throwIO $ TimeErrorParseTime f t

-- | @readInLocalTimeZone format timeStr@ attempts to parse the
-- @timeStr@ given the expected @format@. We parse into the current
-- system time zone, so:
--
-- * @format@ should __not__ mention "%Z"
-- * @timeStr@ should __not__ contain time zone information.
--
-- @
-- λ. readInLocalTimeZone "%H" "17"
-- Just 1970-01-01 17:00:00 NZST
-- @
--
-- @since 0.1
readInLocalTimeZone :: TimeLocale -> TimeFormat -> Text -> IO (Maybe ZonedTime)
readInLocalTimeZone locale format timeStr = do
  tzStr <-
    T.pack
      . show
      <$> Local.getCurrentTimeZone
      `safeCatch` (throwIO . TimeErrorLocalTimeZone)
  let timeStr' = timeStr <> " " <> tzStr
  case readTimeFormat locale format' timeStr' of
    Just zt -> pure $ Just zt
    Nothing -> throwIO $ TimeErrorParseTime format' timeStr'
  where
    format' = format <> " %Z"

-- | @readTimeFormat format timeStr@ attempts to parse the @timeStr@ given
-- the expected @format@. No timezone is assumed, so if it is left off then
-- the result is UTC.
--
-- @
-- λ. readTimeFormat "%H %Z" "17 EST"
-- Just 1970-01-01 17:00:00 EST
--
-- λ. readTimeFormat "%H" "17"
-- Just 1970-01-01 17:00:00 +0000
-- @
--
-- @since 0.1
readTimeFormat :: TimeLocale -> TimeFormat -> Text -> Maybe ZonedTime
readTimeFormat locale (MkTimeFormat format) timeStr = Format.parseTimeM True locale format' timeStr'
  where
    format' = T.unpack format
    timeStr' = T.unpack timeStr

-- | Converts a zoned time to the given timezone.
--
-- @since 0.1
convertZoned :: ZonedTime -> Text -> Maybe ZonedTime
convertZoned zt = fmap (convertZonedLabel zt) . txtToTZLabel

-- | Converts a zoned time to the given timezone.
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
    Nothing -> throwIO $ TimeErrorParseTZDatabase txt

tzLabelToTimeZoneName :: TZLabel -> Text
tzLabelToTimeZoneName = T.pack . Local.timeZoneName . tzLabelToTimeZone

tzLabelToTimeZone :: TZLabel -> TimeZone
tzLabelToTimeZone = (`Zones.timeZoneForPOSIX` 0) . All.tzByLabel

txtToTZLabel :: Text -> Maybe TZLabel
txtToTZLabel = All.fromTZName . TEnc.encodeUtf8

safeCatch :: IO a -> (SomeException -> IO a) -> IO a
safeCatch io handler =
  io `catch` \ex ->
    case fromException (toException ex) of
      Just (SomeAsyncException _) -> throwIO ex
      Nothing -> handler ex

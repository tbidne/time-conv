{-# LANGUAGE UndecidableInstances #-}

-- | This module provides types for reading / converting
-- between timezones.
--
-- @since 0.1
module Data.Time.Conversion.Types
  ( TimeBuilder (..),
    SrcTZ (..),
    _SrcTZConv,
    _SrcTZLiteral,
    TZConv (..),
    _TZConvLocal,
    _TZConvDatabase,
    TZDatabase (..),
    _TZDatabaseLabel,
    _TZDatabaseText,

    -- * Formatting
    TimeFormat (..),
    _TimeFormatManual,
    _TimeFormatFull,
    timeFormatStringIso,
    hm,
    hm12h,
    hmTZ,
    hmTZ12h,

    -- * Errors
    TimeError (..),
    _TimeErrorParseTime,
    _TimeErrorParseTZDatabase,
    _TimeErrorLocalTimeZone,
    _TimeErrorLocalSystemTime,
  )
where

import Control.Exception (Exception (..))
import Data.Default (Default (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion.Utils qualified as Utils
import Data.Time.Format (TimeLocale (..))
import Data.Time.Format qualified as Format
import Data.Time.Zones.All (TZLabel (..))
import Optics.Core (A_Lens, Iso', LabelOptic (..), Prism', Review, (^.))
import Optics.Core qualified as O

-- | Determines how to read and convert a time string. The 'Default' instance
-- uses:
--
-- * @format = "%H:%M"@ (24hr hours:minutes)
-- * @srzTZ = 'SrcTZConv' 'TZConvLocal'@ (local)
-- * @destTZ = 'TZConvLocal'@ (local)
-- * @locale = 'Utils.timeLocaleAllZones'@ (all locales)
-- * @timeString = 'Nothing'@ (read system time)
--
-- @since 0.1
data TimeBuilder = MkTimeBuilder
  { -- | Format used when parsing the time string.
    --
    -- @since 0.1
    format :: TimeFormat,
    -- | Timezone in which to read the string.
    --
    -- @since 0.1
    srcTZ :: SrcTZ,
    -- | Timezone in which to convert.
    --
    -- @since 0.1
    destTZ :: TZConv,
    -- | Locale to use when parsing.
    --
    -- @since 0.1
    locale :: TimeLocale,
    -- | The time string to parse. If empty, we retrieve the local system
    -- time instead.
    --
    -- @since 0.1
    timeString :: Maybe Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ TimeFormat, b ~ TimeFormat) =>
  LabelOptic "format" k TimeBuilder TimeBuilder a b
  where
  labelOptic = O.lens format (\tb f -> tb {format = f})

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ SrcTZ, b ~ SrcTZ) =>
  LabelOptic "srcTZ" k TimeBuilder TimeBuilder a b
  where
  labelOptic = O.lens srcTZ (\tb tz -> tb {srcTZ = tz})

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ TimeLocale, b ~ TimeLocale) =>
  LabelOptic "locale" k TimeBuilder TimeBuilder a b
  where
  labelOptic = O.lens locale (\tb l -> tb {locale = l})

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ TZConv, b ~ TZConv) =>
  LabelOptic "destTZ" k TimeBuilder TimeBuilder a b
  where
  labelOptic = O.lens destTZ (\tb tz -> tb {destTZ = tz})

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe Text, b ~ Maybe Text) =>
  LabelOptic "timeString" k TimeBuilder TimeBuilder a b
  where
  labelOptic = O.lens timeString (\tb ts -> tb {timeString = ts})

-- | @since 0.1
instance Default TimeBuilder where
  def = MkTimeBuilder def def def Utils.timeLocaleAllZones Nothing

-- | Conversion timezone options.
--
-- ==== __Examples__
-- >>> def :: TZConv
-- TZConvLocal
--
-- >>> TZConvDatabase $ TZDatabaseLabel America__New_York
-- TZConvDatabase (TZDatabaseLabel America__New_York)
--
-- @since 0.1
data TZConv
  = -- | Represents the local timezone.
    --
    -- @since 0.1
    TZConvLocal
  | -- | Represents an explicitly given timezone.
    --
    -- @since 0.1
    TZConvDatabase TZDatabase
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
_TZConvLocal :: Prism' TZConv ()
_TZConvLocal = O.prism (const TZConvLocal) from
  where
    from TZConvLocal = Right ()
    from other = Left other

-- | @since 0.1
_TZConvDatabase :: Prism' TZConv TZDatabase
_TZConvDatabase = O.prism TZConvDatabase from
  where
    from (TZConvDatabase tzdb) = Right tzdb
    from other = Left other

-- | @since 0.1
instance Default TZConv where
  def = TZConvLocal

-- | Determines what timezone we use when reading a time string.
--
-- ==== __Examples__
-- >>> def :: SrcTZ
-- SrcTZConv TZConvLocal
--
-- @since 0.1
data SrcTZ
  = -- | Use the 'TZConv' to determine which timezone to use.
    --
    -- @since 0.1
    SrcTZConv TZConv
  | -- | Timezones are not given any extra consideration. This is useful
    -- when either the string itself contains a time zone (e.g. "15:03 EST")
    -- or we want UTC.
    --
    -- @since 0.1
    SrcTZLiteral
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
_SrcTZConv :: Prism' SrcTZ TZConv
_SrcTZConv = O.prism SrcTZConv from
  where
    from (SrcTZConv tzdb) = Right tzdb
    from other = Left other

-- | @since 0.1
_SrcTZLiteral :: Prism' SrcTZ ()
_SrcTZLiteral = O.prism (const SrcTZLiteral) from
  where
    from SrcTZLiteral = Right ()
    from other = Left other

-- | @since 0.1
instance Default SrcTZ where
  def = SrcTZConv def

-- | Options for interpreting a timezone.
--
-- ==== __Examples__
-- >>> TZDatabaseLabel America__New_York
-- TZDatabaseLabel America__New_York
--
-- >>> TZDatabaseText "America/New_York"
-- TZDatabaseText "America/New_York"
--
-- @since 0.1
data TZDatabase
  = -- | Uses a 'TZLabel'. This option should be preferred as it enables
    -- total conversions.
    --
    -- @since 0.1
    TZDatabaseLabel TZLabel
  | -- | Interprets the 'Text' as a tz database name e.g. America/New_York.
    -- Obviously such conversions cannot be total.
    --
    -- @since 0.1
    TZDatabaseText Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
_TZDatabaseLabel :: Prism' TZDatabase TZLabel
_TZDatabaseLabel = O.prism TZDatabaseLabel from
  where
    from (TZDatabaseLabel l) = Right l
    from other = Left other

-- | @since 0.1
_TZDatabaseText :: Prism' TZDatabase Text
_TZDatabaseText = O.prism TZDatabaseText from
  where
    from (TZDatabaseText t) = Right t
    from other = Left other

-- | Time formatting string. The 'Monoid' instance behaves like 'Text', where
-- @'TimeFormatManual' ""@ is the identity and 'TimeFormatFull' is the \"top\".
--
-- ==== __Examples__
-- >>> def :: TimeFormat
-- TimeFormatManual "%H:%M"
--
-- >>> mempty :: TimeFormat
-- TimeFormatManual ""
--
-- >>> hm <> " %Z"
-- TimeFormatManual "%H:%M %Z"
--
-- >>> hm <> TimeFormatFull
-- TimeFormatFull
--
-- @since 0.1
data TimeFormat
  = -- | Corresponds to RFC822: @%a, %_d %b %Y %H:%M:%S %Z@.
    --
    -- @since 0.1
    TimeFormatFull
  | -- | Manual format string.
    --
    -- @since 0.1
    TimeFormatManual Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance IsString TimeFormat where
  fromString = TimeFormatManual . T.pack

-- | @since 0.1
instance Semigroup TimeFormat where
  TimeFormatFull <> _ = TimeFormatFull
  _ <> TimeFormatFull = TimeFormatFull
  TimeFormatManual l <> TimeFormatManual r = TimeFormatManual (l <> r)

-- | @since 0.1
instance Monoid TimeFormat where
  mempty = TimeFormatManual ""

-- | @since 0.1
_TimeFormatManual :: Prism' TimeFormat Text
_TimeFormatManual = O.prism TimeFormatManual from
  where
    from (TimeFormatManual f) = Right f
    from other = Left other

-- | @since 0.1
_TimeFormatFull :: Prism' TimeFormat ()
_TimeFormatFull = O.prism (const TimeFormatFull) from
  where
    from TimeFormatFull = Right ()
    from other = Left other

-- | 'Iso'' between 'TimeFormat' and its underlying string
-- representation.
--
-- ==== __Examples__
-- >>> import Optics.Core (view, review)
-- >>> view timeFormatStringIso TimeFormatFull
-- "%a, %_d %b %Y %H:%M:%S %Z"
--
-- >>> review timeFormatStringIso "%H:%M"
-- TimeFormatManual "%H:%M"
--
-- @since 0.1
timeFormatStringIso :: Iso' TimeFormat String
timeFormatStringIso = O.iso from to
  where
    from TimeFormatFull = Format.rfc822DateFormat
    from (TimeFormatManual f) = T.unpack f
    to f
      | f == Format.rfc822DateFormat = TimeFormatFull
      | otherwise = TimeFormatManual (T.pack f)

-- | Alias for 'hm'.
--
-- @since 0.1
instance Default TimeFormat where
  def = hm

-- | Format for 24-hour @hours:minutes@.
--
-- >>> hm
-- TimeFormatManual "%H:%M"
--
-- @since 0.1
hm :: TimeFormat
hm = "%H:%M"

-- | Format for 12-hour @hours:minutes am/pm@.
--
-- >>> hm12h
-- TimeFormatManual "%I:%M %P"
--
-- @since 0.1
hm12h :: TimeFormat
hm12h = "%I:%M %P"

-- | Format for 24-hour @hours:minutes TZ@.
--
-- >>> hmTZ
-- TimeFormatManual "%H:%M %Z"
--
-- @since 0.1
hmTZ :: TimeFormat
hmTZ = "%H:%M %Z"

-- | Format for 12-hour @hours:minutes am/pm TZ@.
--
-- >>> hmTZ12h
-- TimeFormatManual "%I:%M %P %Z"
--
-- @since 0.1
hmTZ12h :: TimeFormat
hmTZ12h = "%I:%M %P %Z"

-- | Errors that can occur when reading/converting times.
--
-- @since 0.1
data TimeError
  = -- | Error parsing time string.
    --
    -- @since 0.1
    TimeErrorParseTime TimeFormat Text
  | -- | Error parsing tz database name.
    --
    -- @since 0.1
    TimeErrorParseTZDatabase Text
  | -- | Error retrieving the local time zone.
    --
    -- @since 0.1
    forall e. Exception e => TimeErrorLocalTimeZone e
  | -- | Error retrieving the local system time.
    --
    -- @since 0.1
    forall e. Exception e => TimeErrorLocalSystemTime e

-- | @since 0.1
deriving stock instance Show TimeError

-- | @since 0.1
instance Exception TimeError where
  displayException (TimeErrorParseTime f t) =
    "Could not parse time string <"
      <> T.unpack t
      <> "> with format <"
      <> (f ^. timeFormatStringIso)
      <> ">"
  displayException (TimeErrorParseTZDatabase tzdb) =
    "Could not parse tz database name <"
      <> T.unpack tzdb
      <> ">. Wanted a name like America/New_York."
  displayException (TimeErrorLocalTimeZone e) =
    "Local timezone exception: " <> displayException e
  displayException (TimeErrorLocalSystemTime e) =
    "Local system time exception: " <> displayException e

-- | @since 0.1
_TimeErrorParseTime :: Prism' TimeError (TimeFormat, Text)
_TimeErrorParseTime = O.prism (uncurry TimeErrorParseTime) from
  where
    from (TimeErrorParseTime f t) = Right (f, t)
    from other = Left other

-- | @since 0.1
_TimeErrorParseTZDatabase :: Prism' TimeError Text
_TimeErrorParseTZDatabase = O.prism TimeErrorParseTZDatabase from
  where
    from (TimeErrorParseTZDatabase t) = Right t
    from other = Left other

-- | @since 0.1
_TimeErrorLocalTimeZone :: Exception e => Review TimeError e
_TimeErrorLocalTimeZone = O.unto TimeErrorLocalTimeZone

-- | @since 0.1
_TimeErrorLocalSystemTime :: Exception e => Review TimeError e
_TimeErrorLocalSystemTime = O.unto TimeErrorLocalSystemTime

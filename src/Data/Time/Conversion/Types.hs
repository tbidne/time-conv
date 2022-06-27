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
    hm,
    hm12h,
    hmTZ,
    hmTZ12h,
    rfc822,

    -- * Errors
    ParseTimeException (..),
    ParseTZDatabaseException (..),
    LocalTimeZoneException (..),
    LocalSystemTimeException (..),
  )
where

import Control.Exception (Exception (..))
import Data.Default (Default (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion.Utils qualified as Utils
import Data.Time.Format (TimeLocale (..))
import Data.Time.Zones.All (TZLabel (..))
import Optics.Core (A_Lens, An_Iso, LabelOptic (..), Prism', (^.))
import Optics.Core qualified as O

-- | Determines how to read and convert a time string. The 'Default' instance
-- uses:
--
-- * @format = "%H:%M"@ (24hr hours:minutes)
-- * @srzTZ = 'SrcTZConv' 'TZConvLocal'@ (local)
-- * @destTZ = 'TZConvLocal'@ (local)
-- * @locale = 'Utils.timeLocaleAllZones'@ (all locales)
-- * @today = 'False'@ (do not automatically add current day)
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
    -- | 'True' if this is the current day. This is a convenient way to parse
    -- the string as if it includes the current day's date.
    --
    -- @since 0.1
    today :: Bool,
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
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "today" k TimeBuilder TimeBuilder a b
  where
  labelOptic = O.lens today (\tb b -> tb {today = b})

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe Text, b ~ Maybe Text) =>
  LabelOptic "timeString" k TimeBuilder TimeBuilder a b
  where
  labelOptic = O.lens timeString (\tb ts -> tb {timeString = ts})

-- | @since 0.1
instance Default TimeBuilder where
  def = MkTimeBuilder def def def Utils.timeLocaleAllZones False Nothing

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
    -- when either the string itself contains a timezone (e.g. "15:03 EST")
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

-- | Time formatting string. The 'Monoid' instance behaves like 'Text',
-- whereas 'Default' is an alias for 'hm'.
--
-- ==== __Examples__
-- >>> def :: TimeFormat
-- MkTimeFormat {unTimeFormat = "%H:%M"}
--
-- >>> mempty :: TimeFormat
-- MkTimeFormat {unTimeFormat = ""}
--
-- >>> hm <> " %Z"
-- MkTimeFormat {unTimeFormat = "%H:%M %Z"}
--
-- @since 0.1
newtype TimeFormat = MkTimeFormat
  { -- | @since 0.1
    unTimeFormat :: Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )
  deriving
    ( -- | @since 0.1
      Monoid,
      -- | @since 0.1
      IsString,
      -- | @since 0.1
      Semigroup
    )
    via Text

-- | Alias for 'hm'.
--
-- @since 0.1
instance Default TimeFormat where
  def = hm

-- | @since 0.1
instance
  (k ~ An_Iso, a ~ Text, b ~ Text) =>
  LabelOptic "unTimeFormat" k TimeFormat TimeFormat a b
  where
  labelOptic = O.iso unTimeFormat MkTimeFormat

-- | Format for 24-hour @hours:minutes@.
--
-- >>> hm
-- MkTimeFormat {unTimeFormat = "%H:%M"}
--
-- @since 0.1
hm :: TimeFormat
hm = "%H:%M"

-- | Format for 12-hour @hours:minutes am/pm@.
--
-- >>> hm12h
-- MkTimeFormat {unTimeFormat = "%I:%M %P"}
--
-- @since 0.1
hm12h :: TimeFormat
hm12h = "%I:%M %P"

-- | Format for 24-hour @hours:minutes TZ@.
--
-- >>> hmTZ
-- MkTimeFormat {unTimeFormat = "%H:%M %Z"}
--
-- @since 0.1
hmTZ :: TimeFormat
hmTZ = "%H:%M %Z"

-- | Format for 12-hour @hours:minutes am/pm TZ@.
--
-- >>> hmTZ12h
-- MkTimeFormat {unTimeFormat = "%I:%M %P %Z"}
--
-- @since 0.1
hmTZ12h :: TimeFormat
hmTZ12h = "%I:%M %P %Z"

-- | Format for RFC822: @%a, %_d %b %Y %H:%M:%S %Z@.
--
-- >>> rfc822
-- MkTimeFormat {unTimeFormat = "%a, %_d %b %Y %H:%M:%S %Z"}
--
-- @since 0.1
rfc822 :: TimeFormat
rfc822 = "%a, %_d %b %Y %H:%M:%S %Z"

-- | Exception parsing time string.
--
-- @since 0.1
data ParseTimeException = MkParseTimeException TimeFormat Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception ParseTimeException where
  displayException (MkParseTimeException f t) =
    "Could not parse time string <"
      <> T.unpack t
      <> "> with format <"
      <> T.unpack (f ^. #unTimeFormat)
      <> ">"

-- | Exception parsing tz database names.
--
-- @since 0.1
newtype ParseTZDatabaseException = MkParseTZDatabaseException Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception ParseTZDatabaseException where
  displayException (MkParseTZDatabaseException tzdb) =
    "Could not parse tz database name <"
      <> T.unpack tzdb
      <> ">. Wanted a name like America/New_York."

-- | Exception parsing tz database names.
--
-- @since 0.1
data LocalTimeZoneException = forall e. Exception e => MkLocalTimeZoneException e

-- | @since 0.1
deriving stock instance Show LocalTimeZoneException

-- | @since 0.1
instance Exception LocalTimeZoneException where
  displayException (MkLocalTimeZoneException e) =
    "Local timezone exception: " <> displayException e

-- | Exception parsing tz database names.
--
-- @since 0.1
data LocalSystemTimeException = forall e. Exception e => MkLocalSystemTimeException e

-- | @since 0.1
deriving stock instance Show LocalSystemTimeException

-- | @since 0.1
instance Exception LocalSystemTimeException where
  displayException (MkLocalSystemTimeException e) =
    "Local system time exception: " <> displayException e

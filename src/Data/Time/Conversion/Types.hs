{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides types for reading / converting
-- between timezones.
--
-- @since 0.1
module Data.Time.Conversion.Types
  ( TimeReader (..),
    defaultTimeReader,
    SrcTZ (..),
    TZDatabase (..),

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

    -- * Optics
    _SrcTZDatabase,
    _SrcTZLiteral,
    _TZDatabaseLabel,
    _TZDatabaseText,
    _MkTimeFormat,
    _MkParseTimeException,
    _MkParseTZDatabaseException,
    _MkLocalTimeZoneException,
    _MkLocalSystemTimeException,
  )
where

import Control.DeepSeq (NFData (..), deepseq)
import Control.Exception (Exception (..))
import Data.Default (Default (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion.Utils qualified as Utils
import Data.Time.Format (TimeLocale (..))
import Data.Time.Zones.All (TZLabel (..))
import GHC.Generics (Generic)
import Optics.Core
  ( A_Lens,
    Iso',
    LabelOptic (..),
    Prism',
    Review,
    iso,
    lens,
    prism,
    unto,
    (^.),
  )

-- | Determines how to read a time string.
--
-- @since 0.1
data TimeReader = MkTimeReader
  { -- | Format used when parsing the time string.
    --
    -- @since 0.1
    format :: TimeFormat,
    -- | Timezone in which to read the string. 'Nothing' corresponds to
    -- local timezone. If 'srcTZ' is 'SrcTZDatabase' then:
    --
    --     * 'timeString' should __not__ contain a timezone like @EST@.
    --     * 'format' should __not__ mention the timezone option @%Z@.
    --
    -- @since 0.1
    srcTZ :: Maybe SrcTZ,
    -- | Locale to use when parsing.
    --
    -- @since 0.1
    locale :: TimeLocale,
    -- | 'True' if this is the current day. This is a convenient way to parse
    -- the string as if it includes the current day's date.
    --
    -- @since 0.1
    today :: Bool,
    -- | The time string to parse.
    --
    -- @since 0.1
    timeString :: Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance NFData TimeReader where
  rnf (MkTimeReader f s l td ts) =
    f `deepseq` s `deepseq` l `evalLocale` td `deepseq` ts `deepseq` ()

-- because TimeLocale does not have an NFData instance
evalLocale :: TimeLocale -> b -> b
evalLocale (TimeLocale {..}) x =
  wDays `deepseq`
    months `deepseq`
      amPm `deepseq`
        dateTimeFmt `deepseq`
          dateFmt `deepseq`
            timeFmt `deepseq`
              time12Fmt `deepseq`
                knownTimeZones `deepseq`
                  x

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ TimeFormat, b ~ TimeFormat) =>
  LabelOptic "format" k TimeReader TimeReader a b
  where
  labelOptic = lens format (\tb f -> tb {format = f})

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe SrcTZ, b ~ Maybe SrcTZ) =>
  LabelOptic "srcTZ" k TimeReader TimeReader a b
  where
  labelOptic = lens srcTZ (\tb tz -> tb {srcTZ = tz})

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ TimeLocale, b ~ TimeLocale) =>
  LabelOptic "locale" k TimeReader TimeReader a b
  where
  labelOptic = lens locale (\tb l -> tb {locale = l})

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "today" k TimeReader TimeReader a b
  where
  labelOptic = lens today (\tb b -> tb {today = b})

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Text, b ~ Text) =>
  LabelOptic "timeString" k TimeReader TimeReader a b
  where
  labelOptic = lens timeString (\tb ts -> tb {timeString = ts})

-- | Given a time string, returns a default time reader.
--
-- * @format = "%H:%M"@: 24hr hours:minutes
-- * @srzTZ = 'Nothing'@: local
-- * @locale = 'Utils.timeLocaleAllZones'@: all locales
-- * @today = 'False'@: do not automatically add current day
--
-- @since 0.1
defaultTimeReader :: Text -> TimeReader
defaultTimeReader = MkTimeReader def Nothing Utils.timeLocaleAllZones False

-- | Determines what timezone we use when reading a time string.
--
-- @since 0.1
data SrcTZ
  = -- | Use the 'TZDatabase' to determine which timezone to use.
    --
    -- @since 0.1
    SrcTZDatabase TZDatabase
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
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
_SrcTZDatabase :: Prism' SrcTZ TZDatabase
_SrcTZDatabase = prism SrcTZDatabase from
  where
    from (SrcTZDatabase tzdb) = Right tzdb
    from other = Left other

-- | @since 0.1
_SrcTZLiteral :: Prism' SrcTZ ()
_SrcTZLiteral = prism (const SrcTZLiteral) from
  where
    from SrcTZLiteral = Right ()
    from other = Left other

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
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
_TZDatabaseLabel :: Prism' TZDatabase TZLabel
_TZDatabaseLabel = prism TZDatabaseLabel from
  where
    from (TZDatabaseLabel l) = Right l
    from other = Left other

-- | @since 0.1
_TZDatabaseText :: Prism' TZDatabase Text
_TZDatabaseText = prism TZDatabaseText from
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
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
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
_MkTimeFormat :: Iso' TimeFormat Text
_MkTimeFormat = iso unTimeFormat MkTimeFormat

-- | Format for 24-hour @hours:minutes@.
--
-- >>> hm
-- MkTimeFormat {unTimeFormat = "%H:%M"}
--
-- @since 0.1
hm :: TimeFormat
hm = "%H:%M"
{-# INLINE hm #-}

-- | Format for 12-hour @hours:minutes am/pm@.
--
-- >>> hm12h
-- MkTimeFormat {unTimeFormat = "%I:%M %P"}
--
-- @since 0.1
hm12h :: TimeFormat
hm12h = "%I:%M %P"
{-# INLINE hm12h #-}

-- | Format for 24-hour @hours:minutes TZ@.
--
-- >>> hmTZ
-- MkTimeFormat {unTimeFormat = "%H:%M %Z"}
--
-- @since 0.1
hmTZ :: TimeFormat
hmTZ = "%H:%M %Z"
{-# INLINE hmTZ #-}

-- | Format for 12-hour @hours:minutes am/pm TZ@.
--
-- >>> hmTZ12h
-- MkTimeFormat {unTimeFormat = "%I:%M %P %Z"}
--
-- @since 0.1
hmTZ12h :: TimeFormat
hmTZ12h = "%I:%M %P %Z"
{-# INLINE hmTZ12h #-}

-- | Format for RFC822: @%a, %_d %b %Y %H:%M:%S %Z@.
--
-- >>> rfc822
-- MkTimeFormat {unTimeFormat = "%a, %_d %b %Y %H:%M:%S %Z"}
--
-- @since 0.1
rfc822 :: TimeFormat
rfc822 = "%a, %_d %b %Y %H:%M:%S %Z"
{-# INLINE rfc822 #-}

-- | Exception parsing time string.
--
-- @since 0.1
data ParseTimeException = MkParseTimeException TimeFormat Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance Exception ParseTimeException where
  displayException (MkParseTimeException f t) =
    "Could not parse time string <"
      <> T.unpack t
      <> "> with format <"
      <> T.unpack (f ^. #unTimeFormat)
      <> ">"

-- | @since 0.1
_MkParseTimeException :: Iso' ParseTimeException (TimeFormat, Text)
_MkParseTimeException = iso (\(MkParseTimeException f t) -> (f, t)) (uncurry MkParseTimeException)
{-# INLINE _MkParseTimeException #-}

-- | Exception parsing tz database names.
--
-- @since 0.1
newtype ParseTZDatabaseException = MkParseTZDatabaseException Text
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
_MkParseTZDatabaseException :: Iso' ParseTZDatabaseException Text
_MkParseTZDatabaseException = iso (\(MkParseTZDatabaseException t) -> t) MkParseTZDatabaseException
{-# INLINE _MkParseTZDatabaseException #-}

-- | @since 0.1
instance Exception ParseTZDatabaseException where
  displayException (MkParseTZDatabaseException tzdb) =
    T.unpack $
      "Could not parse tz database name <"
        <> tzdb
        <> ">. Wanted a name like America/New_York."

-- | Exception reading local system timezone.
--
-- @since 0.1
data LocalTimeZoneException = forall e. Exception e => MkLocalTimeZoneException e

-- | @since 0.1
deriving stock instance Show LocalTimeZoneException

-- | @since 0.1
instance Exception LocalTimeZoneException where
  displayException (MkLocalTimeZoneException e) =
    "Local timezone exception: " <> displayException e

-- | @since 0.1
_MkLocalTimeZoneException :: Exception e => Review LocalTimeZoneException e
_MkLocalTimeZoneException = unto MkLocalTimeZoneException
{-# INLINE _MkLocalTimeZoneException #-}

-- | Exception reading local system time.
--
-- @since 0.1
data LocalSystemTimeException = forall e. Exception e => MkLocalSystemTimeException e

-- | @since 0.1
deriving stock instance Show LocalSystemTimeException

-- | @since 0.1
instance Exception LocalSystemTimeException where
  displayException (MkLocalSystemTimeException e) =
    "Local system time exception: " <> displayException e

-- | @since 0.1
_MkLocalSystemTimeException :: Exception e => Review LocalSystemTimeException e
_MkLocalSystemTimeException = unto MkLocalSystemTimeException
{-# INLINE _MkLocalSystemTimeException #-}

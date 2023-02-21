{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides types for reading / converting
-- between timezones.
--
-- @since 0.1
module Data.Time.Conversion.Types
  ( TimeReader (..),
    defaultTimeReader,
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

import Control.DeepSeq (NFData (..), deepseq)
import Control.Exception (Exception (..))
import Data.Default (Default (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Zones.All (TZLabel (..))
import GHC.Generics (Generic)
import Optics.Core ((^.))
import Optics.TH (makeFieldLabelsNoPrefix, makePrisms)

-- $setup
-- >>> import Data.Default (Default (def))
-- >>> import Data.Time.Zones.All (TZLabel (..))

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
makePrisms ''TZDatabase

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
-- >>> "%Y " <> hm
-- MkTimeFormat {unTimeFormat = "%Y %H:%M"}
--
-- @since 0.1
newtype TimeFormat = MkTimeFormat
  {unTimeFormat :: Text}
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

-- | @since 0.1
makeFieldLabelsNoPrefix ''TimeFormat

-- | Alias for 'hm'.
--
-- @since 0.1
instance Default TimeFormat where
  def = hm

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

-- | Format for 24-hour @hours:minutes TZ@. As this contains a timezone
-- flag, it should be used for formatting output only. In particular, it should
-- __not__ be used with 'TimeReader'\'s 'format'.
--
-- >>> hmTZ
-- MkTimeFormat {unTimeFormat = "%H:%M %Z"}
--
-- @since 0.1
hmTZ :: TimeFormat
hmTZ = "%H:%M %Z"
{-# INLINE hmTZ #-}

-- | Format for 12-hour @hours:minutes am/pm TZ@. As this contains a timezone
-- flag, it should be used for formatting output only. In particular, it should
-- __not__ be used with 'TimeReader'\'s 'format'.
--
-- >>> hmTZ12h
-- MkTimeFormat {unTimeFormat = "%I:%M %P %Z"}
--
-- @since 0.1
hmTZ12h :: TimeFormat
hmTZ12h = "%I:%M %P %Z"
{-# INLINE hmTZ12h #-}

-- | Format for RFC822: @%a, %_d %b %Y %H:%M:%S %Z@. As this contains a timezone
-- flag, it should be used for formatting output only. In particular, it should
-- __not__ be used with 'TimeReader'\'s 'format'.
--
-- >>> rfc822
-- MkTimeFormat {unTimeFormat = "%a, %_d %b %Y %H:%M:%S %Z"}
--
-- @since 0.1
rfc822 :: TimeFormat
rfc822 = "%a, %_d %b %Y %H:%M:%S %Z"
{-# INLINE rfc822 #-}

-- | Determines how to read a time string.
--
-- @since 0.1
data TimeReader = MkTimeReader
  { -- | Format used when parsing the time string. This should __not__ include
    -- timezone formatting e.g. @%Z@. Use 'srcTZ' instead.
    --
    -- @since 0.1
    format :: TimeFormat,
    -- | Timezone in which to read the string. 'Nothing' corresponds to
    -- local timezone.
    --
    -- @since 0.1
    srcTZ :: Maybe TZDatabase,
    -- | 'True' if this is the current day. This is a convenient way to parse
    -- the string as if it includes the current day's date.
    --
    -- @since 0.1
    today :: Bool,
    -- | The time string to parse. This should __not__ include a timezone
    -- e.g. EST. Use 'srcTZ' instead.
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
  rnf (MkTimeReader f s td ts) =
    f `deepseq` s `deepseq` td `deepseq` ts `deepseq` ()

-- | @since 0.1
makeFieldLabelsNoPrefix ''TimeReader

-- | Given a time string, returns a default time reader.
--
-- * @format = "%H:%M"@: 24hr hours:minutes
-- * @srzTZ = 'Nothing'@: local
-- * @today = 'False'@: do not automatically add current day
--
-- @since 0.1
defaultTimeReader :: Text -> TimeReader
defaultTimeReader = MkTimeReader def Nothing False

-- | Exception parsing time string.
--
-- @since 0.1
data ParseTimeException = MkParseTimeException !TimeFormat !Text
  deriving stock
    ( -- | @since 0.1
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

-- | Exception parsing tz database names.
--
-- @since 0.1
newtype ParseTZDatabaseException = MkParseTZDatabaseException Text
  deriving stock
    ( -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance Exception ParseTZDatabaseException where
  displayException (MkParseTZDatabaseException tzdb) =
    "Could not parse tz database name <"
      <> T.unpack tzdb
      <> ">. Wanted a name like America/New_York."

-- | Exception reading local system timezone.
--
-- @since 0.1
data LocalTimeZoneException
  = forall e. (Exception e) => MkLocalTimeZoneException !e

-- | @since 0.1
deriving stock instance Show LocalTimeZoneException

-- | @since 0.1
instance Exception LocalTimeZoneException where
  displayException (MkLocalTimeZoneException e) =
    "Local timezone exception: " <> displayException e

-- | Exception reading local system time.
--
-- @since 0.1
data LocalSystemTimeException
  = forall e. (Exception e) => MkLocalSystemTimeException !e

-- | @since 0.1
deriving stock instance Show LocalSystemTimeException

-- | @since 0.1
instance Exception LocalSystemTimeException where
  displayException (MkLocalSystemTimeException e) =
    "Local system time exception: " <> displayException e

{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.1
module Data.Time.Conversion.Types.TimeReader
  ( TimeReader (..),
    defaultTimeReader,
  )
where

import Control.DeepSeq (NFData (..), deepseq)
import Data.Default (Default (..))
import Data.Text (Text)
import Data.Time.Conversion.Types.TZDatabase (TZDatabase)
import Data.Time.Conversion.Types.TimeFormat (TimeFormat)
import GHC.Generics (Generic)
import Optics.Core ( A_Lens, LabelOptic (labelOptic), lensVL)

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
instance
  (k ~ A_Lens, a ~ TimeFormat, b ~ TimeFormat) =>
  LabelOptic "format" k TimeReader TimeReader a b
  where
  labelOptic = lensVL $ \f (MkTimeReader _format _srcTZ _today _timeString) ->
    fmap (\format' -> MkTimeReader format' _srcTZ _today _timeString) (f _format)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe TZDatabase, b ~ Maybe TZDatabase) =>
  LabelOptic "srcTZ" k TimeReader TimeReader a b
  where
  labelOptic = lensVL $ \f (MkTimeReader _format _srcTZ _today _timeString) ->
    fmap (\srcTZ' -> MkTimeReader _format srcTZ' _today _timeString) (f _srcTZ)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "today" k TimeReader TimeReader a b
  where
  labelOptic = lensVL $ \f (MkTimeReader _format _srcTZ _today _timeString) ->
    fmap (\today' -> MkTimeReader _format _srcTZ today' _timeString) (f _today)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Text, b ~ Text) =>
  LabelOptic "timeString" k TimeReader TimeReader a b
  where
  labelOptic = lensVL $ \f (MkTimeReader _format _srcTZ _today _timeString) ->
    fmap (MkTimeReader _format _srcTZ _today) (f _timeString)
  {-# INLINE labelOptic #-}

-- | Given a time string, returns a default time reader.
--
-- * @format = "%H:%M"@: 24hr hours:minutes
-- * @srzTZ = 'Nothing'@: local
-- * @today = 'False'@: do not automatically add current day
--
-- @since 0.1
defaultTimeReader :: Text -> TimeReader
defaultTimeReader = MkTimeReader def Nothing False
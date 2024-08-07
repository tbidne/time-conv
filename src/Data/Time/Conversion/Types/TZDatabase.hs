-- | @since 0.1
module Data.Time.Conversion.Types.TZDatabase
  ( TZDatabase (..),
    _TZDatabaseLabel,
    _TZDatabaseText,
  )
where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.Time.Zones.All (TZLabel)
import GHC.Generics (Generic)
import Optics.Core (Prism', prism)

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
_TZDatabaseLabel =
  prism
    TZDatabaseLabel
    ( \case
        TZDatabaseLabel lbl -> Right lbl
        other -> Left other
    )
{-# INLINE _TZDatabaseLabel #-}

-- | @since 0.1
_TZDatabaseText :: Prism' TZDatabase Text
_TZDatabaseText =
  prism
    TZDatabaseText
    ( \case
        TZDatabaseText t -> Right t
        other -> Left other
    )
{-# INLINE _TZDatabaseText #-}

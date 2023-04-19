{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.1
module Data.Time.Conversion.Types.Date
  ( Date (..),
    parseDate,
    _DateToday,
    _DateLiteral,
    Internal.DateString,
    Internal.parseDateString,
    Internal.unDateString,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Text (Text)
import Data.Time.Conversion.Types.Date.Internal (DateString (..))
import Data.Time.Conversion.Types.Date.Internal qualified as Internal
import GHC.Generics (Generic)
import Optics.Core (Prism', prism)

-- | Date to use when reading a time string.
--
-- @since 0.1
data Date
  = -- | Corresponds to today.
    --
    -- @since 0.1
    DateToday
  | -- | Manual date string.
    --
    -- @since 0.1
    DateLiteral !DateString
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

-- | Parses a date.
--
-- @since 0.1
parseDate :: (MonadFail f) => Text -> f Date
parseDate "today" = pure DateToday
parseDate txt = DateLiteral <$> Internal.parseDateString txt

-- | @since 0.1
_DateToday :: Prism' Date ()
_DateToday =
  prism
    (const DateToday)
    ( \x -> case x of
        DateToday -> Right ()
        _ -> Left x
    )
{-# INLINE _DateToday #-}

-- | @since 0.1
_DateLiteral :: Prism' Date DateString
_DateLiteral =
  prism
    DateLiteral
    ( \x -> case x of
        DateLiteral s -> Right s
        _ -> Left x
    )
{-# INLINE _DateLiteral #-}
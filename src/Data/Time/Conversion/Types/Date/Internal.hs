{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.1
module Data.Time.Conversion.Types.Date.Internal
  ( DateString (..),
    unDateString,
    parseDateString,
  )
where

import Control.DeepSeq (NFData (..))
import Control.Monad ((>=>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)
import Optics.Core
  ( A_ReversedPrism,
    LabelOptic (labelOptic),
    ReversibleOptic (..),
    prism,
  )
import Text.Read qualified as TR

-- | Represents a date string in the format @YYYY-MM-DD@.
--
-- @since 0.1
newtype DateString = UnsafeDateString Text
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
instance
  (k ~ A_ReversedPrism, a ~ Text, b ~ Text) =>
  LabelOptic "unDateString" k DateString DateString a b
  where
  labelOptic = re $ prism getter setter
    where
      setter t = case parseDateString t of
        Nothing -> Left t
        Just d -> Right d
      getter (UnsafeDateString s) = s
  {-# INLINE labelOptic #-}

-- | @since 0.1
unDateString :: DateString -> Text
unDateString (UnsafeDateString t) = t

-- | Parses a date string in @YYYY-MM-DD@ form.
--
-- @since 0.1
parseDateString :: (MonadFail f) => Text -> f DateString
parseDateString txt = case T.split (== '-') txt of
  [y, m, d] | nonEmpty y && nonEmpty m && nonEmpty d ->
    case (readYear y, readMonth m, readDay d) of
      (Just _, Just _, Just _) -> pure $ UnsafeDateString txt
      (Nothing, _, _) ->
        fail $
          "Year should be an integer between 1900 and 3000, received " <> squote y
      (_, Nothing, _) ->
        fail $
          "Month should be an integer between 1 and 12, received " <> squote m
      (_, _, Nothing) ->
        fail $
          "Day should be an integer between 1 and 31, received " <> squote d
  _ ->
    fail $ "Date has the form YYYY-MM-DD, received " <> squote txt
  where
    readYear = readDecimal @Word16 4 1900 3000
    readMonth = readDecimal @Word8 2 1 12
    readDay = readDecimal @Word8 2 1 31

    readDecimal :: (Ord a, Read a) => Int -> a -> a -> Text -> Maybe a
    readDecimal len l u =
      (\t -> if T.length t == len then Just t else Nothing)
        >=> TR.readMaybe . T.unpack
        >=> \n ->
          if n >= l && n <= u
            then Just n
            else Nothing

    nonEmpty :: Text -> Bool
    nonEmpty = not . T.null . T.strip

    squote :: Text -> String
    squote t = T.unpack $ "'" <> t <> "'"

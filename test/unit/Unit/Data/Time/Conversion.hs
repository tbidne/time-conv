{-# LANGUAGE CPP #-}

module Unit.Data.Time.Conversion (tests) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Text qualified as T
import Data.Time.Conversion qualified as Conversion
import Data.Time.Conversion.Types.TZDatabase (TZDatabase (..))
import Data.Time.Conversion.Types.TimeFormat qualified as TimeFmt
import Data.Time.Conversion.Types.TimeReader (TimeReader (..))
import Data.Time.Format qualified as Format
import Hedgehog (Property, PropertyName)
import Hedgehog qualified as H
import Hedgehog.Internal.Property ((===))
import Props.Generators qualified as G
import Test.Tasty (TestName, TestTree, testGroup)
#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
import Test.Tasty.Hedgehog (testPropertyNamed)
#else
import Test.Tasty.Hedgehog (testProperty)
#endif

tests :: TestTree
tests =
  testGroup
    "Data.Time.Conversion"
    [ testDestSrcRoundtrips
    ]

testDestSrcRoundtrips :: TestTree
testDestSrcRoundtrips =
  testPropertyCompat "currTime == fromSource . toDest (date today)" "testDestSrcRoundtrips" $
    H.property $ do
      tzdb <- TZDatabaseLabel <$> H.forAll G.tzLabel

      currTime <- liftIO $ Conversion.readConvertTime Nothing Nothing
      H.annotateShow currTime
      currTimeDest <- liftIO $ Conversion.readConvertTime Nothing (Just tzdb)
      H.annotateShow currTime
      let currTimeDestStr = fmt currTimeDest
          timeReader =
            MkTimeReader
              { format = TimeFmt.hm,
                srcTZ = Just tzdb,
                today = True,
                timeString = T.pack currTimeDestStr
              }

      currTime' <- liftIO $ Conversion.readConvertTime (Just timeReader) Nothing
      H.annotateShow currTime'

      fmt currTime === fmt currTime'
  where
    fmt = Format.formatTime locale fmtOut
    fmtOut = "%H:%M"
    locale = Format.defaultTimeLocale

#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
testPropertyCompat = testPropertyNamed
#else
testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
testPropertyCompat tn _ = testProperty tn
#endif

{-# LANGUAGE CPP #-}

module Unit.Data.Time.Conversion (tests) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Data.Time.Conversion qualified as Conversion
import Data.Time.Conversion.Types.Date (Date (DateLiteral, DateToday))
import Data.Time.Conversion.Types.TZDatabase (TZDatabase (TZDatabaseLabel))
import Data.Time.Conversion.Types.TimeFormat qualified as TimeFmt
import Data.Time.Conversion.Types.TimeReader
  ( TimeReader
      ( MkTimeReader,
        date,
        format,
        srcTZ,
        timeString
      ),
  )
import Data.Time.Format qualified as Format
import Effects.Exception (catchAny)
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
import Unit.Utils qualified as Utils

tests :: TestTree
tests =
  testGroup
    "Data.Time.Conversion"
    [ testDestSrcRoundtrips,
      testDestSrcDateRoundtrips
    ]

testDestSrcRoundtrips :: TestTree
testDestSrcRoundtrips =
  testPropertyCompat "currTime == fromSource . toDest (date today)" "testDestSrcRoundtrips" $
    H.property $ do
      tzdb <- TZDatabaseLabel <$> H.forAll G.tzLabel

      currTime <- liftIO $ Conversion.readConvertTime Nothing Nothing
      H.annotateShow currTime
      currTimeDest <- liftIO $ Conversion.readConvertTime Nothing (Just tzdb)
      H.annotateShow currTimeDest
      let currTimeDestStr = fmt currTimeDest
          timeReader =
            MkTimeReader
              { format = TimeFmt.hm,
                srcTZ = Just tzdb,
                date = Just DateToday,
                timeString = T.pack currTimeDestStr
              }

      currTime' <-
        liftIO (Conversion.readConvertTime (Just timeReader) Nothing)
          `catchAny` \ex -> do
            H.annotateShow ex
            H.failure
      H.annotateShow currTime'

      fmt currTime === fmt currTime'
  where
    fmt = Format.formatTime locale fmtOut
    fmtOut = "%H:%M"
    locale = Format.defaultTimeLocale

testDestSrcDateRoundtrips :: TestTree
testDestSrcDateRoundtrips =
  testPropertyCompat "currTime == fromSource . toDest (date literal)" "testDestSrcDateRoundtrips" $
    H.property $ do
      tzdb <- TZDatabaseLabel <$> H.forAll G.tzLabel

      currTime <- liftIO $ Conversion.readConvertTime Nothing Nothing
      H.annotateShow currTime
      currTimeDest <- liftIO $ Conversion.readConvertTime Nothing (Just tzdb)
      H.annotateShow currTime
      let (currDateDestStr, currTimeDestStr) = case T.split (== ' ') (T.pack $ fmt currTimeDest) of
            [y, d] -> (y, d)
            _ ->
              error $
                mconcat
                  [ "Unit.Data.Time.Conversion: date should have format YYYY-MM-DD, ",
                    "received: '",
                    fmt currTimeDest,
                    "'"
                  ]
      H.annotate $ T.unpack currDateDestStr
      H.annotate $ T.unpack currTimeDestStr
      currDateDestStr' <- case Utils.runParseDateString currDateDestStr of
        Right s -> pure s
        Left err -> do
          H.annotate err
          H.failure

      H.annotateShow currDateDestStr'
      let timeReader =
            MkTimeReader
              { format = TimeFmt.hm,
                srcTZ = Just tzdb,
                date = Just $ DateLiteral currDateDestStr',
                timeString = currTimeDestStr
              }

      currTime' <-
        liftIO (Conversion.readConvertTime (Just timeReader) Nothing)
          `catchAny` \ex -> do
            H.annotateShow ex
            H.failure

      H.annotateShow currTime'

      fmt currTime === fmt currTime'
  where
    fmt = Format.formatTime locale fmtOut
    fmtOut = "%Y-%m-%d %H:%M"
    locale = Format.defaultTimeLocale

#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
testPropertyCompat = testPropertyNamed
#else
testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
testPropertyCompat tn _ = testProperty tn
#endif

{-# LANGUAGE CPP #-}

module Unit.Data.Time.Conversion (tests) where

import Data.Text qualified as T
import Data.Time.Conversion qualified as Conversion
import Data.Time.Conversion.Types.Date (Date (..))
import Data.Time.Conversion.Types.TZDatabase (TZDatabase (..))
import Data.Time.Conversion.Types.TimeFormat qualified as TimeFmt
import Data.Time.Conversion.Types.TimeReader (TimeReader (..))
import Data.Time.Format qualified as Format
import Effectful (Eff, IOE, MonadIO (liftIO), runEff)
import Effectful.Exception (catchAny)
import Effectful.FileSystem.FileReader.Static (FileReaderStatic, runFileReaderStaticIO)
import Effectful.FileSystem.PathReader.Static (PathReaderStatic, runPathReaderStaticIO)
import Effectful.Optparse.Static (OptparseStatic, runOptparseStaticIO)
import Effectful.Terminal.Static (TerminalStatic, runTerminalStaticIO)
import Effectful.Time.Dynamic (TimeDynamic, runTimeDynamicIO)
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

      currTime <- runIO $ Conversion.readConvertTime Nothing Nothing
      H.annotateShow currTime
      currTimeDest <- runIO $ Conversion.readConvertTime Nothing (Just tzdb)
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
        runIO (Conversion.readConvertTime (Just timeReader) Nothing)
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

      currTime <- runIO $ Conversion.readConvertTime Nothing Nothing
      H.annotateShow currTime
      currTimeDest <- runIO $ Conversion.readConvertTime Nothing (Just tzdb)
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
        runIO (Conversion.readConvertTime (Just timeReader) Nothing)
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

runIO ::
  (MonadIO m) =>
  Eff
    [ TimeDynamic,
      TerminalStatic,
      PathReaderStatic,
      OptparseStatic,
      FileReaderStatic,
      IOE
    ]
    a ->
  m a
runIO =
  liftIO
    . runEff
    . runFileReaderStaticIO
    . runOptparseStaticIO
    . runPathReaderStaticIO
    . runTerminalStaticIO
    . runTimeDynamicIO

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Functional test suite
--
-- @since 0.1
module Main (main) where

import Control.Exception (Exception (displayException))
import Control.Monad.Catch (try)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion.Types.Date qualified as Date
import Data.Time.Conversion.Types.Exception
  ( DateNoTimeStringException,
    ParseTZDatabaseException,
    ParseTimeException,
    SrcTZNoTimeStringException,
  )
import Data.Time.Format qualified as Format
import Effectful (Eff, runEff, type (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Environment.Static qualified as SysEnv
import Effectful.FileSystem.FileReader.Static (runFileReader)
import Effectful.FileSystem.PathReader.Static (runPathReader)
import Effectful.Internal.Monad (IOE)
import Effectful.Optparse.Static (runOptparse)
import Effectful.Terminal.Dynamic (Terminal (PutStrLn))
import Effectful.Time.Dynamic
  ( Time (GetMonotonicTime, GetSystemZonedTime),
    runTime,
  )
import FileSystem.OsPath (combineFilePaths)
import GHC.Stack (HasCallStack)
import Optics.Core (set', (^.))
import Params (TestParams (MkTestParams, args, configEnabled, mCurrentTime))
import Params qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@=?))
import TimeConv.Runner (runTimeConv)

-- | Runs functional tests.
--
-- @since 0.1
main :: IO ()
main =
  Tasty.defaultMain $
    testGroup
      "Functional tests"
      [ formatTests,
        formatOutputTests,
        srcTzTests,
        destTzTests,
        testNoArgs,
        testNoTimeString,
        testToday,
        testNoDateLiteral,
        testNoDateToday,
        tomlTests,
        testSrcTzNoTimeStr,
        testDateNoTimeStr
      ]

formatTests :: TestTree
formatTests =
  testGroup
    "Input Format"
    [ testFormatDefault,
      testFormatCustom,
      testFormatFails
    ]

testFormatDefault :: TestTree
testFormatDefault = testCase "Uses default parsing" $ do
  result <- captureTimeConvIO ["08:30", "-o", "%H:%M"]
  "08:30" @=? result

testFormatCustom :: TestTree
testFormatCustom = testCase "Uses custom parsing" $ do
  result <-
    captureTimeConvIO
      ["-f", "%Y-%m-%d %H:%M", "-o", "%Y-%m-%d %H:%M", "2022-06-15 08:30"]
  "2022-06-15 08:30" @=? result

testFormatFails :: TestTree
testFormatFails =
  testCase "Bad format fails" $
    assertException @ParseTimeException expected $
      captureTimeConvIO args
  where
    args = pureTZ <> ["-f", "%Y %H:%M", "08:30"]
    expected = "Could not parse time string '08:30' with format '%Y %H:%M'"

formatOutputTests :: TestTree
formatOutputTests =
  testGroup
    "Output Format"
    [ testFormatOutputCustom,
      testFormatOutputCustomTZOffset,
      testFormatOutputRfc822
    ]

testFormatOutputCustom :: TestTree
testFormatOutputCustom = testCase "Overrides input formatting" $ do
  result <- captureTimeConvIO $ pureTZ ++ ["-o", "%H:%M %Z", "08:30"]
  "08:30 UTC" @=? result

testFormatOutputCustomTZOffset :: TestTree
testFormatOutputCustomTZOffset = testCase desc $ do
  result <- captureTimeConvIO $ pureTZ ++ ["-o", "%H:%M %Z", "08:30"]
  "08:30 UTC" @=? result
  where
    desc = "Overrides input formatting tz offset"

testFormatOutputRfc822 :: TestTree
testFormatOutputRfc822 = testCase "Uses rfc822 output" $ do
  result <- captureTimeConvIO $ pureTZ ++ ["-o", "rfc822", "08:30"]
  "Thu,  1 Jan 1970 08:30:00 UTC" @=? result

srcTzTests :: TestTree
srcTzTests =
  testGroup
    "Source Timezone"
    [ testSrcTzDatabase,
      testSrcTzDatabaseCase,
      testSrcTzFails,
      testSrcTzDST,
      testSrcTzToday
    ]

testSrcTzDatabase :: TestTree
testSrcTzDatabase = testCase "Uses source timezone from tz database" $ do
  result <-
    captureTimeConvIO $
      pureDestTZ ++ ["-f", "%H:%M", "-s", "Europe/Paris", "08:30"]
  "Thu,  1 Jan 1970 07:30:00 UTC" @=? result

testSrcTzDatabaseCase :: TestTree
testSrcTzDatabaseCase = testCase desc $ do
  result <-
    captureTimeConvIO $
      pureDestTZ ++ ["-f", "%H:%M", "-s", "aMeRiCa/new_yoRk", "08:30"]
  "Thu,  1 Jan 1970 13:30:00 UTC" @=? result

  result2 <-
    captureTimeConvIO $
      pureDestTZ ++ ["-f", "%H:%M", "-s", "etc/utc", "08:30"]
  "Thu,  1 Jan 1970 08:30:00 UTC" @=? result2
  where
    desc = "Uses source timezone from tz database with 'wrong' case"

testSrcTzFails :: TestTree
testSrcTzFails = testCase "Bad source timezone fails" $ do
  assertException @ParseTZDatabaseException expected $ captureTimeConvIO args
  where
    args = pureDestTZ <> ["-s", "Europe/Pariss", "08:30"]
    expected =
      mconcat
        [ "Could not parse tz database name 'Europe/Pariss'. ",
          "Wanted a name like America/New_York."
        ]

testSrcTzDST :: TestTree
testSrcTzDST = testCase "Correctly converts src w/ DST" $ do
  result <- captureTimeConvIO $ pureDestTZ ++ argsDST
  "Mon, 10 Apr 2023 12:30:00 UTC" @=? result

  result2 <- captureTimeConvIO $ pureDestTZ ++ argsNoDST
  "Tue, 10 Jan 2023 13:30:00 UTC" @=? result2
  where
    argsDST = withDate ["--date", "2023-04-10"]
    argsNoDST = withDate ["--date", "2023-01-10"]
    withDate ds =
      ds
        ++ [ "-f",
             "%H:%M",
             "-s",
             "America/New_York",
             "08:30"
           ]

testSrcTzToday :: TestTree
testSrcTzToday = testCase "Correctly converts src w/ --date today" $ do
  resultUtcSrcDst <- captureTimeConvParamsIO $ mkSrcParams pureDestTZ
  "Tue, 18 Apr 2023 23:30:00 UTC" @=? resultUtcSrcDst

  resultNzstSrcDst <-
    captureTimeConvParamsIO $
      mkSrcParams ["-d", "Pacific/Auckland"]
  "Wed, 19 Apr 2023 11:30:00 NZST" @=? resultNzstSrcDst

  resultUtcDestDst <- captureTimeConvParamsIO $ mkDestParams pureDestTZ
  "Sun, 19 Feb 2023 00:30:00 UTC" @=? resultUtcDestDst

  resultNzstDestDst <-
    captureTimeConvParamsIO $
      mkDestParams ["-d", "Pacific/Auckland"]
  "Sun, 19 Feb 2023 13:30:00 NZDT" @=? resultNzstDestDst
  where
    mkSrcParams = mkParams currTimeSrcDst
    mkDestParams = mkParams currTimeDestDst

    mkParams :: String -> [String] -> TestParams
    mkParams currTime dest =
      MkTestParams
        { args =
            dest
              ++ [ "-f",
                   "%H:%M",
                   "--date",
                   "today",
                   "-s",
                   "America/New_York",
                   "19:30"
                 ],
          configEnabled = False,
          mCurrentTime = Just currTime
        }

    currTimeSrcDst = "2023-04-18 19:30 -0400"
    currTimeDestDst = "2023-02-18 19:30 -0500"

destTzTests :: TestTree
destTzTests =
  testGroup
    "Dest Timezone"
    [ testDestTzDatabase,
      testSrcDestTzDatabase,
      testDestTzFails
    ]

testDestTzDatabase :: TestTree
testDestTzDatabase = testCase "Uses dest timezone from tz database" $ do
  result <-
    captureTimeConvIO $
      pureSrcTZ ++ ["-f", "%H:%M", "-d", "Europe/Paris", "08:30"]
  "Thu,  1 Jan 1970 09:30:00 CET" @=? result

testSrcDestTzDatabase :: TestTree
testSrcDestTzDatabase = testCase "Uses src to dest" $ do
  result <-
    captureTimeConvIO
      ["-s", "America/New_York", "-d", "Europe/Paris", "08:30"]
  "Thu,  1 Jan 1970 14:30:00 CET" @=? result

testDestTzFails :: TestTree
testDestTzFails = testCase "Bad dest timezone fails" $ do
  assertException @ParseTZDatabaseException expected $ captureTimeConvIO args
  where
    args = pureSrcTZ <> ["-d", "Europe/Pariss", "08:30"]
    expected =
      mconcat
        [ "Could not parse tz database name 'Europe/Pariss'. ",
          "Wanted a name like America/New_York."
        ]

testNoArgs :: TestTree
testNoArgs = testCase "No args succeeds" $ do
  result <- captureTimeConvIO []
  assertBool ("Should be non-empty: " <> T.unpack result) $ (not . T.null) result

testNoTimeString :: TestTree
testNoTimeString = testCase "No time string gets current time" $ do
  resultsLocal <- captureTimeConvParamsIO $ mkParams []
  "Tue, 18 Apr 2023 19:30:00 -0400" @=? resultsLocal

  resultsUtc <- captureTimeConvParamsIO $ mkParams ["-d", "etc/utc"]
  "Tue, 18 Apr 2023 23:30:00 UTC" @=? resultsUtc

  resultsParis <- captureTimeConvParamsIO $ mkParams ["-d", "europe/paris"]
  "Wed, 19 Apr 2023 01:30:00 CEST" @=? resultsParis
  where
    mkParams args = set' #mCurrentTime (Just currTime) (Params.fromArgs args)

    currTime = "2023-04-18 19:30 -0400"

testToday :: TestTree
testToday = testCase "Today arg succeeds" $ do
  result <- captureTimeConvIO ["--date", "today", "16:30"]
  assertBool ("Should be non-empty: " <> T.unpack result) $ (not . T.null) result

testNoDateLiteral :: TestTree
testNoDateLiteral = testCase "Disables --date literal" $ do
  results <- captureTimeConvIO args
  "Thu,  1 Jan 1970 03:30:00 EST" @=? results
  where
    args =
      [ "-s",
        "Etc/Utc",
        "-d",
        "America/New_York",
        "--no-date",
        "--date",
        "2022-07-12",
        "08:30"
      ]

testNoDateToday :: TestTree
testNoDateToday = testCase "Disables --date today" $ do
  results <- captureTimeConvIO args
  "Thu,  1 Jan 1970 03:30:00 EST" @=? results
  where
    args =
      [ "-s",
        "Etc/Utc",
        "-d",
        "America/New_York",
        "--date",
        "today",
        "--no-date",
        "08:30"
      ]

tomlTests :: TestTree
tomlTests =
  testGroup
    "Toml"
    [ testTomlToday,
      testArgsOverridesTomlToday,
      testTomlAliases,
      testTomlNoDate
    ]

testTomlToday :: TestTree
testTomlToday = testCase "Uses toml 'today'" $ do
  results <- captureTimeConvParamsIO params
  dt <- Date.parseDateString results
  2021 @=? dt ^. #year
  where
    params =
      MkTestParams
        { args =
            [ "-c",
              "test" `cfp` "functional" `cfp` "today.toml",
              "-s",
              "Etc/Utc",
              "--format-out",
              "%Y-%m-%d",
              "13:30"
            ],
          mCurrentTime = Just "2021-04-18 19:30 -0400",
          configEnabled = True
        }

testArgsOverridesTomlToday :: TestTree
testArgsOverridesTomlToday = testCase "Args overrides toml's 'today'" $ do
  results <- captureTimeConvParamsIO params
  "Fri, 12 Jun 2020 09:30:00 -0400" @=? results
  where
    params =
      MkTestParams
        { args =
            [ "-c",
              "test" `cfp` "functional" `cfp` "today.toml",
              "-s",
              "Etc/Utc",
              "--date",
              "2020-06-12",
              "13:30"
            ],
          mCurrentTime = Just "2021-04-18 19:30 -0400",
          configEnabled = True
        }

testTomlAliases :: TestTree
testTomlAliases = testCase "Config aliases succeed" $ do
  resultsLA <- captureTimeConvParamsIO (withDest "la")
  "Tue, 12 Jul 2022 01:30:00 PDT" @=? resultsLA

  resultZagreb <- captureTimeConvParamsIO (withDest "zagreb")
  "Tue, 12 Jul 2022 10:30:00 CEST" @=? resultZagreb
  where
    withDest d =
      MkTestParams
        { args =
            [ "-c",
              "examples" `cfp` "config.toml",
              "-s",
              "Etc/Utc",
              "-d",
              d,
              "--date",
              "2022-07-12",
              "08:30"
            ],
          configEnabled = True,
          mCurrentTime = Nothing
        }

testTomlNoDate :: TestTree
testTomlNoDate = testCase "Disables toml 'today'" $ do
  results <- captureTimeConvParamsIO params
  "Thu,  1 Jan 1970 03:30:00 EST" @=? results
  where
    params =
      MkTestParams
        { args =
            [ "-c",
              "examples" `cfp` "config.toml",
              "-s",
              "Etc/Utc",
              "-d",
              "America/New_York",
              "--no-date",
              "08:30"
            ],
          configEnabled = True,
          mCurrentTime = Nothing
        }

testSrcTzNoTimeStr :: TestTree
testSrcTzNoTimeStr = testCase "Src w/o time string fails" $ do
  assertException @SrcTZNoTimeStringException expected $ captureTimeConvIO args
  where
    expected = "The --src-tz option was specified without required time string"
    args =
      [ "-s",
        "Etc/Utc"
      ]

testDateNoTimeStr :: TestTree
testDateNoTimeStr = testCase "Date w/o time string fails" $ do
  assertException @DateNoTimeStringException expected $ captureTimeConvIO args
  where
    expected = "The --date option was specified without required time string"
    args =
      [ "--date",
        "today"
      ]

assertException :: forall e a. (Exception e) => String -> IO a -> Assertion
assertException expected io = do
  try @_ @e io >>= \case
    Right _ -> assertFailure "Expected exception, received none"
    Left result -> do
      let result' = displayException result
      assertBool
        ("Encountered exception: " <> expected <> "\nReceived: " <> result')
        (startsWith expected result')

-- | Runs time-conv with default TestParams i.e.
--
-- - The given CLI args.
-- - Toml config disabled.
-- - No mocked time string.
captureTimeConvIO :: [String] -> IO Text
captureTimeConvIO = captureTimeConvParamsIO . Params.fromArgs

-- | General function for capturing time-conv output given TestParams.
captureTimeConvParamsIO :: TestParams -> IO Text
captureTimeConvParamsIO params = do
  termRef <- newIORef ""
  case params.mCurrentTime of
    Nothing ->
      runEff
        . runTime
        $ captureTimeConvConfigM termRef args'
    Just timeString ->
      runEff
        . runMockTime timeString
        $ captureTimeConvConfigM termRef args'

  readIORef termRef
  where
    args' =
      if params.configEnabled
        then params.args
        else "--no-config" : params.args

    -- Runs time-conv with the args, capturing terminal output.
    -- Toml configuration is not disabled, so take care that one of the following
    -- situations applies:
    --
    -- 1. Args includes @--config <path>@.
    -- 2. Args includes @--no-config@.
    -- 3. The XDG config dir is overridden to an expected path.
    --
    -- Otherwise, we may end up picking up a toml configuration at the real,
    -- expected XDG location, possibly inteferring with tests.
    --
    -- The function is polymorphic so that we can run it both without mocked
    -- time (IO) and with mocked time (MockTimeIO).
    --
    -- This function is intended to be used by captureTimeConvParamsIO only,
    -- hence the @where@ declaration.
    captureTimeConvConfigM :: IORef Text -> [String] -> Eff [Time, IOE] ()
    captureTimeConvConfigM termRef argList =
      SysEnv.runEnvironment
        . SysEnv.withArgs argList
        . runMockTerminal termRef
        . runFileReader
        . runPathReader
        . runOptparse
        $ runTimeConv

    runMockTerminal ::
      (HasCallStack, IOE :> es) =>
      IORef Text ->
      Eff (Terminal : es) a ->
      Eff es a
    runMockTerminal termRef = interpret_ $ \case
      PutStrLn s -> liftIO $ modifyIORef' termRef (T.pack s <>)
      _ -> error "runMockTerminal: undefined"

    runMockTime ::
      (HasCallStack, IOE :> es) =>
      String ->
      Eff (Time : es) a ->
      Eff es a
    runMockTime timeString = interpret_ $ \case
      GetSystemZonedTime ->
        liftIO $
          Format.parseTimeM
            True
            Format.defaultTimeLocale
            "%Y-%m-%d %H:%M %z"
            timeString
      GetMonotonicTime -> pure 0

-- when we want to ensure that nothing depends on local time.
pureTZ :: [String]
pureTZ = pureSrcTZ ++ pureDestTZ

pureSrcTZ :: [String]
pureSrcTZ = ["-s", "Etc/UTC"]

pureDestTZ :: [String]
pureDestTZ = ["-d", "Etc/UTC"]

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith (_ : _) [] = False
startsWith (x : xs) (y : ys)
  | x == y = startsWith xs ys
  | otherwise = False

cfp :: FilePath -> FilePath -> FilePath
cfp = combineFilePaths

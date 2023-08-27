{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Functional test suite
--
-- @since 0.1
module Main (main) where

import Control.Exception (Exception (displayException))
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
import Effectful (Eff, runEff, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Environment (Environment)
import Effectful.Environment qualified as SysEnv
import Effectful.Exception (try)
import Effectful.FileSystem.FileReader.Static (FileReaderStatic, runFileReaderStaticIO)
import Effectful.FileSystem.PathReader.Static (PathReaderStatic, runPathReaderStaticIO)
import Effectful.FileSystem.Utils (combineFilePaths)
import Effectful.IORef.Static (IORefStatic, modifyIORef', newIORef, readIORef, runIORefStaticIO)
import Effectful.Optparse.Static (OptparseStatic, runOptparseStaticIO)
import Effectful.Time.Dynamic
  ( TimeDynamic (GetMonotonicTime, GetSystemZonedTime),
    runTimeDynamicIO,
  )
import Optics.Core ((^.))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@=?))
import TimeConv.Runner (runTimeConvHandler)

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
  result <- captureTimeConvNoConfig ["08:30", "-o", "%H:%M"]
  "08:30" @=? result

testFormatCustom :: TestTree
testFormatCustom = testCase "Uses custom parsing" $ do
  result <- captureTimeConvNoConfig ["-f", "%Y-%m-%d %H:%M", "-o", "%Y-%m-%d %H:%M", "2022-06-15 08:30"]
  "2022-06-15 08:30" @=? result

testFormatFails :: TestTree
testFormatFails =
  testCase "Bad format fails" $
    assertException @ParseTimeException expected $
      captureTimeConvNoConfig args
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
  result <- captureTimeConvNoConfig $ pureTZ ++ ["-o", "%H:%M %Z", "08:30"]
  "08:30 UTC" @=? result

testFormatOutputCustomTZOffset :: TestTree
testFormatOutputCustomTZOffset = testCase "Overrides input formatting tz offset" $ do
  result <- captureTimeConvNoConfig $ pureTZ ++ ["-o", "%H:%M %Z", "08:30"]
  "08:30 UTC" @=? result

testFormatOutputRfc822 :: TestTree
testFormatOutputRfc822 = testCase "Uses rfc822 output" $ do
  result <- captureTimeConvNoConfig $ pureTZ ++ ["-o", "rfc822", "08:30"]
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
  result <- captureTimeConvNoConfig $ pureDestTZ ++ ["-f", "%H:%M", "-s", "Europe/Paris", "08:30"]
  "Thu,  1 Jan 1970 07:30:00 UTC" @=? result

testSrcTzDatabaseCase :: TestTree
testSrcTzDatabaseCase = testCase "Uses source timezone from tz database with 'wrong' case" $ do
  result <- captureTimeConvNoConfig $ pureDestTZ ++ ["-f", "%H:%M", "-s", "aMeRiCa/new_yoRk", "08:30"]
  "Thu,  1 Jan 1970 13:30:00 UTC" @=? result

  result2 <- captureTimeConvNoConfig $ pureDestTZ ++ ["-f", "%H:%M", "-s", "etc/utc", "08:30"]
  "Thu,  1 Jan 1970 08:30:00 UTC" @=? result2

testSrcTzFails :: TestTree
testSrcTzFails = testCase "Bad source timezone fails" $ do
  assertException @ParseTZDatabaseException expected $ captureTimeConvNoConfig args
  where
    args = pureDestTZ <> ["-s", "Europe/Pariss", "08:30"]
    expected = "Could not parse tz database name 'Europe/Pariss'. Wanted a name like America/New_York."

testSrcTzDST :: TestTree
testSrcTzDST = testCase "Correctly converts src w/ DST" $ do
  result <- captureTimeConvNoConfig $ pureDestTZ ++ argsDST
  "Mon, 10 Apr 2023 12:30:00 UTC" @=? result

  result2 <- captureTimeConvNoConfig $ pureDestTZ ++ argsNoDST
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
  resultUtcSrcDst <- captureTimeConvNoConfigMock currTimeSrcDst $ pureDestTZ ++ args
  "Tue, 18 Apr 2023 23:30:00 UTC" @=? resultUtcSrcDst

  resultNzstSrcDst <- captureTimeConvNoConfigMock currTimeSrcDst $ ["-d", "Pacific/Auckland"] ++ args
  "Wed, 19 Apr 2023 11:30:00 NZST" @=? resultNzstSrcDst

  resultUtcDestDst <- captureTimeConvNoConfigMock currTimeDestDst $ pureDestTZ ++ args
  "Sun, 19 Feb 2023 00:30:00 UTC" @=? resultUtcDestDst

  resultNzstDestDst <- captureTimeConvNoConfigMock currTimeDestDst $ ["-d", "Pacific/Auckland"] ++ args
  "Sun, 19 Feb 2023 13:30:00 NZDT" @=? resultNzstDestDst
  where
    currTimeSrcDst = "2023-04-18 19:30 -0400"
    currTimeDestDst = "2023-02-18 19:30 -0500"
    args =
      [ "-f",
        "%H:%M",
        "--date",
        "today",
        "-s",
        "America/New_York",
        "19:30"
      ]

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
  result <- captureTimeConvNoConfig $ pureSrcTZ ++ ["-f", "%H:%M", "-d", "Europe/Paris", "08:30"]
  "Thu,  1 Jan 1970 09:30:00 CET" @=? result

testSrcDestTzDatabase :: TestTree
testSrcDestTzDatabase = testCase "Uses src to dest" $ do
  result <- captureTimeConvNoConfig ["-s", "America/New_York", "-d", "Europe/Paris", "08:30"]
  "Thu,  1 Jan 1970 14:30:00 CET" @=? result

testDestTzFails :: TestTree
testDestTzFails = testCase "Bad dest timezone fails" $ do
  assertException @ParseTZDatabaseException expected $ captureTimeConvNoConfig args
  where
    args = pureSrcTZ <> ["-d", "Europe/Pariss", "08:30"]
    expected = "Could not parse tz database name 'Europe/Pariss'. Wanted a name like America/New_York."

testNoArgs :: TestTree
testNoArgs = testCase "No args succeeds" $ do
  result <- captureTimeConvNoConfig []
  assertBool ("Should be non-empty: " <> T.unpack result) $ (not . T.null) result

testNoTimeString :: TestTree
testNoTimeString = testCase "No time string gets current time" $ do
  resultsLocal <- captureTimeConvNoConfigMock currTime []
  "Tue, 18 Apr 2023 19:30:00 -0400" @=? resultsLocal

  resultsUtc <- captureTimeConvNoConfigMock currTime ["-d", "etc/utc"]
  "Tue, 18 Apr 2023 23:30:00 UTC" @=? resultsUtc

  resultsParis <- captureTimeConvNoConfigMock currTime ["-d", "europe/paris"]
  "Wed, 19 Apr 2023 01:30:00 CEST" @=? resultsParis
  where
    currTime = "2023-04-18 19:30 -0400"

testToday :: TestTree
testToday = testCase "Today arg succeeds" $ do
  result <- captureTimeConvNoConfig ["--date", "today", "16:30"]
  assertBool ("Should be non-empty: " <> T.unpack result) $ (not . T.null) result

testNoDateLiteral :: TestTree
testNoDateLiteral = testCase "Disables --date literal" $ do
  results <- captureTimeConvNoConfig args
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
  results <- captureTimeConvNoConfig args
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
  results <- captureTimeConvMock currTime args
  dt <- Date.parseDateString results
  2021 @=? dt ^. #year
  where
    args =
      [ "-c",
        "test" `combineFilePaths` "functional" `combineFilePaths` "today.toml",
        "-s",
        "Etc/Utc",
        "--format-out",
        "%Y-%m-%d",
        "13:30"
      ]
    currTime = "2021-04-18 19:30 -0400"

testArgsOverridesTomlToday :: TestTree
testArgsOverridesTomlToday = testCase "Args overrides toml's 'today'" $ do
  results <- captureTimeConvMock currTime args
  "Fri, 12 Jun 2020 09:30:00 -0400" @=? results
  where
    args =
      [ "-c",
        "test" `combineFilePaths` "functional" `combineFilePaths` "today.toml",
        "-s",
        "Etc/Utc",
        "--date",
        "2020-06-12",
        "13:30"
      ]
    currTime = "2021-04-18 19:30 -0400"

testTomlAliases :: TestTree
testTomlAliases = testCase "Config aliases succeed" $ do
  resultsLA <- captureTimeConv (withDest "la")
  "Tue, 12 Jul 2022 01:30:00 PDT" @=? resultsLA

  resultZagreb <- captureTimeConv (withDest "zagreb")
  "Tue, 12 Jul 2022 10:30:00 CEST" @=? resultZagreb
  where
    withDest d =
      [ "-c",
        "examples" `combineFilePaths` "config.toml",
        "-s",
        "Etc/Utc",
        "-d",
        d,
        "--date",
        "2022-07-12",
        "08:30"
      ]

testTomlNoDate :: TestTree
testTomlNoDate = testCase "Disables toml 'today'" $ do
  results <- captureTimeConvNoConfig args
  "Thu,  1 Jan 1970 03:30:00 EST" @=? results
  where
    args =
      [ "-c",
        "examples" `combineFilePaths` "config.toml",
        "-s",
        "Etc/Utc",
        "-d",
        "America/New_York",
        "--no-date",
        "08:30"
      ]

testSrcTzNoTimeStr :: TestTree
testSrcTzNoTimeStr = testCase "Src w/o time string fails" $ do
  assertException @SrcTZNoTimeStringException expected $ captureTimeConvNoConfig args
  where
    expected = "The --src-tz option was specified without required time string"
    args =
      [ "-s",
        "Etc/Utc"
      ]

testDateNoTimeStr :: TestTree
testDateNoTimeStr = testCase "Date w/o time string fails" $ do
  assertException @DateNoTimeStringException expected $ captureTimeConvNoConfig args
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

captureTimeConv :: [String] -> IO Text
captureTimeConv =
  runEff
    . SysEnv.runEnvironment
    . runFileReaderStaticIO
    . runIORefStaticIO
    . runOptparseStaticIO
    . runPathReaderStaticIO
    . runTimeDynamicIO
    . captureTimeConvEff

captureTimeConvNoConfig :: [String] -> IO Text
captureTimeConvNoConfig =
  runEff
    . SysEnv.runEnvironment
    . runFileReaderStaticIO
    . runIORefStaticIO
    . runOptparseStaticIO
    . runPathReaderStaticIO
    . runTimeDynamicIO
    . captureTimeConvNoConfigEff

runMockTimeIO :: String -> Eff (TimeDynamic : es) a -> Eff es a
runMockTimeIO timeStr = interpret $ \_ -> \case
  GetMonotonicTime -> pure 0
  GetSystemZonedTime ->
    unsafeEff_ $
      Format.parseTimeM
        True
        Format.defaultTimeLocale
        "%Y-%m-%d %H:%M %z"
        timeStr

captureTimeConvNoConfigMock :: String -> [String] -> IO Text
captureTimeConvNoConfigMock timeStr =
  runEff
    . SysEnv.runEnvironment
    . runFileReaderStaticIO
    . runIORefStaticIO
    . runOptparseStaticIO
    . runPathReaderStaticIO
    . runMockTimeIO timeStr
    . captureTimeConvNoConfigEff

captureTimeConvMock :: String -> [String] -> IO Text
captureTimeConvMock timeStr =
  runEff
    . SysEnv.runEnvironment
    . runFileReaderStaticIO
    . runIORefStaticIO
    . runOptparseStaticIO
    . runPathReaderStaticIO
    . runMockTimeIO timeStr
    . captureTimeConvEff

captureTimeConvNoConfigEff ::
  ( Environment :> es,
    FileReaderStatic :> es,
    IORefStatic :> es,
    OptparseStatic :> es,
    PathReaderStatic :> es,
    TimeDynamic :> es
  ) =>
  [String] ->
  Eff es Text
captureTimeConvNoConfigEff = captureTimeConvEff . (["--no-config"] ++)

captureTimeConvEff ::
  ( Environment :> es,
    FileReaderStatic :> es,
    IORefStatic :> es,
    OptparseStatic :> es,
    PathReaderStatic :> es,
    TimeDynamic :> es
  ) =>
  [String] ->
  Eff es Text
captureTimeConvEff argList = do
  output <- newIORef ""
  let handler txt = modifyIORef' output (txt <>)
  SysEnv.withArgs argList (runTimeConvHandler handler)
  readIORef output

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

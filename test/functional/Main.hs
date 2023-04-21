{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Functional test suite
--
-- @since 0.1
module Main (main) where

import Control.Exception (Exception (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion.Types.Exception
  ( ParseTZDatabaseException,
    ParseTimeException,
  )
import Data.Time.Format qualified as Format
import Effects.Exception (MonadCatch, MonadThrow, tryCS)
import Effects.FileSystem.FileReader (MonadFileReader)
import Effects.FileSystem.Path ((</>))
import Effects.FileSystem.PathReader (MonadPathReader)
import Effects.IORef (MonadIORef, modifyIORef', newIORef, readIORef)
import Effects.Optparse (MonadOptparse)
import Effects.System.Environment (MonadEnv)
import Effects.System.Environment qualified as SysEnv
import Effects.Time (MonadTime (..))
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
        testAliases
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
  result <- captureTimeConv ["08:30", "-o", "%H:%M"]
  "08:30" @=? result

testFormatCustom :: TestTree
testFormatCustom = testCase "Uses custom parsing" $ do
  result <- captureTimeConv ["-f", "%Y-%m-%d %H:%M", "-o", "%Y-%m-%d %H:%M", "2022-06-15 08:30"]
  "2022-06-15 08:30" @=? result

testFormatFails :: TestTree
testFormatFails =
  testCase "Bad format fails" $
    assertException @ParseTimeException expected $
      captureTimeConv args
  where
    args = pureTZ <> ["-f", "%Y %H:%M", "08:30"]
    expected = "Could not parse time string <08:30> with format <%Y %H:%M>"

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
  result <- captureTimeConv $ pureTZ ++ ["-o", "%H:%M %Z", "08:30"]
  "08:30 UTC" @=? result

testFormatOutputCustomTZOffset :: TestTree
testFormatOutputCustomTZOffset = testCase "Overrides input formatting tz offset" $ do
  result <- captureTimeConv $ pureTZ ++ ["-o", "%H:%M %Z", "08:30"]
  "08:30 UTC" @=? result

testFormatOutputRfc822 :: TestTree
testFormatOutputRfc822 = testCase "Uses rfc822 output" $ do
  result <- captureTimeConv $ pureTZ ++ ["-o", "rfc822", "08:30"]
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
  result <- captureTimeConv $ pureDestTZ ++ ["-f", "%H:%M", "-s", "Europe/Paris", "08:30"]
  "Thu,  1 Jan 1970 07:30:00 UTC" @=? result

testSrcTzDatabaseCase :: TestTree
testSrcTzDatabaseCase = testCase "Uses source timezone from tz database with 'wrong' case" $ do
  result <- captureTimeConv $ pureDestTZ ++ ["-f", "%H:%M", "-s", "aMeRiCa/new_yoRk", "08:30"]
  "Thu,  1 Jan 1970 13:30:00 UTC" @=? result

  result2 <- captureTimeConv $ pureDestTZ ++ ["-f", "%H:%M", "-s", "etc/utc", "08:30"]
  "Thu,  1 Jan 1970 08:30:00 UTC" @=? result2

testSrcTzFails :: TestTree
testSrcTzFails = testCase "Bad source timezone fails" $ do
  assertException @ParseTZDatabaseException expected $ captureTimeConv args
  where
    args = pureDestTZ <> ["-s", "Europe/Pariss", "08:30"]
    expected = "Could not parse tz database name <Europe/Pariss>. Wanted a name like America/New_York."

testSrcTzDST :: TestTree
testSrcTzDST = testCase "Correctly converts src w/ DST" $ do
  result <- captureTimeConv $ pureDestTZ ++ argsDST
  "Mon, 10 Apr 2023 12:30:00 UTC" @=? result

  result2 <- captureTimeConv $ pureDestTZ ++ argsNoDST
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
  resultUtcSrcDst <- captureTimeConvMock currTimeSrcDst $ pureDestTZ ++ args
  "Tue, 18 Apr 2023 23:30:00 UTC" @=? resultUtcSrcDst

  resultNzstSrcDst <- captureTimeConvMock currTimeSrcDst $ ["-d", "Pacific/Auckland"] ++ args
  "Wed, 19 Apr 2023 11:30:00 NZST" @=? resultNzstSrcDst

  resultUtcDestDst <- captureTimeConvMock currTimeDestDst $ pureDestTZ ++ args
  "Sun, 19 Feb 2023 00:30:00 UTC" @=? resultUtcDestDst

  resultNzstDestDst <- captureTimeConvMock currTimeDestDst $ ["-d", "Pacific/Auckland"] ++ args
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
  result <- captureTimeConv $ pureSrcTZ ++ ["-f", "%H:%M", "-d", "Europe/Paris", "08:30"]
  "Thu,  1 Jan 1970 09:30:00 CET" @=? result

testSrcDestTzDatabase :: TestTree
testSrcDestTzDatabase = testCase "Uses src to dest" $ do
  result <- captureTimeConv ["-s", "America/New_York", "-d", "Europe/Paris", "08:30"]
  "Thu,  1 Jan 1970 14:30:00 CET" @=? result

testDestTzFails :: TestTree
testDestTzFails = testCase "Bad dest timezone fails" $ do
  assertException @ParseTZDatabaseException expected $ captureTimeConv args
  where
    args = pureSrcTZ <> ["-d", "Europe/Pariss", "08:30"]
    expected = "Could not parse tz database name <Europe/Pariss>. Wanted a name like America/New_York."

testNoArgs :: TestTree
testNoArgs = testCase "No args succeeds" $ do
  result <- captureTimeConv []
  assertBool ("Should be non-empty: " <> T.unpack result) $ (not . T.null) result

testNoTimeString :: TestTree
testNoTimeString = testCase "No time string gets current time" $ do
  resultsLocal <- captureTimeConvMock currTime []
  "Tue, 18 Apr 2023 19:30:00 -0400" @=? resultsLocal

  resultsUtc <- captureTimeConvMock currTime ["-d", "etc/utc"]
  "Tue, 18 Apr 2023 23:30:00 UTC" @=? resultsUtc

  resultsParis <- captureTimeConvMock currTime ["-d", "europe/paris"]
  "Wed, 19 Apr 2023 01:30:00 CEST" @=? resultsParis
  where
    currTime = "2023-04-18 19:30 -0400"

testToday :: TestTree
testToday = testCase "Today arg succeeds" $ do
  result <- captureTimeConv ["--date", "today"]
  assertBool ("Should be non-empty: " <> T.unpack result) $ (not . T.null) result

testAliases :: TestTree
testAliases = testCase "Config aliases succeed" $ do
  resultsLA <- captureTimeConv (withDest "la")
  "Tue, 12 Jul 2022 01:30:00 PDT" @=? resultsLA

  resultZagreb <- captureTimeConv (withDest "zagreb")
  "Tue, 12 Jul 2022 10:30:00 CEST" @=? resultZagreb
  where
    withDest d =
      [ "-c",
        "examples" </> "config.toml",
        "-s",
        "Etc/Utc",
        "-d",
        d,
        "--date",
        "2022-07-12",
        "08:30"
      ]

assertException :: forall e a. (Exception e) => String -> IO a -> Assertion
assertException expected io = do
  tryCS @_ @e io >>= \case
    Right _ -> assertFailure "Expected exception, received none"
    Left result -> do
      let result' = displayException result
      assertBool
        ("Encountered exception: " <> expected <> "\nReceived: " <> result')
        (startsWith expected result')

captureTimeConv :: [String] -> IO Text
captureTimeConv = captureTimeConvM

newtype MockTimeM m a = MkMockTimeM (m a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadEnv,
      MonadFileReader,
      MonadIO,
      MonadIORef,
      MonadOptparse,
      MonadPathReader,
      MonadThrow
    )
    via m

instance MonadTrans MockTimeM where
  lift = MkMockTimeM

type MockTimeIO = MockTimeM (ReaderT String IO)

runMockTimeIO :: MockTimeIO a -> String -> IO a
runMockTimeIO (MkMockTimeM rdr) = runReaderT rdr

usingMockTimeIO :: String -> MockTimeIO a -> IO a
usingMockTimeIO = flip runMockTimeIO

instance MonadTime MockTimeIO where
  getSystemZonedTime = do
    str <- lift ask
    liftIO $
      Format.parseTimeM
        True
        Format.defaultTimeLocale
        "%Y-%m-%d %H:%M %z"
        str
  getMonotonicTime = pure 0

captureTimeConvMock :: String -> [String] -> IO Text
captureTimeConvMock timeStr = usingMockTimeIO timeStr . captureTimeConvM

captureTimeConvM ::
  ( MonadEnv m,
    MonadCatch m,
    MonadFileReader m,
    MonadIORef m,
    MonadOptparse m,
    MonadPathReader m,
    MonadTime m
  ) =>
  [String] ->
  m Text
captureTimeConvM argList = do
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

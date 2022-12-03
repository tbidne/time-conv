{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Functional test suite
--
-- @since 0.1
module Main (main) where

import Control.Exception (Exception (..))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion (ParseTZDatabaseException, ParseTimeException)
import Effects.MonadCallStack (try)
import System.Environment qualified as SysEnv
import System.Environment.Guard (ExpectEnv (..), guardOrElse')
import Test.Tasty (TestTree, testGroup)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@=?))
import TimeConv.Runner (runTimeConvHandler)

-- | Runs functional tests.
--
-- @since 0.1
main :: IO ()
main = do
  allTests <-
    guardOrElse'
      "FUNCTIONAL_IMPURE"
      ExpectEnvSet
      (pure pureAndImpureTests)
      (pure pureTests)

  Tasty.defaultMain $ testGroup "Functional tests" allTests
  where
    pureTests =
      [ formatTests,
        formatOutputTests,
        srcTzTests,
        destTzTests
      ]
    pureAndImpureTests =
      [ testGroup "Pure tests" pureTests,
        impureTests
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
    -- NOTE: Despite our library throwing AnnotatedExceptions, the below works
    -- because catch "sees through" the AnnotatedException to the underlying
    -- exception, in this case, ParseTimeException. So we can capture the underlying
    -- exception, throwing away all of the extraneous data (i.e. callstacks).
    --
    -- In real code we actually want these data, so we should either catch
    -- SomeException or (AnnotatedException E) for some E.
    assertException @ParseTimeException expected $
      captureTimeConv args
  where
    args = pureTZ <> ["-f", "%Y %H:%M", "08:30"]
    expected = "Could not parse time string <08:30 UTC> with format <%Y %H:%M %Z>"

formatOutputTests :: TestTree
formatOutputTests =
  testGroup
    "Output Format"
    [ testFormatOutputCustom,
      testFormatOutputRfc822
    ]

testFormatOutputCustom :: TestTree
testFormatOutputCustom = testCase "Overrides input formatting" $ do
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
      testSrcTzFails
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

impureTests :: TestTree
impureTests =
  testGroup
    "Impure Tests"
    [ testNoArgs,
      testToday
    ]

testNoArgs :: TestTree
testNoArgs = testCase "No args succeeds" $ do
  result <- captureTimeConv []
  assertBool ("Should be non-empty: " <> T.unpack result) $ (not . T.null) result

testToday :: TestTree
testToday = testCase "Today arg succeeds" $ do
  result <- captureTimeConv ["-t"]
  assertBool ("Should be non-empty: " <> T.unpack result) $ (not . T.null) result

assertException :: forall e a. Exception e => String -> IO a -> Assertion
assertException expected io = do
  try @e io >>= \case
    Right _ -> assertFailure "Expected exception, received none"
    Left result -> do
      let result' = displayException result
      assertBool
        ("Encountered exception: " <> expected <> "\nReceived: " <> result')
        (startsWith expected result')

captureTimeConv :: [String] -> IO Text
captureTimeConv argList = do
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

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith (_ : _) [] = False
startsWith (x : xs) (y : ys)
  | x == y = startsWith xs ys
  | otherwise = False

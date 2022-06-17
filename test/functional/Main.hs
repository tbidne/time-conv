{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Functional test suite
--
-- @since 0.1
module Main (main) where

import Control.Exception (Exception (..))
import Data.Functor (($>))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion (ParseTZDatabaseException, ParseTimeException)
import Data.Time.Conversion.Utils qualified as Utils
import System.Environment qualified as SysEnv
import System.Environment.Guard (guardSet)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@=?))
import TimeConv.Runner (runTimeConvHandler)

-- | Runs functional tests.
--
-- @since 0.1
main :: IO ()
main = do
  mimpureTests <- guardSet "FUNCTIONAL_IMPURE" $ pure impureTests
  let allTests = case mimpureTests of
        Nothing -> pureTests
        Just impureTests' ->
          [ testGroup "Pure tests" pureTests,
            impureTests'
          ]
  Tasty.defaultMain $
    testGroup "Functional tests" allTests
  where
    pureTests =
      [ formatTests,
        formatOutputTests,
        srcTzTests,
        destTzTests
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
  result <- captureTimeConv ["08:30"]
  "08:30" @=? result

testFormatCustom :: TestTree
testFormatCustom = testCase "Uses custom parsing" $ do
  result <- captureTimeConv ["-f", "%Y-%m-%d %H:%M", "2022-06-15 08:30"]
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
      testFormatOutputFull
    ]

testFormatOutputCustom :: TestTree
testFormatOutputCustom = testCase "Overrides input formatting" $ do
  result <- captureTimeConv $ pureTZ ++ ["-o", "%H:%M %Z", "08:30"]
  "08:30 UTC" @=? result

testFormatOutputFull :: TestTree
testFormatOutputFull = testCase "Uses rfc822 output" $ do
  result <- captureTimeConv $ pureTZ ++ ["-o", "rfc822", "08:30"]
  "Thu,  1 Jan 1970 08:30:00 UTC" @=? result

srcTzTests :: TestTree
srcTzTests =
  testGroup
    "Source Timezone"
    [ testSrcTzLiteral,
      testSrcTzDatabase,
      testSrcTzFails
    ]

testSrcTzLiteral :: TestTree
testSrcTzLiteral = testCase "Uses source timezone from literal" $ do
  result <- captureTimeConv $ pureTZ ++ ["-f", "%H:%M %Z", "08:30 EST"]
  "13:30 UTC" @=? result

testSrcTzDatabase :: TestTree
testSrcTzDatabase = testCase "Uses source timezone from tz database" $ do
  result <- captureTimeConv $ pureDestTZ ++ ["-f", "%H:%M %Z", "-s", "Europe/Paris", "08:30 EST"]
  "07:30 UTC" @=? result

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
  result <- captureTimeConv $ pureSrcTZ ++ ["-f", "%H:%M %Z", "-d", "Europe/Paris", "08:30 EST"]
  "14:30 CET" @=? result

testSrcDestTzDatabase :: TestTree
testSrcDestTzDatabase = testCase "Uses src to dest" $ do
  result <- captureTimeConv ["-s", "America/New_York", "-d", "Europe/Paris", "08:30"]
  "14:30" @=? result

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
  result <- (io $> Nothing) `catchSync` (pure . Just)
  Just expected @=? fmap displayException result
  where
    catchSync = Utils.catchSync @e

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
pureSrcTZ = ["-s", "literal"]

pureDestTZ :: [String]
pureDestTZ = ["-d", "Etc/UTC"]

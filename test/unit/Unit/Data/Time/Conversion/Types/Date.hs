{-# LANGUAGE CPP #-}

module Unit.Data.Time.Conversion.Types.Date (tests) where

import Data.Text (Text)
import Data.Time.Conversion.Types.Date
import Data.Time.Conversion.Types.Date qualified as Date
import Data.Time.Conversion.Types.Date.Internal
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

tests :: TestTree
tests =
  testGroup
    "Data.Time.Conversion.Types.Date"
    [ testParseDateTodaySuccess,
      testParseDateLiteralSuccess,
      testParseDateLiteralBadYear,
      testParseDateLiteralBadMonth,
      testParseDateLiteralBadDay,
      testParseDateLiteralBadFormat
    ]

testParseDateTodaySuccess :: TestTree
testParseDateTodaySuccess = testCase "parseDateString today success" $ do
  Right DateToday @=? runParseDate "today"

testParseDateLiteralSuccess :: TestTree
testParseDateLiteralSuccess = testCase "parseDateString literal success" $ do
  Right (DateLiteral (UnsafeDateString "2022-03-12")) @=? runParseDate "2022-03-12"
  Right (DateLiteral (UnsafeDateString "2010-01-31")) @=? runParseDate "2010-01-31"
  Right (DateLiteral (UnsafeDateString "1900-02-29")) @=? runParseDate "1900-02-29"
  Right (DateLiteral (UnsafeDateString "2099-08-25")) @=? runParseDate "2099-08-25"

testParseDateLiteralBadYear :: TestTree
testParseDateLiteralBadYear = testCase "parseDateString bad year fails" $ do
  Left "Year should be an integer between 1900 and 3000, received '1899'" @=? runParseDate "1899-03-12"
  Left "Year should be an integer between 1900 and 3000, received '3001'" @=? runParseDate "3001-03-12"

testParseDateLiteralBadMonth :: TestTree
testParseDateLiteralBadMonth = testCase "parseDateString bad month fails" $ do
  Left "Month should be an integer between 1 and 12, received '00'" @=? runParseDate "2000-00-12"
  Left "Month should be an integer between 1 and 12, received '13'" @=? runParseDate "2000-13-12"
  Left "Month should be an integer between 1 and 12, received 'x'" @=? runParseDate "2000-x-12"

testParseDateLiteralBadDay :: TestTree
testParseDateLiteralBadDay = testCase "parseDateString bad day fails" $ do
  Left "Day should be an integer between 1 and 31, received '00'" @=? runParseDate "2000-05-00"
  Left "Day should be an integer between 1 and 31, received '32'" @=? runParseDate "2000-05-32"
  Left "Day should be an integer between 1 and 31, received 'y'" @=? runParseDate "2000-05-y"

testParseDateLiteralBadFormat :: TestTree
testParseDateLiteralBadFormat = testCase "parseDateString bad format fails" $ do
  Left "Date has the form YYYY-MM-DD, received '-05-00'" @=? runParseDate "-05-00"
  Left "Date has the form YYYY-MM-DD, received '2000-05-'" @=? runParseDate "2000-05-"
  Left "Date has the form YYYY-MM-DD, received '7'" @=? runParseDate "7"
  Left "Date has the form YYYY-MM-DD, received 'cat'" @=? runParseDate "cat"
  Left "Date has the form YYYY-MM-DD, received ''" @=? runParseDate ""

runParseDate :: Text -> Either String Date
runParseDate = unEString . Date.parseDate

newtype EString a = MkEString (Either String a)
  deriving stock (Eq, Show)
  deriving (Applicative, Functor, Monad) via (Either String)

unEString :: EString a -> Either String a
unEString (MkEString x) = x

instance MonadFail EString where
  fail = MkEString . Left

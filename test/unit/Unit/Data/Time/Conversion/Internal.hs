{-# LANGUAGE CPP #-}

module Unit.Data.Time.Conversion.Internal (tests) where

import Control.DeepSeq (force)
import Control.Exception
  ( AsyncException (..),
    Exception (..),
    SomeException,
    catch,
    evaluate,
    throwIO,
  )
import Control.Monad (void)
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text.Encoding.Error qualified as TError
import Data.Time.Conversion.Internal qualified as Internal
import Hedgehog
  ( Property,
    PropertyName,
    failure,
    forAll,
    property,
    withTests,
  )
import Props.Generators qualified as G
import Props.MaxRuns (MaxRuns (..))
import Test.Tasty (TestName, TestTree, askOption, testGroup)
#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
import Test.Tasty.Hedgehog (testPropertyNamed)
#else
import Test.Tasty.Hedgehog (testProperty)
#endif
import Test.Tasty.HUnit (assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "Data.Time.Conversion.Internal"
    [ tzTests,
      exceptionTests
    ]

tzTests :: TestTree
tzTests =
  testGroup
    "TZ database tests"
    [ tzNameMapParses,
      tzNameRandomCase
    ]

tzNameMapParses :: TestTree
tzNameMapParses =
  testCase "tzNameMap parses without errors" $
    void $
      evaluate (force mp)
  where
    mp = Internal.tzLowerNameLabelMapWith TError.strictDecode

tzNameRandomCase :: TestTree
tzNameRandomCase = askOption $ \(MkMaxRuns limit) ->
  testPropertyCompat "tzNameToTZLabel handles random case" "tzNameRandomCase" $
    withTests limit $
      property $ do
        txt <- forAll G.tzText
        case Internal.tzNameToTZLabel txt of
          Just _ -> pure ()
          Nothing -> failure

exceptionTests :: TestTree
exceptionTests =
  testGroup
    "Exception tests"
    [ catchesSyncExceptions,
      doesNotCatchAsyncExceptions
    ]

newtype TestException = MkTestException Text
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

catchesSyncExceptions :: TestTree
catchesSyncExceptions =
  testCase "Catches synchronous exception" $
    Internal.catchSync @SomeException throwEx (const $ pure ())
  where
    throwEx = throwIO $ MkTestException "should have caught this"

doesNotCatchAsyncExceptions :: TestTree
doesNotCatchAsyncExceptions = testCase "Does not catch asynchronous exception" $ do
  -- result is true iff shouldThrow does in fact throw (and enters catch handler)
  res <- (shouldThrow $> False) `catch` (\(_ :: SomeException) -> pure True)
  assertBool "Async exception should have been thrown" res
  where
    throwEx = throwIO UserInterrupt
    shouldThrow = Internal.catchSync @SomeException throwEx (const $ pure ())

#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
testPropertyCompat = testPropertyNamed
#else
testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
testPropertyCompat tn _ = testProperty tn
#endif

module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Unit.Data.Time.Conversion.Internal (tests)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Unit tests"
      [ tests
      ]

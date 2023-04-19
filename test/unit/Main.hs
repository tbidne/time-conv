module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Unit.Data.Time.Conversion qualified as C
import Unit.Data.Time.Conversion.Internal qualified as CInternal

main :: IO ()
main =
  defaultMain $
    testGroup
      "Unit tests"
      [ C.tests,
        CInternal.tests
      ]

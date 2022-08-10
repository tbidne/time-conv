module Main (main) where

import Data.Data (Proxy (..))
import Props.MaxRuns (MaxRuns)
import Test.Tasty
import Test.Tasty.Options (OptionDescription (..))
import Unit.Data.Time.Conversion.Internal (tests)

main :: IO ()
main = do
  let options = includingOptions [Option @MaxRuns Proxy]
      ingredients = options : defaultIngredients
  defaultMainWithIngredients ingredients $
    testGroup
      "Unit tests"
      [ tests
      ]

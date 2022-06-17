-- | Main module.
--
-- @since 0.1
module Main (main) where

import Control.Exception (Exception (..))
import Control.Exception.Base (SomeException)
import Data.Time.Conversion.Utils qualified as Utils
import TimeConv.Runner (runTimeConv)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  runTimeConv `Utils.catchSync` \(e :: SomeException) ->
    putStrLn $ displayException e

-- | Main module.
--
-- @since 0.1
module Main (main) where

import Args (parserInfo)
import Control.Exception (Exception (..))
import Control.Exception.Base (SomeException)
import Data.Time.Conversion (TimeBuilder)
import Data.Time.Conversion qualified as Conv
import Data.Time.Conversion.Utils qualified as Utils
import Options.Applicative qualified as OApp

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  builder <- OApp.execParser parserInfo
  runConv builder `Utils.catchSync` \(e :: SomeException) ->
    putStrLn $ displayException e

runConv :: TimeBuilder -> IO ()
runConv builder = do
  time <- Conv.readConvertTime builder
  putStrLn $ Conv.formatTimeBuilder builder time

-- | Main module.
--
-- @since 0.1
module Main (main) where

import Args (Args (..), parserInfo)
import Control.Exception (Exception (..))
import Control.Exception.Base (SomeException)
import Data.Time.Conversion qualified as Conv
import Data.Time.Conversion.Utils qualified as Utils
import Options.Applicative qualified as OApp

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  args <- OApp.execParser parserInfo
  runConv args `Utils.catchSync` \(e :: SomeException) ->
    putStrLn $ displayException e

runConv :: Args -> IO ()
runConv MkArgs {builder, timeString} = do
  time <- Conv.readConvertTime builder timeString
  print time

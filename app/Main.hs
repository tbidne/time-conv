-- | Main module.
--
-- @since 0.1
module Main (main) where

import Args (Args, argsToBuilder, parserInfo)
import Control.Exception (Exception (..))
import Control.Exception.Base (SomeException)
import Data.Maybe (fromMaybe)
import Data.Time.Conversion qualified as Conv
import Data.Time.Conversion.Utils qualified as Utils
import Data.Time.Format qualified as Format
import Optics.Core ((^.))
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
runConv args = do
  time <- Conv.readConvertTime builder
  putStrLn $
    Format.formatTime
      (builder ^. #locale)
      formatStr
      time
  where
    builder = args ^. argsToBuilder
    format = fromMaybe (builder ^. #format) (args ^. #formatOut)
    formatStr = format ^. Conv.timeFormatStringIso

-- | Internal library for testing.
--
-- @since 0.1
module TimeConv.Runner
  ( runTimeConv,
    runTimeConvHandler,
  )
where

import Control.Exception (SomeException, displayException)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion qualified as Conv
import Data.Time.Conversion.Internal qualified as Internal
import Data.Time.Conversion.Types (_MkTimeFormat)
import Data.Time.Conversion.Utils qualified as Utils
import Data.Time.Format qualified as Format
import Optics.Core ((^.))
import Options.Applicative qualified as OApp
import System.Exit (exitFailure)
import TimeConv.Args (Args, argsToBuilder, parserInfo)

-- | 'runTimeConvHandler' that prints the result.
--
-- @since 0.1
runTimeConv :: IO ()
runTimeConv = do
  args <- OApp.execParser parserInfo
  -- catch needs to be _within_ this call (i.e. not applied to the execParser
  -- function) otherwise e.g. we catch the --help "exception".
  runWithArgs (putStrLn . T.unpack) args
    `Internal.catchSync` \(e :: SomeException) -> do
      _ <- putStrLn $ displayException e
      exitFailure

-- | Runs time-conv and applies the given handler.
--
-- @since 0.1
runTimeConvHandler :: (Text -> IO a) -> IO a
runTimeConvHandler handler = do
  args <- OApp.execParser parserInfo
  runWithArgs handler args

-- | Runs time-conv and applies the given handler.
--
-- @since 0.1
runWithArgs :: (Text -> IO a) -> Args -> IO a
runWithArgs handler args = do
  let (mtimeReader, destTZ, formatOut) = args ^. argsToBuilder
      formatStr = T.unpack $ formatOut ^. _MkTimeFormat

  readAndHandle mtimeReader destTZ formatStr
  where
    readAndHandle tr d fmt = do
      time <- Conv.readConvertTime tr d
      let result = T.pack $ Format.formatTime Utils.timeLocaleAllZones fmt time
      handler result

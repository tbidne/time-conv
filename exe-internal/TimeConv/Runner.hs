-- | Internal library for testing.
--
-- @since 0.1
module TimeConv.Runner
  ( runTimeConv,
    runTimeConvHandler,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion qualified as Conv
import Data.Time.Format qualified as Format
import Optics.Core ((^.))
import Options.Applicative qualified as OApp
import TimeConv.Args (argsToBuilder, parserInfo)

-- | 'runTimeConvHandler' that prints the result.
--
-- @since 0.1
runTimeConv :: IO ()
runTimeConv = runTimeConvHandler (putStrLn . T.unpack)

-- | Runs time-conv and applies the given handler.
--
-- @since 0.1
runTimeConvHandler :: (Text -> IO a) -> IO a
runTimeConvHandler handler = do
  args <- OApp.execParser parserInfo
  let builder = args ^. argsToBuilder
      format = fromMaybe (builder ^. #format) (args ^. #formatOut)
      formatStr = format ^. Conv.timeFormatStringIso

  time <- Conv.readConvertTime builder

  let result = T.pack $ Format.formatTime (builder ^. #locale) formatStr time
  handler result

-- | Internal library for testing.
--
-- @since 0.1
module TimeConv.Runner
  ( runTimeConv,
    runTimeConvHandler,
  )
where

import Control.Exception (displayException)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion qualified as Conv
import Data.Time.Format qualified as Format
import Effects.Exception (MonadCatch, catchAny, exitFailure)
import Effects.Optparse (MonadOptparse (execParser))
import Effects.System.Terminal (MonadTerminal)
import Effects.System.Terminal qualified as T
import Effects.Time (MonadTime)
import Optics.Core ((^.))
import TimeConv.Args (Args, argsToBuilder, parserInfo)

-- | 'runTimeConvHandler' that prints the result.
--
-- @since 0.1
runTimeConv ::
  ( MonadCatch m,
    MonadOptparse m,
    MonadTerminal m,
    MonadTime m
  ) =>
  m ()
runTimeConv = do
  args <- execParser parserInfo
  -- catch needs to be _within_ this call (i.e. not applied to the execParser
  -- function) otherwise e.g. we catch the --help "exception".
  runWithArgs T.putTextLn args
    `catchAny` \e -> do
      _ <- T.putStrLn $ displayException e
      exitFailure

-- | Runs time-conv and applies the given handler.
--
-- @since 0.1
runTimeConvHandler ::
  ( MonadCatch m,
    MonadOptparse m,
    MonadTime m
  ) =>
  (Text -> m a) ->
  m a
runTimeConvHandler handler = do
  args <- execParser parserInfo
  runWithArgs handler args

-- | Runs time-conv and applies the given handler.
--
-- @since 0.1
runWithArgs :: (MonadCatch m, MonadTime m) => (Text -> m a) -> Args -> m a
runWithArgs handler args = do
  let (mtimeReader, destTZ, formatOut) = args ^. argsToBuilder
      formatStr = T.unpack $ formatOut ^. #unTimeFormat

  readAndHandle mtimeReader destTZ formatStr
  where
    readAndHandle tr d fmt = do
      time <- Conv.readConvertTime tr d
      let result = T.pack $ Format.formatTime locale fmt time
      handler result
    -- NOTE: It seems that the locale's timezone info is not used when
    -- formatting the output, so we do not have to worry about including
    -- extra tz info here.
    locale = Format.defaultTimeLocale

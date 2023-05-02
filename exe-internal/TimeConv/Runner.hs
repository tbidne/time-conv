-- | Internal library for testing.
--
-- @since 0.1
module TimeConv.Runner
  ( runTimeConv,
    runTimeConvHandler,
  )
where

import Control.Exception (displayException)
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Maybe.Optics (_Just)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion qualified as Conv
import Data.Time.Conversion.Types.Date (Date (..))
import Data.Time.Conversion.Types.TZDatabase (TZDatabase, _TZDatabaseText)
import Data.Time.Conversion.Types.TimeReader (TimeReader (..))
import Data.Time.Format qualified as Format
import Effects.Exception (MonadCatch, MonadThrow, catchAny, exitFailure, throwM)
import Effects.FileSystem.FileReader (MonadFileReader, readFileUtf8ThrowM)
import Effects.FileSystem.Path ((</>))
import Effects.FileSystem.PathReader (MonadPathReader (doesFileExist), getXdgConfig)
import Effects.Optparse (MonadOptparse (execParser))
import Effects.System.Terminal (MonadTerminal)
import Effects.System.Terminal qualified as T
import Effects.Time (MonadTime)
import Optics.Core (over', (%), (^.))
import TOML qualified
import TimeConv.Args (Args, argsToBuilder, parserInfo)
import TimeConv.Toml (Toml)

-- | 'runTimeConvHandler' that prints the result.
--
-- @since 0.1
runTimeConv ::
  ( MonadCatch m,
    MonadFileReader m,
    MonadOptparse m,
    MonadPathReader m,
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
    MonadFileReader m,
    MonadOptparse m,
    MonadPathReader m,
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
runWithArgs ::
  ( MonadCatch m,
    MonadFileReader m,
    MonadPathReader m,
    MonadTime m
  ) =>
  (Text -> m a) ->
  Args ->
  m a
runWithArgs handler args = do
  let (mtimeReader, destTZ, formatOut) = args ^. argsToBuilder
      formatStr = T.unpack $ formatOut ^. #unTimeFormat

  -- if the toml is set, and aliases exist, update
  (mtimeReader', destTZ') <-
    if args ^. #noConfig
      then pure (mtimeReader, destTZ)
      else updateFromToml (args ^. #config) (mtimeReader, destTZ)

  readAndHandle mtimeReader' destTZ' formatStr
  where
    readAndHandle tr d fmt = do
      time <- Conv.readConvertTime tr d
      let result = T.pack $ Format.formatTime locale fmt time
      handler result
    -- NOTE: It seems that the locale's timezone info is not used when
    -- formatting the output, so we do not have to worry about including
    -- extra tz info here.
    locale = Format.defaultTimeLocale

updateFromToml ::
  ( MonadFileReader m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  Maybe FilePath ->
  (Maybe TimeReader, Maybe TZDatabase) ->
  m (Maybe TimeReader, Maybe TZDatabase)
updateFromToml mconfigPath (mtimeReader, destTZ) = do
  case mconfigPath of
    Nothing -> do
      configDir <- getXdgConfig "time-conv"
      let configPath = configDir </> "config.toml"
      exists <- doesFileExist configPath
      if exists
        then updateFromFile configPath
        else pure (mtimeReader, destTZ)
    Just configPath -> updateFromFile configPath
  where
    updateFromFile configPath = do
      contents <- readFileUtf8ThrowM configPath
      toml <- case TOML.decode @Toml contents of
        Left ex -> throwM ex
        Right toml -> pure toml

      case toml ^. #aliases of
        Nothing -> pure (mtimeReader, destTZ)
        Just aliases -> do
          let fromAliases' = fromAliases aliases
              destTZ' = over' (_Just % _TZDatabaseText) fromAliases' destTZ
              mtimeReader' =
                mtimeReader <&> \timeReader ->
                  MkTimeReader
                    { format = timeReader ^. #format,
                      -- translate aliases if they exist
                      srcTZ = over' (_Just % _TZDatabaseText) fromAliases' (timeReader ^. #srcTZ),
                      -- default to date specified by CLI; if not, try the
                      -- today flag from the toml file; otherwise, nothing
                      date = case timeReader ^. #date of
                        Just d -> Just d
                        Nothing -> case toml ^. #today of
                          Just True -> Just DateToday
                          _ -> Nothing,
                      timeString = timeReader ^. #timeString
                    }
          pure (mtimeReader', destTZ')

    fromAliases :: Map.Map Text Text -> Text -> Text
    fromAliases aliasesMap txt = fromMaybe txt (Map.lookup txt aliasesMap)

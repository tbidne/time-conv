-- | Internal library for testing.
--
-- @since 0.1
module TimeConv.Runner
  ( runTimeConv,
    runTimeConvHandler,
  )
where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Maybe.Optics (_Just)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion qualified as Conv
import Data.Time.Conversion.Types.Date (Date (..))
import Data.Time.Conversion.Types.Exception (SrcTZNoTimeStringException (..))
import Data.Time.Conversion.Types.TZDatabase (TZDatabase, _TZDatabaseText)
import Data.Time.Conversion.Types.TimeReader (TimeReader (..))
import Data.Time.Format qualified as Format
import Effects.Exception (MonadCatch, MonadThrow, throwM)
import Effects.FileSystem.FileReader (MonadFileReader, readFileUtf8ThrowM)
import Effects.FileSystem.Path ((</>))
import Effects.FileSystem.PathReader
  ( MonadPathReader (doesFileExist),
    getXdgConfig,
  )
import Effects.Optparse (MonadOptparse (execParser))
import Effects.System.Terminal (MonadTerminal)
import Effects.System.Terminal qualified as T
import Effects.Time (MonadTime)
import Optics.Core (over', (%), (%?), (^.))
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
  runWithArgs T.putTextLn args

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
  case (args ^. #srcTZ, args ^. #timeString) of
    (Just _, Nothing) -> throwM MkSrcTZNoTimeStringException
    _ -> pure ()

  -- Transform Args to TimeReader, DestTZ and FormatOut
  let (mtimeReader, destTZ, formatOut) = args ^. argsToBuilder
      formatStr = T.unpack $ formatOut ^. #unTimeFormat

  -- If the toml config exists, further transform TimeReader and DestTZ
  -- according to its config
  (mtimeReader', destTZ') <-
    if args ^. #noConfig
      then pure (mtimeReader, destTZ)
      else
        updateFromTomlFile
          (args ^. #config)
          mtimeReader
          (args ^. #noDate)
          destTZ

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

updateFromTomlFile ::
  ( MonadFileReader m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  -- | Path to toml config file.
  Maybe FilePath ->
  -- | TimeReader so far (CLI Args)
  Maybe TimeReader ->
  -- | CLI Args' noDate
  Bool ->
  -- | Dest TZ
  Maybe TZDatabase ->
  -- | Updated (TimeReader, DestTZ)
  m (Maybe TimeReader, Maybe TZDatabase)
updateFromTomlFile mconfigPath mtimeReader noDate mdestTZ = do
  case mconfigPath of
    Nothing -> do
      configDir <- getXdgConfig "time-conv"
      let configPath = configDir </> "config.toml"
      exists <- doesFileExist configPath
      if exists
        then updateFromFile configPath
        else pure (mtimeReader, mdestTZ)
    Just configPath -> updateFromFile configPath
  where
    updateFromFile configPath = do
      contents <- readFileUtf8ThrowM configPath
      case TOML.decode @Toml contents of
        Left ex -> throwM ex
        Right toml -> pure $ updateFromToml mtimeReader noDate mdestTZ toml

updateFromToml ::
  -- | TimeReader from args.
  Maybe TimeReader ->
  -- | noDate: If true, ignore toml's 'today' field.
  Bool ->
  -- | Dest TZ.
  Maybe TZDatabase ->
  -- | Toml.
  Toml ->
  -- | Updated TimeReader and Dest TZ.
  (Maybe TimeReader, Maybe TZDatabase)
updateFromToml mtimeReader noDate mdestTZ toml =
  let -- update timeReader's srcTZ and destTZ w/ aliases
      (mtimeReaderAliases, mdestTZAliases) =
        maybe
          (mtimeReader, mdestTZ)
          updateAliases
          (toml ^. #aliases)

      -- update timeReader's date w/ toml.today and noDate
      mTimeReaderAliasesDate =
        if noDate
          then mtimeReaderAliases
          else over' (_Just % #date) setIfNothingAndTomlToday mtimeReaderAliases
   in (mTimeReaderAliasesDate, mdestTZAliases)
  where
    -- update timeReader's srcTZ and destTZ w/ aliases
    updateAliases aliases =
      let fromAliases' = fromAliases aliases
          mdestTZ' = over' (_Just % _TZDatabaseText) fromAliases' mdestTZ
          mtimeReader' =
            over' (_Just % #srcTZ %? _TZDatabaseText) fromAliases' mtimeReader
       in (mtimeReader', mdestTZ')

    -- sets reader's date to today only if it is unspecified and set in
    -- toml
    setIfNothingAndTomlToday Nothing = case toml ^. #today of
      Just True -> Just DateToday
      _ -> Nothing
    setIfNothingAndTomlToday x = x

    fromAliases :: Map.Map Text Text -> Text -> Text
    fromAliases aliasesMap txt = fromMaybe txt (Map.lookup txt aliasesMap)

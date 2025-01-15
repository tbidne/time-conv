{-# LANGUAGE QuasiQuotes #-}

-- | Internal library for testing.
--
-- @since 0.1
module TimeConv.Runner
  ( runTimeConv,
  )
where

import Control.Monad.Catch (MonadCatch, MonadThrow, throwM)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion qualified as Conv
import Data.Time.Conversion.Types.Date (Date (DateToday))
import Data.Time.Conversion.Types.Exception
  ( DateNoTimeStringException (MkDateNoTimeStringException),
    SrcTZNoTimeStringException (MkSrcTZNoTimeStringException),
  )
import Data.Time.Conversion.Types.TZDatabase (TZDatabase, _TZDatabaseText)
import Data.Time.Conversion.Types.TimeReader (TimeReader)
import Data.Time.Format qualified as Format
import Effects.FileSystem.FileReader (MonadFileReader, readFileUtf8ThrowM)
import Effects.FileSystem.PathReader
  ( MonadPathReader (doesFileExist),
    OsPath,
    getXdgConfig,
  )
import Effects.Optparse (MonadOptparse (execParser))
import Effects.System.Terminal (MonadTerminal)
import Effects.System.Terminal qualified as T
import Effects.Time (MonadTime)
import FileSystem.OsPath (osp, (</>))
import Optics.Core
  ( A_Setter,
    Is,
    Optic',
    Prism,
    Prism',
    over',
    prism,
    set',
    (%),
    (%?),
    (^.),
  )
import Optics.Core.Extras (is)
import TOML qualified
import TimeConv.Runner.Args
  ( Args
      ( config,
        date,
        noConfig,
        noDate,
        srcTZ,
        timeString
      ),
    argsToBuilder,
    parserInfo,
  )
import TimeConv.Runner.Toml (Toml)

-- | Runs time-conv with CLI args.
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
  runWithArgs args

-- | Runs time-conv with given args.
--
-- @since 0.1
runWithArgs ::
  ( MonadCatch m,
    MonadFileReader m,
    MonadPathReader m,
    MonadTerminal m,
    MonadTime m
  ) =>
  Args ->
  m ()
runWithArgs args = do
  case args.timeString of
    Just _ -> pure ()
    Nothing -> do
      case args.srcTZ of
        Just _ -> throwM MkSrcTZNoTimeStringException
        Nothing -> pure ()
      case args.date of
        Just _ -> throwM MkDateNoTimeStringException
        Nothing -> pure ()

  -- Transform Args to TimeReader, DestTZ and FormatOut
  let (mtimeReader, destTZ, formatOut) = argsToBuilder args
      formatStr = T.unpack $ formatOut ^. #unTimeFormat

  -- If the toml config exists, further transform TimeReader and DestTZ
  -- according to its config
  (mtimeReader', destTZ') <-
    if args.noConfig
      then pure (mtimeReader, destTZ)
      else
        updateFromTomlFile
          args.config
          mtimeReader
          args.noDate
          destTZ

  readAndHandle mtimeReader' destTZ' formatStr
  where
    readAndHandle tr d fmt = do
      time <- Conv.readConvertTime tr d
      let result = T.pack $ Format.formatTime locale fmt time
      T.putTextLn result
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
  Maybe OsPath ->
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
      configDir <- getXdgConfig [osp|time-conv|]
      let configPath = configDir </> [osp|config.toml|]
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
updateFromToml mTimeReader noDate mDestTZ toml = (mFinalTimeReader, mFinalDest)
  where
    mFinalTimeReader :: Maybe TimeReader
    mFinalTimeReader = do
      timeReader <- mTimeReader

      let -- 1. Update src via aliases
          timeReaderAliases =
            updateAliases
              (#srcTZ %? _TZDatabaseText)
              timeReader

          -- 2. CLI date (date string or literal 'today') can be overridden
          --    in exactly one scenario:
          --
          --    1. noDate is False (--no-date unspecified).
          --    2. CLI --date unspecified.
          --    3. toml.today = true
          --
          -- maybeUpdateDate checks conditions 1 and 3.
          maybeUpdateDate :: Bool
          maybeUpdateDate = not noDate && is (#today %? _True) toml

          -- AffineTraversal here checks condition 2.
          timeReaderAliasesDate =
            if maybeUpdateDate
              then
                set'
                  (#date % _UnlawfulNothing)
                  DateToday
                  timeReaderAliases
              else
                timeReaderAliases

      Just timeReaderAliasesDate

    mFinalDest :: Maybe TZDatabase
    -- 1. Update via aliases
    mFinalDest = updateAliases _TZDatabaseText <$> mDestTZ

    -- if aliases exist, update relevant fields (timeReader's srcTZ and destTZ)
    updateAliases :: (Is k A_Setter) => Optic' k is s Text -> s -> s
    updateAliases k = over' k aliasOrDefault

    -- returns 'Map.lookup txt aliases' if aliases and the key both exist.
    aliasOrDefault :: Text -> Text
    aliasOrDefault txt = fromMaybe txt $ do
      aliasesMap <- toml ^. #aliases
      Map.lookup txt aliasesMap

-- | Matches Nothing but overrides with Just.
--
-- __WARNING:__ This is __unlawful__ since e.g. the law
--
-- @
--   preview l (review l b) ≡ Just b
-- @
--
-- does not hold here. To wit,
--
-- @
--   preview _UnlawfulNothing (review _UnlawfulNothing ()) ≡ Just ()
--   preview _UnlawfulNothing (Just ()) ≡ Just ()
--   Nothing ≢ Just ()
-- @
--
-- But this is exactly the behavior we want here, swapping constructors.
_UnlawfulNothing :: Prism (Maybe a) (Maybe a) () a
_UnlawfulNothing =
  prism
    Just
    ( \case
        Nothing -> Right ()
        other -> Left other
    )

_True :: Prism' Bool ()
_True =
  prism
    (const True)
    ( \case
        True -> Right ()
        other -> Left other
    )

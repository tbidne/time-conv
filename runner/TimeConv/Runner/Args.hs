{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | CLI args for TimeConv.
--
-- @since 0.1
module TimeConv.Runner.Args
  ( Args (..),
    argsToBuilder,
    parserInfo,
  )
where

import Data.Functor ((<&>))
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion.Types.Date (Date)
import Data.Time.Conversion.Types.Date qualified as Date
import Data.Time.Conversion.Types.TZDatabase (TZDatabase (TZDatabaseText))
import Data.Time.Conversion.Types.TimeFormat (TimeFormat)
import Data.Time.Conversion.Types.TimeFormat qualified as TimeFmt
import Data.Time.Conversion.Types.TimeReader
  ( TimeReader
      ( MkTimeReader,
        date,
        format,
        srcTZ,
        timeString
      ),
  )
import Data.Version (Version (versionBranch))
import Effects.Optparse (OsPath, osPath)
import Optics.Core (Getter, (^.))
import Optics.Core qualified as O
import Optics.TH (makeFieldLabelsNoPrefix)
import Options.Applicative
  ( Parser,
    ParserInfo
      ( ParserInfo,
        infoFailureCode,
        infoFooter,
        infoFullDesc,
        infoHeader,
        infoParser,
        infoPolicy,
        infoProgDesc
      ),
    (<**>),
  )
import Options.Applicative qualified as OApp
import Options.Applicative.Help (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse), ReadM)
import Paths_time_conv qualified as Paths

-- | CLI args.
--
-- @since 0.1
data Args = MkArgs
  { config :: Maybe OsPath,
    noConfig :: Bool,
    date :: Maybe Date,
    noDate :: Bool,
    destTZ :: Maybe TZDatabase,
    formatIn :: TimeFormat,
    formatOut :: Maybe TimeFormat,
    srcTZ :: Maybe TZDatabase,
    timeString :: Maybe Text
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Args

-- | Optparse-Applicative info.
--
-- @since 0.1
parserInfo :: ParserInfo Args
parserInfo =
  ParserInfo
    { infoParser = parseArgs,
      infoFullDesc = True,
      infoProgDesc = desc,
      infoHeader = Chunk header,
      infoFooter = Chunk footer,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    header = Just "time-conv: A tool for timezone conversions."
    footer = Just $ fromString versNum
    desc =
      Chunk.paragraph $
        "time-conv reads time strings and converts between timezones."
          <> " For the src and dest options, TZ_DB refers to labels like"
          <> " America/New_York. See https://en.wikipedia.org/wiki/Tz_database."

parseArgs :: Parser Args
parseArgs =
  MkArgs
    <$> parseConfig
    <*> parseNoConfig
    <*> parseDate
    <*> parseNoDate
    <*> parseDestTZ
    <*> parseFormatIn
    <*> parseFormatOut
    <*> parseSrcTZ
    <*> parseTimeStr
      <**> OApp.helper
      <**> version

-- | Maps 'Args' to 'TimeReader'. Details:
--
-- * If no 'Args.timeString' is given then no 'TimeReader' is returned.
-- * The output format is, in order:
--
--     1. 'formatOut' if it exists.
--     2. 'Args.format' if no 'formatOut' and 'Args.timeString' exists.
--     3. Otherwise, default format of "%H:%M".
--
-- @since 0.1
argsToBuilder :: Getter Args (Maybe TimeReader, Maybe TZDatabase, TimeFormat)
argsToBuilder = O.to to
  where
    to args =
      let mtimeReader = case args ^. #timeString of
            Just str ->
              Just $
                MkTimeReader
                  { format = args ^. #formatIn,
                    srcTZ = args ^. #srcTZ,
                    date =
                      if args ^. #noDate
                        then Nothing
                        else args ^. #date,
                    timeString = str
                  }
            Nothing -> Nothing
          formatOut = fromMaybe TimeFmt.rfc822 (args ^. #formatOut)
       in (mtimeReader, args ^. #destTZ, formatOut)

parseConfig :: Parser (Maybe OsPath)
parseConfig =
  OApp.optional $
    OApp.option
      osPath
      ( OApp.long "config"
          <> OApp.short 'c'
          <> OApp.metavar "PATH"
          <> mkHelp helpTxt
      )
  where
    helpTxt =
      mconcat
        [ "Path to TOML config file. It not given we automatically look in ",
          "the XDG config e.g. ~/.config/time-conv/config.toml."
        ]

parseNoConfig :: Parser Bool
parseNoConfig =
  OApp.switch
    ( mconcat
        [ OApp.long "no-config",
          OApp.hidden,
          mkHelp helpTxt
        ]
    )
  where
    helpTxt = "Disables --config."

parseDestTZ :: Parser (Maybe TZDatabase)
parseDestTZ =
  OApp.option
    readTZDatabase
    ( OApp.value Nothing
        <> OApp.long "dest-tz"
        <> OApp.short 'd'
        <> OApp.metavar "TZ_DB"
        <> mkHelp helpTxt
    )
  where
    helpTxt =
      mconcat
        [ "Timezone in which to convert the read string. Must be a tz database",
          " label like America/New_York. If none is given then we use the",
          " local system timezone."
        ]

readTZDatabase :: ReadM (Maybe TZDatabase)
readTZDatabase = Just . TZDatabaseText <$> OApp.str

parseFormatIn :: Parser TimeFormat
parseFormatIn =
  OApp.option
    (fromString <$> OApp.str)
    ( OApp.value TimeFmt.defaultTimeFormat
        <> OApp.long "format-in"
        <> OApp.short 'f'
        <> OApp.metavar "FMT_STR"
        <> mkHelp helpTxt
    )
  where
    helpTxt =
      mconcat
        [ "Glibc-style format string for parsing the time string. Should not ",
          "contain a timezone flag like %Z (see --src-tz) nor a date ",
          "(see --date). Defaults to ",
          defFormatStr,
          " i.e. 24-hr hour:minute. See 'man date' for basic examples."
        ]
    defFormatStr = T.unpack $ TimeFmt.defaultTimeFormat ^. #unTimeFormat

parseFormatOut :: Parser (Maybe TimeFormat)
parseFormatOut =
  OApp.optional $
    OApp.option
      readFormat
      ( OApp.long "format-out"
          <> OApp.short 'o'
          <> OApp.metavar "(rfc822 | FMT_STR)"
          <> mkHelp helpTxt
      )
  where
    helpTxt =
      mconcat
        [ "Like --format-in, but used for the output. If this is not ",
          "present we default to rfc822 i.e. RFC822."
        ]

readFormat :: ReadM TimeFormat
readFormat =
  OApp.str <&> \case
    "rfc822" -> TimeFmt.rfc822
    other -> fromString other

parseSrcTZ :: Parser (Maybe TZDatabase)
parseSrcTZ =
  OApp.option
    readTZDatabase
    ( OApp.value Nothing
        <> OApp.long "src-tz"
        <> OApp.short 's'
        <> OApp.metavar "TZ_DB"
        <> mkHelp helpTxt
    )
  where
    helpTxt =
      mconcat
        [ "Timezone in which to read the string. Must be a tz database",
          " label like America/New_York. If none is given then we use the",
          " local system timezone. This option requires TIME_STR."
        ]

parseDate :: Parser (Maybe Date)
parseDate =
  OApp.optional $
    OApp.option readDate $
      mconcat
        [ OApp.long "date",
          OApp.metavar "(today | YYYY-mm-dd)",
          mkHelp helpTxt
        ]
  where
    helpTxt =
      mconcat
        [ "Date in which to read the string. Today uses the current date, as ",
          "determined by the source.  This option requires TIME_STR."
        ]
    readDate = OApp.str >>= Date.parseDate

parseNoDate :: Parser Bool
parseNoDate =
  OApp.switch $
    mconcat
      [ OApp.long "no-date",
        OApp.hidden,
        mkHelp "Disables --date. Useful for disabling the toml field 'today'."
      ]

parseTimeStr :: Parser (Maybe Text)
parseTimeStr =
  OApp.optional $
    T.pack
      <$> OApp.argument
        OApp.str
        (OApp.metavar "TIME_STR" <> mkHelp helpTxt)
  where
    helpTxt =
      "Time string to parse. If none is given then we parse the"
        <> " local system time. To format the output, use --format-out."

version :: Parser (a -> a)
version = OApp.infoOption versNum (OApp.long "version" <> OApp.short 'v')

versNum :: String
versNum = "Version: " <> L.intercalate "." (show <$> versionBranch Paths.version)

mkHelp :: String -> OApp.Mod f a
mkHelp =
  OApp.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph

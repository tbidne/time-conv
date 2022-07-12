{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | CLI args for TimeConv.
--
-- @since 0.1
module TimeConv.Args
  ( Args (..),
    argsToBuilder,
    parserInfo,
  )
where

import Control.Applicative ((<|>))
import Data.Default (Default (..))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion.Types
  ( SrcTZ (..),
    TZDatabase (..),
    TimeFormat (..),
    TimeReader (..),
  )
import Data.Time.Conversion.Types qualified as Types
import Data.Time.Conversion.Utils qualified as Utils
import Data.Version.Package qualified as PV
import Development.GitRev qualified as GitRev
import Optics.Core (Getter, (%), (^.), (^?), _Just)
import Optics.Core qualified as O
import Optics.TH (makeFieldLabelsNoPrefix)
import Options.Applicative
  ( Parser,
    ParserInfo (..),
    (<**>),
  )
import Options.Applicative qualified as OApp
import Options.Applicative.Help (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..), ReadM)

-- | CLI args.
--
-- @since 0.1
data Args = MkArgs
  { format :: TimeFormat,
    formatOut :: Maybe TimeFormat,
    srcTZ :: Maybe SrcTZ,
    destTZ :: Maybe TZDatabase,
    today :: Bool,
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
      infoProgDesc = Chunk desc,
      infoHeader = Chunk header,
      infoFooter = Chunk footer,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    header = Just "time-conv: A tool for timezone conversions."
    footer = Just $ fromString $ T.unpack versNum
    desc =
      Just $
        "\ntime-conv reads time strings and converts between timezones."
          <> " For the src and dest options, TZ_DATABASE refers to labels like"
          <> " America/New_York. See https://en.wikipedia.org/wiki/Tz_database."

parseArgs :: Parser Args
parseArgs =
  MkArgs
    <$> parseFormat
    <*> parseFormatOut
    <*> parseSrcTZ
    <*> parseDestTZ
    <*> parseToday
    <*> parseTimeStr
    <**> OApp.helper
    <**> version

-- | Maps 'Args' to 'TimeReader'. Details:
--
-- * If no 'Args.timeString' is given then no 'TimeReader' is returned.
-- * If a 'TimeReader' is returned then its locale is always
--   'Utils.timeLocaleAllZones'.
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
                  { format = args ^. #format,
                    srcTZ = args ^. #srcTZ,
                    locale = Utils.timeLocaleAllZones,
                    today = args ^. #today,
                    timeString = str
                  }
            Nothing -> Nothing
          formatOut = fromMaybe def (args ^. #formatOut <|> mtimeReader ^? _Just % #format)
       in (mtimeReader, args ^. #destTZ, formatOut)

parseDestTZ :: Parser (Maybe TZDatabase)
parseDestTZ =
  OApp.option
    readTZDatabase
    ( OApp.value Nothing
        <> OApp.long "dest-tz"
        <> OApp.short 'd'
        <> OApp.metavar "TZ_DATABASE"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      mconcat
        [ "Timezone in which to convert the read string. Must be a tz database",
          " label like America/New_York. If none is given then we use the",
          " local system timezone."
        ]
    readTZDatabase = Just . TZDatabaseText <$> OApp.str

parseFormat :: Parser TimeFormat
parseFormat =
  OApp.option
    readFormat
    ( OApp.value def
        <> OApp.long "format"
        <> OApp.short 'f'
        <> OApp.metavar "<rfc822 | FORMAT_STRING>"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      mconcat
        [ "Glibc-style format string e.g. %Y-%m-%d for yyyy-mm-dd, only used if",
          " a time string is given. Defaults to ",
          defFormatStr,
          " i.e. 24-hr hour:minute. If the string 'rfc822' is given then we use",
          " RFC822. See 'man date' for basic examples, and ",
          " https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Format.html#v:formatTime for the exact spec."
        ]
    defFormatStr = T.unpack $ def @TimeFormat ^. #unTimeFormat

parseFormatOut :: Parser (Maybe TimeFormat)
parseFormatOut =
  OApp.optional $
    OApp.option
      readFormat
      ( OApp.long "format-out"
          <> OApp.short 'o'
          <> OApp.metavar "<rfc822 | FORMAT_STRING>"
          <> OApp.help helpTxt
      )
  where
    helpTxt =
      mconcat
        [ "Like --format, but used for the output only. If this is not",
          " present but a time string is, then --format is used for both",
          " input and output. In other words, this option must be used",
          " if you want to format the local system time output."
        ]

readFormat :: ReadM TimeFormat
readFormat =
  OApp.str <&> \case
    "rfc822" -> Types.rfc822
    other -> fromString other

parseSrcTZ :: Parser (Maybe SrcTZ)
parseSrcTZ =
  OApp.option
    readSrcTZ
    ( OApp.value Nothing
        <> OApp.long "src-tz"
        <> OApp.short 's'
        <> OApp.metavar "<literal | TZ_DATABASE>"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      mconcat
        [ "Timezone in which to read the string. Can be 'literal' or",
          " a tz database label. If none is given then we use the local",
          " system timezone. The literal option means we read the",
          " (possibly empty) timezone from the string itself e.g.",
          " '7:00 EST'. If a timezone is included, then a formatter using the",
          " '%Z' flag should be present. If literal is specified and no ",
          " timezone is included then we assume UTC."
        ]
    readSrcTZ =
      OApp.str <&> \case
        "literal" -> Just SrcTZLiteral
        other -> Just $ SrcTZDatabase $ TZDatabaseText other

parseToday :: Parser Bool
parseToday =
  OApp.switch $
    mconcat
      [ OApp.long "today",
        OApp.short 't',
        OApp.help helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "Used when reading a time string, adds the local date. This is a",
          " convenience option and should only be used if the time string",
          " and format do not explicitly mention date."
        ]

parseTimeStr :: Parser (Maybe Text)
parseTimeStr =
  OApp.optional $
    T.pack
      <$> OApp.argument
        OApp.str
        (OApp.metavar "TIME_STRING" <> OApp.help helpTxt)
  where
    helpTxt =
      "Time string to parse. If none is given then we parse the"
        <> " local system time. To format the output, use --format-out."

version :: Parser (a -> a)
version = OApp.infoOption txt (OApp.long "version" <> OApp.short 'v')
  where
    txt =
      T.unpack $
        T.unlines
          [ "time-conv",
            versNum,
            "Revision:" <> $(GitRev.gitHash),
            $(GitRev.gitCommitDate)
          ]

versNum :: Text
versNum = "Version: " <> $$(PV.packageVersionTextTH "time-conv.cabal")

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

import Data.Default (Default (..))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion.Types
  ( TZDatabase (..),
    TimeFormat (..),
    TimeReader (..),
  )
import Data.Time.Conversion.Types qualified as Types
import Data.Version.Package qualified as PV
import Development.GitRev qualified as GitRev
import Optics.Core (Getter, (^.))
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
  { formatIn :: TimeFormat,
    formatOut :: Maybe TimeFormat,
    srcTZ :: Maybe TZDatabase,
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
    <$> parseFormatIn
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
                    today = args ^. #today,
                    timeString = str
                  }
            Nothing -> Nothing
          formatOut = fromMaybe Types.rfc822 (args ^. #formatOut)
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

readTZDatabase :: ReadM (Maybe TZDatabase)
readTZDatabase = Just . TZDatabaseText <$> OApp.str

parseFormatIn :: Parser TimeFormat
parseFormatIn =
  OApp.option
    (fromString <$> OApp.str)
    ( OApp.value def
        <> OApp.long "format-in"
        <> OApp.short 'f'
        <> OApp.metavar "FORMAT_STRING"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      mconcat
        [ "Glibc-style format string -- e.g. %Y-%m-%d for yyyy-mm-dd -- for ",
          "parsing the time string. Should not contain a timezone flag like ",
          "%Z, see --src-tz instead. Defaults to ",
          defFormatStr,
          " i.e. 24-hr hour:minute. See 'man date' for basic examples, and ",
          "https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Format.html#v:formatTime for the exact spec."
        ]
    defFormatStr = T.unpack $ def @TimeFormat ^. #unTimeFormat

parseFormatOut :: Parser (Maybe TimeFormat)
parseFormatOut =
  OApp.optional $
    OApp.option
      readFormat
      ( OApp.long "format-out"
          <> OApp.short 'o'
          <> OApp.metavar "(rfc822 | FORMAT_STRING)"
          <> OApp.help helpTxt
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
    "rfc822" -> Types.rfc822
    other -> fromString other

parseSrcTZ :: Parser (Maybe TZDatabase)
parseSrcTZ =
  OApp.option
    readTZDatabase
    ( OApp.value Nothing
        <> OApp.long "src-tz"
        <> OApp.short 's'
        <> OApp.metavar "TZ_DATABASE"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      mconcat
        [ "Timezone in which to read the string. Must be a tz database",
          " label like America/New_York. If none is given then we use the",
          " local system timezone."
        ]

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

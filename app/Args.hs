{-# LANGUAGE TemplateHaskell #-}

module Args
  ( parserInfo,
    Args (..),
  )
where

import Data.Default (Default (..))
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion.Types
  ( SrcTZ (..),
    TZConv (..),
    TZDatabase (..),
    TimeBuilder (..),
    TimeFormat (..),
  )
import Data.Time.Conversion.Utils qualified as Utils
import Data.Version.Package qualified as PV
import Development.GitRev qualified as GitRev
import Options.Applicative
  ( Parser,
    ParserInfo (..),
    (<**>),
  )
import Options.Applicative qualified as OApp
import Options.Applicative.Help (Chunk (..))
import Options.Applicative.Types (ArgPolicy (..))

data Args = MkArgs
  { builder :: !TimeBuilder,
    timeString :: !Text
  }

-- | Optparse-Applicative info.
parserInfo :: ParserInfo Args
parserInfo =
  ParserInfo
    { infoParser = argParser,
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
          <> " For the src and dest options, tz_database refers to labels like"
          <> " America/New_York. See https://en.wikipedia.org/wiki/Tz_database."

argParser :: Parser Args
argParser =
  MkArgs
    <$> parseBuilder
    <*> parseTimeStr
    <**> OApp.helper
    <**> version

parseBuilder :: Parser TimeBuilder
parseBuilder =
  MkTimeBuilder
    <$> parseFormat
    <*> parseSrcTZ
    <*> parseDestTZ
    <*> pure Utils.timeLocaleAllZones

parseDestTZ :: Parser TZConv
parseDestTZ =
  OApp.option
    readTZConv
    ( OApp.value TZConvLocal
        <> OApp.long "dest-tz"
        <> OApp.short 'd'
        <> OApp.metavar "<local | tz_database>"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      "Timezone in which to convert the read string. Can be 'local' or"
        <> " a tz database label. Defaults to local."
    readTZConv = do
      a <- OApp.str
      case a of
        "local" -> pure TZConvLocal
        other -> pure $ TZConvDatabase $ TZDatabaseText other

parseFormat :: Parser TimeFormat
parseFormat =
  OApp.option
    OApp.str
    ( OApp.value def
        <> OApp.long "format"
        <> OApp.short 'f'
        <> OApp.metavar "STR"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      "Glibc-style format string e.g. %Y-%m-%d for yyyy-mm-dd. Defaults to "
        <> defFormatStr
        <> "i.e. 24-hr hour:minute. See 'man date' for basic examples, and "
        <> " https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Format.html#v:formatTime for the exact spec."
    defFormatStr = T.unpack $ unTimeFormat def

parseSrcTZ :: Parser SrcTZ
parseSrcTZ = do
  OApp.option
    readSrcTZ
    ( OApp.value (SrcTZConv TZConvLocal)
        <> OApp.long "src-tz"
        <> OApp.short 's'
        <> OApp.metavar "<local | literal | tz_database>"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      "Timezone in which to read the string. Can be 'local', 'literal' or"
        <> " a tz database label. Defaults to local. The literal option means we"
        <> " read the (possibly empty) timezone from the string itself e.g."
        <> " '7:00 EST'. If a timezone is included, then a formatter using the"
        <> " '%Z' flag should be present. If literal is specified and no "
        <> " timezone is included then we assume UTC."
    readSrcTZ = do
      a <- OApp.str
      case a of
        "local" -> pure $ SrcTZConv TZConvLocal
        "literal" -> pure SrcTZLiteral
        other -> pure $ SrcTZConv $ TZConvDatabase $ TZDatabaseText other

parseTimeStr :: Parser Text
parseTimeStr = T.pack <$> OApp.argument OApp.str (OApp.metavar "STRING")

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

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Args
  ( Args (..),
    argsToBuilder,
    parserInfo,
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
import Data.Time.Conversion.Types qualified as Types
import Data.Time.Conversion.Utils qualified as Utils
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

-- Very nearly identical to TimeBuilder. The reason we manually reimplement
-- this type rather than just use reuse TimeBuilder is so we can control the
-- order of the arguments in the help page (order depends on argument order
-- in the below definition).
data Args = MkArgs
  { format :: TimeFormat,
    formatOut :: Maybe TimeFormat,
    srcTZ :: SrcTZ,
    destTZ :: TZConv,
    timeString :: Maybe Text
  }
  deriving stock (Eq, Show)

makeFieldLabelsNoPrefix ''Args

-- | Optparse-Applicative info.
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
          <> " For the src and dest options, tz_database refers to labels like"
          <> " America/New_York. See https://en.wikipedia.org/wiki/Tz_database."

parseArgs :: Parser Args
parseArgs =
  MkArgs
    <$> parseFormat
    <*> parseFormatOut
    <*> parseSrcTZ
    <*> parseDestTZ
    <*> parseTimeStr
    <**> OApp.helper
    <**> version

argsToBuilder :: Getter Args TimeBuilder
argsToBuilder = O.to to
  where
    to args =
      MkTimeBuilder
        { format = args ^. #format,
          srcTZ = args ^. #srcTZ,
          destTZ = args ^. #destTZ,
          locale = Utils.timeLocaleAllZones,
          timeString = args ^. #timeString
        }

parseBuilder :: Parser TimeBuilder
parseBuilder =
  MkTimeBuilder
    <$> parseFormat
    <*> parseSrcTZ
    <*> parseDestTZ
    <*> pure Utils.timeLocaleAllZones
    <*> parseTimeStr

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
    readFormat
    ( OApp.value def
        <> OApp.long "format"
        <> OApp.short 'f'
        <> OApp.metavar "<full | STRING>"
        <> OApp.help helpTxt
    )
  where
    helpTxt =
      "Glibc-style format string e.g. %Y-%m-%d for yyyy-mm-dd. Defaults to "
        <> defFormatStr
        <> "i.e. 24-hr hour:minute. If the string 'full' is given then we use"
        <> " RFC822. See 'man date' for basic examples, and "
        <> " https://hackage.haskell.org/package/time-1.13/docs/Data-Time-Format.html#v:formatTime for the exact spec."
    defFormatStr = def ^. Types.timeFormatStringIso

parseFormatOut :: Parser (Maybe TimeFormat)
parseFormatOut =
  OApp.optional $
    OApp.option
      readFormat
      ( OApp.long "format-out"
          <> OApp.short 'o'
          <> OApp.metavar "<full | STRING>"
          <> OApp.help helpTxt
      )
  where
    helpTxt =
      "Like --format, but used for the output. If this is not"
        <> " present then --format is used for both input and output."

readFormat :: ReadM TimeFormat
readFormat = do
  s <- OApp.str
  pure $ case s of
    "full" -> TimeFormatFull
    other -> TimeFormatManual (T.pack other)

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

parseTimeStr :: Parser (Maybe Text)
parseTimeStr =
  OApp.optional $
    T.pack
      <$> OApp.argument
        OApp.str
        (OApp.metavar "STRING" <> OApp.help helpTxt)
  where
    helpTxt =
      "Time string to parse. If none is given then we parse the"
        <> " local system time."

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

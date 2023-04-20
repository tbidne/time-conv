module Main (main) where

import System.Environment.Guard (ExpectEnv (..), guardOrElse')
import Test.DocTest qualified as DocTest

main :: IO ()
main =
  guardOrElse'
    "RUN_DOCTEST"
    ExpectEnvSet
    (DocTest.doctest args)
    (putStrLn "*** Doctests Disabled. Set RUN_DOCTEST=1 to run ***")
  where
    args = files <> exts

files :: [String]
files =
  [ "-isrc",
    "src/Data/Time/Conversion/Types/Date.hs",
    "src/Data/Time/Conversion/Types/Date/Internal.hs",
    "src/Data/Time/Conversion/Types/Exception.hs",
    "src/Data/Time/Conversion/Types/TimeFormat.hs",
    "src/Data/Time/Conversion/Types/TimeReader.hs",
    "src/Data/Time/Conversion/Types/TZDatabase.hs",
    "src/Data/Time/Conversion.hs"
  ]

exts :: [String]
exts =
  [ "-XNoStarIsType",
    "-XApplicativeDo",
    "-XDataKinds",
    "-XDeriveAnyClass",
    "-XDeriveGeneric",
    "-XDerivingVia",
    "-XDuplicateRecordFields",
    "-XFlexibleContexts",
    "-XFlexibleInstances",
    "-XGADTs",
    "-XImportQualifiedPost",
    "-XInstanceSigs",
    "-XLambdaCase",
    "-XMultiParamTypeClasses",
    "-XMultiWayIf",
    "-XNamedFieldPuns",
    "-XNumericUnderscores",
    "-XOverloadedLabels",
    "-XOverloadedStrings",
    "-XPatternSynonyms",
    "-XScopedTypeVariables",
    "-XStandaloneDeriving",
    "-XStandaloneKindSignatures",
    "-XTupleSections",
    "-XTypeApplications",
    "-XTypeFamilies"
  ]

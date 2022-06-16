module Main (main) where

import System.Environment.Guard (guardSet_)
import Test.DocTest qualified as DocTest

main :: IO ()
main = guardSet_ "RUN_DOCTEST" $ DocTest.doctest args
  where
    args = files <> exts

files :: [String]
files =
  [ "-isrc",
    "src/Data/Time/Conversion/Types.hs",
    "src/Data/Time/Conversion.hs"
  ]

-- This is needed because DocTest does not read the cabal
-- file's default-extensions
exts :: [String]
exts =
  [ "-XNoStarIsType",
    "-XApplicativeDo",
    "-XDataKinds",
    "-XDeriveGeneric",
    "-XDerivingVia",
    "-XDuplicateRecordFields",
    "-XFlexibleContexts",
    "-XFlexibleInstances",
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
    "-XStandaloneKindSignatures",
    "-XTupleSections",
    "-XTypeApplications",
    "-XTypeFamilies"
  ]

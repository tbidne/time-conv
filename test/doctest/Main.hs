module Main (main) where

import System.Environment.Guard (GuardExpectation (..), guardOrElse_)
import Test.DocTest qualified as DocTest

main :: IO ()
main =
  guardOrElse_
    "RUN_DOCTEST"
    GuardExpectationSet
    (DocTest.doctest args)
    (putStrLn "*** Doctests Disabled ***")
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

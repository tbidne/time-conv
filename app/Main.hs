-- | Main module.
--
-- @since 0.1
module Main (main) where

import Control.Exception (displayException)
import Effectful (runEff)
import Effectful.Exception (catch, throwM)
import Effectful.FileSystem.FileReader.Static (runFileReaderStaticIO)
import Effectful.FileSystem.PathReader.Static (runPathReaderStaticIO)
import Effectful.Optparse.Static (runOptparseStaticIO)
import Effectful.Terminal.Static (runTerminalStaticIO)
import Effectful.Time.Dynamic (runTimeDynamicIO)
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import System.Exit (ExitCode (..))
import TimeConv.Runner (runTimeConv)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)

  run runTimeConv `catch` doNothingOnSuccess
  where
    run =
      runEff
        . runFileReaderStaticIO
        . runOptparseStaticIO
        . runPathReaderStaticIO
        . runTerminalStaticIO
        . runTimeDynamicIO

    doNothingOnSuccess ExitSuccess = pure ()
    doNothingOnSuccess ex@(ExitFailure _) = throwM ex

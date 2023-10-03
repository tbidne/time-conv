-- | Main module.
--
-- @since 0.1
module Main (main) where

import Control.Exception (displayException)
import Effects.Exception (catchCS, throwM)
import GHC.Conc.Sync (setUncaughtExceptionHandler)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import TimeConv.Runner (runTimeConv)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  setUncaughtExceptionHandler (putStrLn . displayException)

  runTimeConv `catchCS` doNothingOnSuccess
  where
    doNothingOnSuccess ExitSuccess = pure ()
    doNothingOnSuccess ex@(ExitFailure _) = throwM ex

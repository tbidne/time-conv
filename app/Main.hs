-- | Main module.
--
-- @since 0.1
module Main (main) where

import Effects.Exception (catchCS, throwM)
import System.Exit (ExitCode (..))
import TimeConv.Runner (runTimeConv)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = runTimeConv `catchCS` doNothingOnSuccess
  where
    doNothingOnSuccess ExitSuccess = pure ()
    doNothingOnSuccess ex@(ExitFailure _) = throwM ex

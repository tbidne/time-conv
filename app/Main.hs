-- | Main module.
--
-- @since 0.1
module Main (main) where

import Args (Args (..), parserInfo)
import Control.Exception (Exception (..), throwIO)
import Control.Exception.Base (SomeAsyncException (..), SomeException, catch)
import Data.Time.Conversion qualified as Conv
import Options.Applicative qualified as OApp

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  args <- OApp.execParser parserInfo
  runConv args `safeCatch` \e ->
    putStrLn $ displayException e

runConv :: Args -> IO ()
runConv MkArgs {builder, timeString} = do
  time <- Conv.readConvertTime builder timeString
  print time

safeCatch :: IO a -> (SomeException -> IO a) -> IO a
safeCatch io handler =
  io `catch` \ex ->
    case fromException (toException ex) of
      Just (SomeAsyncException _) -> throwIO ex
      Nothing -> handler ex

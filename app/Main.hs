-- | Main module.
--
-- @since 0.1
module Main (main) where

import Control.Exception.Annotation.Utils (ExceptionProxy (MkExceptionProxy))
import Control.Exception.Annotation.Utils qualified as AnnUtils
import Data.Proxy (Proxy (Proxy))
import Data.Time.Conversion.Types.Exception
  ( DateNoTimeStringException,
    LocalSystemTimeException,
    LocalTimeZoneException,
    ParseTZDatabaseException,
    ParseTimeException,
    SrcTZNoTimeStringException,
  )
import TimeConv.Runner qualified as Runner

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  AnnUtils.setUncaughtExceptionDisplayInnerMatch
    proxies
    putStrLn

  Runner.runTimeConvIO
  where
    proxies =
      [ MkExceptionProxy $ Proxy @DateNoTimeStringException,
        MkExceptionProxy $ Proxy @LocalSystemTimeException,
        MkExceptionProxy $ Proxy @LocalTimeZoneException,
        MkExceptionProxy $ Proxy @ParseTimeException,
        MkExceptionProxy $ Proxy @ParseTZDatabaseException,
        MkExceptionProxy $ Proxy @SrcTZNoTimeStringException
      ]

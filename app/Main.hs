{-# LANGUAGE CPP #-}

-- | Main module.
--
-- @since 0.1
module Main (main) where

import Data.Proxy (Proxy (Proxy))
import Data.Time.Conversion.Types.Exception
  ( DateNoTimeStringException,
    LocalSystemTimeException,
    LocalTimeZoneException,
    ParseTZDatabaseException,
    ParseTimeException,
    SrcTZNoTimeStringException,
  )
import Effects.Exception (ExceptionProxy (MkExceptionProxy))
import Effects.Exception qualified as Ex
import TimeConv.Runner (runTimeConv)

{- ORMOLU_DISABLE -}

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  setFn
    proxies
    putStrLn

  runTimeConv
  where
    proxies =
      [ MkExceptionProxy $ Proxy @DateNoTimeStringException,
        MkExceptionProxy $ Proxy @LocalSystemTimeException,
        MkExceptionProxy $ Proxy @LocalTimeZoneException,
        MkExceptionProxy $ Proxy @ParseTimeException,
        MkExceptionProxy $ Proxy @ParseTZDatabaseException,
        MkExceptionProxy $ Proxy @SrcTZNoTimeStringException
      ]
    setFn =
#if MIN_VERSION_base(4, 20, 0)
      Ex.setUncaughtExceptionDisplayInnerMatch
#else
      Ex.setUncaughtExceptionDisplayCSNoMatch
#endif

{- ORMOLU_ENABLE -}
